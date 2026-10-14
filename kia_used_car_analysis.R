# =====================================================================
# BAN 7010 - Team Project (Team Gyoza)
# Company: KIA  |  US used-car market analysis (auction data Dec-2014 to Jul-2015)
# =====================================================================
# Inputs (put in the working directory):
#   car_prices.csv                 - course dataset
#   bea_sa51.csv                   - BEA SA51: per-capita disposable personal income by state, 1948-2015
#   state_year.rda                 - BLS LAUS annual state unemployment (jjchern/laus on GitHub)
#   2014_usa_states.csv            - Census 2014 state population (plotly/datasets on GitHub)
#   elec.csv                       - county presidential results 2008-16 (tonmcg on GitHub)
# Outputs: figures in ./fig, tables in ./out
# =====================================================================

suppressMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(lubridate); library(scales); library(broom); library(forecast)
  library(sandwich); library(lmtest)
})
dir.create("fig", showWarnings = FALSE); dir.create("out", showWarnings = FALSE)
OUR <- "Kia"
KIA_BLUE <- "#2a78d6"; GREY <- "#b5b3ab"
theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold"),
                plot.title.position = "plot"))
save_fig <- function(p, name, w = 8, h = 5) ggsave(file.path("fig", name), p, width = w, height = h, dpi = 150, bg = "white")
log_line <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = "out/key_numbers.txt", append = TRUE) }
if (file.exists("out/key_numbers.txt")) file.remove("out/key_numbers.txt")

# ---------------------------------------------------------------------
# STEP 1 & 4. LOAD + CLEAN
# ---------------------------------------------------------------------
raw <- read_csv("car_prices.csv", show_col_types = FALSE)
log_line("Raw rows: ", nrow(raw))

make_fix <- c("chev truck" = "chevrolet", "ford tk" = "ford", "ford truck" = "ford",
              "dodge tk" = "dodge", "gmc truck" = "gmc", "hyundai tk" = "hyundai",
              "mazda tk" = "mazda", "mercedes" = "mercedes-benz", "mercedes-b" = "mercedes-benz",
              "vw" = "volkswagen", "landrover" = "land rover")

us_states <- c(state.abb, "DC") |> tolower()

cars <- raw |>
  mutate(make = tolower(trimws(make)),
         make = ifelse(make %in% names(make_fix), make_fix[make], make),
         model = tolower(trimws(model)),
         body = tolower(trimws(body)),
         state = tolower(state)) |>
  filter(!is.na(make), !is.na(sellingprice), !is.na(mmr), !is.na(saledate),
         state %in% us_states,                     # drops Canada, Puerto Rico, corrupted rows
         sellingprice >= 100, odometer < 999999, !is.na(odometer)) |>
  mutate(
    Make = tools::toTitleCase(make),
    Make = recode(Make, "Bmw" = "BMW", "Gmc" = "GMC", "Ram" = "Ram", "Mercedes-Benz" = "Mercedes-Benz"),
    # condition is recorded on two scales (1-5 and 10-49): put everything on 1.0-5.0
    condition = ifelse(condition > 5, condition / 10, condition),
    sale_dt  = parse_date_time(substr(saledate, 5, 15), orders = "b d Y", quiet = TRUE),
    sale_date = as.Date(sale_dt),
    sale_year = year(sale_date),
    age = pmax(sale_year - year, 0),
    price_to_mmr = sellingprice / mmr,
    body_grp = case_when(
      grepl("suv", body) ~ "SUV",
      grepl("cab|supercrew", body) ~ "Pickup",
      grepl("minivan|van", body) ~ "Van/Minivan",
      grepl("sedan", body) ~ "Sedan",
      grepl("coupe|koup", body) ~ "Coupe",
      grepl("convertible", body) ~ "Convertible",
      grepl("hatchback", body) ~ "Hatchback",
      grepl("wagon", body) ~ "Wagon",
      TRUE ~ "Unknown"),
    seller_type = case_when(
      grepl("hertz|avis|enterprise|budget|rental|dtg operations|u-haul|ars/", seller) ~ "Rental fleet",
      grepl("financ|credit|bank|capital|lease|leasing|acceptance|santander|ally|remarketing|ahfc|tdaf|wheels|world omni", seller) ~ "Bank / captive finance / lease",
      grepl("motors america|north america|na rep|dealer program", seller) ~ "Manufacturer",
      TRUE ~ "Dealer / other"),
    origin = case_when(
      make %in% c("ford","chevrolet","dodge","chrysler","jeep","gmc","cadillac","lincoln","buick","ram",
                  "pontiac","mercury","saturn","hummer","oldsmobile","plymouth","tesla","fisker","geo","isuzu_us") ~ "USA",
      make %in% c("toyota","honda","nissan","infiniti","lexus","acura","mazda","subaru","mitsubishi","scion","suzuki","isuzu") ~ "Japan",
      make %in% c("hyundai","kia","daewoo") ~ "South Korea",
      make %in% c("bmw","mercedes-benz","volkswagen","audi","porsche","mini","smart") ~ "Germany",
      TRUE ~ "Other Europe"),
    luxury = make %in% c("bmw","mercedes-benz","lexus","infiniti","acura","audi","cadillac","lincoln","porsche",
                         "land rover","jaguar","volvo","maserati","bentley","ferrari","lamborghini","rolls-royce",
                         "aston martin","tesla","lotus","fisker"),
    is_ours = Make == OUR
  ) |>
  filter(!is.na(sale_date), sale_date >= as.Date("2014-12-01")) |>   # ~200 rows carry stray 2014-Jan dates
  mutate(body_grp = relevel(factor(body_grp), ref = "Sedan"))

log_line("Clean US rows: ", nrow(cars), "  (", percent(nrow(cars)/nrow(raw), 0.1), " of raw)")
log_line("Sale dates: ", format(min(cars$sale_date)), " to ", format(max(cars$sale_date)))
log_line("States in clean data: ", n_distinct(cars$state))

# ---------------------------------------------------------------------
# STEP 3. ENVIRONMENTAL DATA (BEA income, BLS unemployment, Census population, politics, region)
# ---------------------------------------------------------------------
st_lookup <- tibble(state_name = c(state.name, "District of Columbia"),
                    state = tolower(c(state.abb, "DC")),
                    region = c(as.character(state.region), "South"))
st_lookup$region <- recode(st_lookup$region, "North Central" = "Midwest")

bea <- read_csv("bea_sa51.csv", skip = 4, col_types = cols(.default = "c"), n_max = 60) |>
  filter(!is.na(GeoName)) |>
  mutate(GeoName = gsub("\\s*\\*.*$", "", GeoName)) |>
  pivot_longer(-c(GeoFips, GeoName), names_to = "year", values_to = "dpi_pc") |>
  mutate(year = as.integer(year), dpi_pc = suppressWarnings(as.numeric(dpi_pc))) |>
  filter(!is.na(dpi_pc))

dpi <- bea |> filter(year %in% c(2010, 2014)) |>
  pivot_wider(names_from = year, values_from = dpi_pc, names_prefix = "y") |>
  transmute(state_name = GeoName, dpi_pc_2014 = y2014,
            dpi_growth_10_14 = (y2014 / y2010)^(1/4) - 1)

load("state_year.rda")   # object: state_year
unemp <- state_year |> filter(year == 2014) |>
  transmute(state_name = as.character(state), unemp_2014 = unem_rate)

pop <- read_csv("2014_usa_states.csv", show_col_types = FALSE) |>
  transmute(state = tolower(Postal), pop_2014 = Population)

elec <- read_csv("elec.csv", show_col_types = FALSE) |>
  mutate(fips = sprintf("%05d", as.integer(fips_code)), st_fips = substr(fips, 1, 2)) |>
  group_by(st_fips) |>
  summarise(dem_share_2012 = sum(dem_2012) / sum(total_2012))
fips_map <- state_year |> distinct(fips, state) |> transmute(st_fips = fips, state_name = as.character(state))
elec <- elec |> inner_join(fips_map, by = "st_fips") |> select(state_name, dem_share_2012)

env <- st_lookup |>
  left_join(dpi, by = "state_name") |> left_join(unemp, by = "state_name") |>
  left_join(pop, by = "state") |> left_join(elec, by = "state_name")
write_csv(env, "out/state_environment.csv")

# ---------------------------------------------------------------------
# Q2. INDUSTRY STRUCTURE
# ---------------------------------------------------------------------
n_all <- nrow(cars); rev_all <- sum(cars$sellingprice)
brand <- cars |> group_by(Make, origin, luxury) |>
  summarise(units = n(), revenue = sum(sellingprice), avg_price = mean(sellingprice),
            med_price = median(sellingprice), avg_age = mean(age), avg_miles = mean(odometer),
            avg_cond = mean(condition, na.rm = TRUE), price_to_mmr = mean(price_to_mmr, na.rm = TRUE),
            suv_share = mean(body_grp == "SUV"), sedan_share = mean(body_grp == "Sedan"),
            pickup_share = mean(body_grp == "Pickup"), .groups = "drop") |>
  mutate(unit_share = units / n_all, rev_share = revenue / rev_all) |>
  arrange(desc(units)) |> mutate(rank = row_number())
write_csv(brand, "out/brand_summary.csv")

hhi <- sum((brand$unit_share * 100)^2)
cr4 <- sum(head(brand$unit_share, 4))
log_line("Makes: ", nrow(brand), " | HHI (units): ", round(hhi), " | CR4: ", percent(cr4, 0.1),
         " | CR8: ", percent(sum(head(brand$unit_share, 8)), 0.1))

origin_tbl <- cars |> count(origin) |> mutate(share = n / sum(n)) |> arrange(desc(n))
write_csv(origin_tbl, "out/origin_share.csv"); print(origin_tbl)
body_tbl <- cars |> count(body_grp) |> mutate(share = n / sum(n)) |> arrange(desc(n))
write_csv(body_tbl, "out/body_share.csv"); print(body_tbl)
seller_tbl <- cars |> group_by(seller_type) |>
  summarise(units = n(), avg_price = mean(sellingprice), avg_age = mean(age), .groups = "drop") |>
  mutate(share = units / sum(units)) |> arrange(desc(units))
write_csv(seller_tbl, "out/seller_share.csv"); print(seller_tbl)
log_line("Luxury share of units: ", percent(mean(cars$luxury), 0.1),
         " | of revenue: ", percent(sum(cars$sellingprice[cars$luxury]) / rev_all, 0.1))
log_line("Median price all: ", dollar(median(cars$sellingprice)), " | median age: ", median(cars$age),
         " | median miles: ", comma(median(cars$odometer)))

top15 <- brand |> slice_head(n = 15) |>
  mutate(Make = reorder(Make, units), ours = Make == OUR)
p <- ggplot(top15, aes(units, Make, fill = ours)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(unit_share, 0.1)), hjust = -0.15, size = 3.3, colour = "#52514e") +
  scale_fill_manual(values = c(`TRUE` = KIA_BLUE, `FALSE` = GREY), guide = "none") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .12))) +
  labs(title = "Ford, Chevrolet, Nissan and Toyota account for half of auction volume",
       subtitle = "Top 15 makes by units sold, US auctions Dec 2014 - Jul 2015 (labels = unit share)",
       x = "Units sold", y = NULL)
save_fig(p, "01_brand_share.png")

p <- body_tbl |> filter(body_grp != "Unknown") |> mutate(body_grp = reorder(body_grp, n)) |>
  ggplot(aes(share, body_grp)) + geom_col(fill = KIA_BLUE, width = 0.7) +
  geom_text(aes(label = percent(share, 0.1)), hjust = -0.15, size = 3.3, colour = "#52514e") +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, .12))) +
  labs(title = "Sedans and SUVs make up ~70% of the used market", x = "Share of units", y = NULL)
save_fig(p, "02_body_mix.png", 7, 4)

# ---------------------------------------------------------------------
# Q3. MARKET LEADERS & THEIR CHARACTERISTICS
# ---------------------------------------------------------------------
peers <- c("Ford", "Chevrolet", "Nissan", "Toyota", "Honda", "Hyundai", "Kia")
leaders <- brand |> filter(Make %in% peers) |>
  select(rank, Make, origin, units, unit_share, avg_price, med_price, avg_age, avg_miles, avg_cond,
         price_to_mmr, suv_share, sedan_share, pickup_share)
write_csv(leaders, "out/leaders_vs_kia.csv"); print(leaders, width = 200)

# leader state footprint: number of states where each is #1
state_leader <- cars |> count(state, Make) |> group_by(state) |> slice_max(n, n = 1) |> ungroup()
print(count(state_leader, Make, sort = TRUE))
write_csv(count(state_leader, Make, sort = TRUE), "out/state_number_one.csv")

# depreciation curves: median price by age for peers (ages 1-10)
dep <- cars |> filter(Make %in% peers, age >= 1, age <= 10) |>
  group_by(Make, age) |> summarise(med_price = median(sellingprice), n = n(), .groups = "drop") |>
  group_by(Make) |> mutate(index = med_price / med_price[age == 1] * 100) |> ungroup()
write_csv(dep, "out/depreciation_peers.csv")
p <- ggplot(dep, aes(age, index, group = Make)) +
  geom_line(data = filter(dep, Make != OUR), colour = GREY, linewidth = 0.8) +
  geom_line(data = filter(dep, Make == OUR), colour = KIA_BLUE, linewidth = 1.6) +
  geom_text(data = filter(dep, age == 10), aes(label = Make), hjust = -0.1, size = 3.2, colour = "#52514e") +
  scale_x_continuous(breaks = 1:10, expand = expansion(mult = c(0.02, .15))) +
  labs(title = "Older Kias fetch much less than older rivals",
       subtitle = "Median auction price by vehicle age, indexed to age 1 = 100",
       x = "Vehicle age (years)", y = "Price index")
save_fig(p, "03_depreciation.png")

# ---------------------------------------------------------------------
# Q1. ENVIRONMENTAL FACTORS
# ---------------------------------------------------------------------
state_df <- cars |> group_by(state) |>
  summarise(units = n(), kia_units = sum(is_ours), avg_price = mean(sellingprice),
            kia_avg_price = mean(sellingprice[is_ours]), .groups = "drop") |>
  mutate(kia_share = kia_units / units) |>
  inner_join(env, by = "state") |>
  mutate(units_per_100k = units / pop_2014 * 1e5)
write_csv(state_df, "out/state_level.csv")
log_line("States with >=100 sales used in state models: ", sum(state_df$units >= 100))

sd_m <- state_df |> filter(units >= 100) |>
  mutate(income_k = dpi_pc_2014 / 1000, dem_pct = dem_share_2012 * 100, log_pop = log(pop_2014))

# (a) market size: what drives used-car volume per capita?
m_vol <- lm(log(units_per_100k) ~ income_k + unemp_2014 + dem_pct + region, data = sd_m)
# (b) Kia share across states, weighted by state volume
m_share <- lm(I(kia_share * 100) ~ income_k + unemp_2014 + dem_pct + region, data = sd_m, weights = units)
print(summary(m_vol)); print(summary(m_share))
write_csv(tidy(m_vol), "out/model_state_volume.csv"); write_csv(glance(m_vol), "out/model_state_volume_fit.csv")
write_csv(tidy(m_share), "out/model_state_kiashare.csv"); write_csv(glance(m_share), "out/model_state_kiashare_fit.csv")

# (c) transaction level: is a given sale a Kia? (logit, ~500k obs) environmental + region
tx <- cars |> inner_join(env, by = "state") |>
  mutate(income_k = dpi_pc_2014 / 1000, dem_pct = dem_share_2012 * 100)
m_logit <- glm(is_ours ~ income_k + unemp_2014 + dem_pct + region, data = tx, family = binomial)
print(summary(m_logit))
# 500k rows but only 33 states: cluster standard errors by state, otherwise p-values are far too optimistic
logit_cl <- coeftest(m_logit, vcov = vcovCL(m_logit, cluster = ~state))
print(logit_cl)
logit_or <- tidy(logit_cl) |> mutate(odds_ratio = exp(estimate))
write_csv(logit_or, "out/model_logit_kia_clustered.csv")

# (d) hedonic price model for Kia: what drives the price of OUR used cars
kia <- tx |> filter(is_ours, !is.na(condition))
m_price <- lm(log(sellingprice) ~ age + I(odometer / 10000) + condition + body_grp + transmission +
                income_k + unemp_2014 + region + seller_type, data = kia)
print(summary(m_price))
write_csv(tidy(m_price), "out/model_kia_price.csv"); write_csv(glance(m_price), "out/model_kia_price_fit.csv")

p <- ggplot(filter(state_df, units >= 100), aes(dpi_pc_2014, kia_share)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = GREY, fill = "#ecebe6", aes(weight = units)) +
  geom_point(aes(size = units), colour = KIA_BLUE, alpha = 0.75) +
  geom_text(aes(label = toupper(state)), size = 2.8, vjust = -1.1, colour = "#52514e") +
  scale_x_continuous(labels = dollar) + scale_y_continuous(labels = percent) +
  scale_size_area(max_size = 10, labels = comma, name = "Auction units") +
  labs(title = "Kia's share tends to be higher where incomes are lower",
       subtitle = "Kia share of auction units vs. per-capita disposable income (BEA, 2014); states with 100+ sales",
       x = "Per-capita disposable personal income, 2014", y = "Kia share of units")
save_fig(p, "04_kia_share_income.png", 8.5, 5.5)

# ---------------------------------------------------------------------
# Q4. HOW WILL THE MARKET CHANGE? (time series)
# ---------------------------------------------------------------------
# (a) Long-run: BEA per-capita disposable income 1948-2015, ARIMA forecast to 2025 (national + states)
us_dpi <- bea |> filter(GeoName == "United States") |> arrange(year)
ts_us <- ts(log(us_dpi$dpi_pc), start = min(us_dpi$year))
fit_us <- auto.arima(ts_us)
fc_us <- forecast(fit_us, h = 10)
log_line("US DPI ARIMA: ", paste(capture.output(print(fit_us))[2], collapse = " "))
us_fc <- tibble(year = 2016:2025, dpi_fc = exp(as.numeric(fc_us$mean)),
                lo80 = exp(as.numeric(fc_us$lower[, 1])), hi80 = exp(as.numeric(fc_us$upper[, 1])))
write_csv(us_fc, "out/us_dpi_forecast.csv")
log_line("US per-capita DPI 2015: ", dollar(tail(us_dpi$dpi_pc, 1)), " -> 2025 forecast ", dollar(round(us_fc$dpi_fc[10])),
         " (80% PI ", dollar(round(us_fc$lo80[10])), "-", dollar(round(us_fc$hi80[10])), ")")

state_fc <- bea |> filter(GeoName %in% st_lookup$state_name, year >= 1970) |>
  group_by(GeoName) |> arrange(year) |>
  group_modify(~{
    f <- forecast(auto.arima(ts(log(.x$dpi_pc), start = min(.x$year))), h = 10)
    tibble(dpi_2015 = tail(.x$dpi_pc, 1), dpi_2025_fc = exp(as.numeric(f$mean)[10]))
  }) |> ungroup() |>
  mutate(cagr_15_25 = (dpi_2025_fc / dpi_2015)^(1/10) - 1) |> rename(state_name = GeoName)
write_csv(state_fc, "out/state_dpi_forecast.csv")

p <- ggplot() +
  geom_ribbon(data = us_fc, aes(year, ymin = lo80, ymax = hi80), fill = "#cfe0f5") +
  geom_line(data = filter(us_dpi, year >= 1980), aes(year, dpi_pc), colour = "#52514e", linewidth = 0.9) +
  geom_line(data = bind_rows(tibble(year = 2015L, dpi_fc = tail(us_dpi$dpi_pc, 1)), us_fc), aes(year, dpi_fc), colour = KIA_BLUE, linewidth = 1.2) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Per-capita disposable income forecast to keep rising through 2025",
       subtitle = "BEA SA51, US per-capita disposable income (current $); ARIMA(0,2,1) on log income, 80% interval",
       x = NULL, y = NULL)
save_fig(p, "05_income_forecast.png")

# (b) Short-run: weekly auction volume & Kia share, Jan-Jun 2015
wk <- cars |> filter(sale_date >= as.Date("2014-12-29"), sale_date <= as.Date("2015-06-28")) |>
  mutate(week = floor_date(sale_date, "week", week_start = 1)) |>
  group_by(week) |> summarise(units = n(), kia_share = mean(is_ours), avg_price = mean(sellingprice), .groups = "drop")
write_csv(wk, "out/weekly.csv")
mo <- cars |> mutate(month = floor_date(sale_date, "month")) |> count(month)
write_csv(mo, "out/monthly.csv"); print(mo)

# (c) Structural trend inside the data: product mix by model year (what is coming into the used market next)
mix <- cars |> filter(year >= 2005, body_grp %in% c("SUV", "Sedan", "Pickup", "Van/Minivan", "Hatchback", "Coupe")) |>
  count(year, body_grp) |> group_by(year) |> mutate(share = n / sum(n)) |> ungroup()
write_csv(mix, "out/bodymix_by_modelyear.csv")
lab_mix <- mix |> filter(year == 2015)
p <- ggplot(mix, aes(year, share, colour = body_grp)) +
  geom_line(linewidth = 1) +
  geom_text(data = filter(lab_mix, body_grp %in% c("SUV", "Sedan")), aes(label = body_grp), hjust = -0.1, size = 3.2, show.legend = FALSE) +
  scale_colour_manual(values = c(SUV = "#2a78d6", Sedan = "#eb6834", Pickup = "#1baf7a",
                                 `Van/Minivan` = "#eda100", Hatchback = "#e87ba4", Coupe = "#4a3aa7"), name = NULL) +
  scale_y_continuous(labels = percent) + scale_x_continuous(breaks = seq(2005, 2015, 2), expand = expansion(mult = c(0.02, .15))) +
  labs(title = "Sedan-heavy rental and lease returns dominate 2012-14 model years",
       subtitle = "Share of auction units by body type within each model year", x = "Model year", y = NULL) +
  theme(legend.position = "bottom")
save_fig(p, "06_bodymix_trend.png")

# ---------------------------------------------------------------------
# Q5. RECOMMENDATIONS
# ---------------------------------------------------------------------
# (a) Where: score states on size, Kia headroom, environmental fit (model b), and income growth
nat_kia <- mean(cars$is_ours)
log_line("National Kia unit share: ", percent(nat_kia, 0.01))
# "missing units" = sales Kia would have if it held its national share in that state
target <- state_df |> filter(units >= 1000) |> left_join(state_fc, by = "state_name") |>
  mutate(share_gap = nat_kia - kia_share,
         missing_units = units * share_gap,
         score = scale(log(units))[, 1] + scale(share_gap)[, 1] + scale(cagr_15_25)[, 1]) |>
  arrange(desc(score)) |>
  select(state, state_name, region, units, kia_units, kia_share, share_gap, missing_units, dpi_pc_2014,
         unemp_2014, dem_share_2012, cagr_15_25, score)
write_csv(target, "out/target_states.csv"); print(head(target, 12), width = 200)

reg_tbl <- cars |> inner_join(st_lookup, by = "state") |> group_by(region) |>
  summarise(units = n(), kia_units = sum(is_ours), kia_share = mean(is_ours), .groups = "drop") |>
  mutate(unit_share = units / sum(units))
write_csv(reg_tbl, "out/region.csv"); print(reg_tbl)

# (b) What: Kia models & specs
kia_models <- cars |> filter(is_ours) |> group_by(model) |>
  summarise(units = n(), avg_price = mean(sellingprice), avg_age = mean(age), price_to_mmr = mean(price_to_mmr, na.rm = TRUE),
            body = names(which.max(table(body_grp))), .groups = "drop") |>
  mutate(share_of_kia = units / sum(units)) |> arrange(desc(units))
write_csv(kia_models, "out/kia_models.csv"); print(kia_models, n = 20)

kia_body <- cars |> filter(is_ours) |> count(body_grp) |> mutate(kia_share = n / sum(n)) |>
  full_join(body_tbl |> select(body_grp, market_share = share), by = "body_grp") |> arrange(desc(market_share))
write_csv(kia_body, "out/kia_vs_market_body.csv"); print(kia_body)

# price premium by body within the market (SUV vs sedan, same age/miles) - hedonic on all mainstream brands
mkt <- filter(cars, !luxury, !is.na(condition)) |> add_count(Make) |> filter(n >= 500) |>
  mutate(Make = relevel(factor(Make), ref = "Toyota"))
m_mkt <- lm(log(sellingprice) ~ age + I(odometer / 10000) + condition + body_grp + Make, data = mkt)
write_csv(tidy(m_mkt), "out/model_market_price.csv")
log_line("Market hedonic R2: ", round(glance(m_mkt)$r.squared, 3))

# (c) consumer behaviour trends
colour_tbl <- cars |> filter(!is.na(color), color != "—") |> count(color, sort = TRUE) |> mutate(share = n / sum(n))
write_csv(head(colour_tbl, 10), "out/colors.csv")
trans_tbl <- cars |> filter(!is.na(transmission)) |> count(transmission) |> mutate(share = n / sum(n))
write_csv(trans_tbl, "out/transmission.csv")
age_tbl <- cars |> mutate(age_band = cut(age, c(-1, 1, 3, 5, 8, 50), labels = c("0-1", "2-3", "4-5", "6-8", "9+"))) |>
  group_by(age_band) |> summarise(units = n(), avg_price = mean(sellingprice), above_mmr = mean(sellingprice > mmr, na.rm = TRUE), .groups = "drop") |>
  mutate(share = units / sum(units))
write_csv(age_tbl, "out/age_bands.csv"); print(age_tbl)
dow <- cars |> mutate(dow = wday(sale_date, label = TRUE)) |> count(dow) |> mutate(share = n / sum(n))
write_csv(dow, "out/day_of_week.csv")

log_line("Monthly volume (coverage check): ", paste(format(mo$month, "%b-%y"), mo$n, collapse = "; "))

p <- kia_models |> filter(units >= 200) |> mutate(model = reorder(tools::toTitleCase(model), units),
                                                  is_suv = body == "SUV") |>
  ggplot(aes(units, model, fill = is_suv)) + geom_col(width = 0.7) +
  geom_text(aes(label = dollar(round(avg_price, -2))), hjust = -0.15, size = 3.2, colour = "#52514e") +
  scale_fill_manual(values = c(`TRUE` = KIA_BLUE, `FALSE` = GREY), labels = c(`TRUE` = "SUV", `FALSE` = "Car / van"), name = NULL) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
  labs(title = "Optima and Sorento carry Kia's used volume", subtitle = "Kia auction units by model (labels = average selling price)",
       x = "Units sold", y = NULL) + theme(legend.position = "top")
save_fig(p, "07_kia_models.png", 7.5, 4.5)

# demand signal by body type: how often buyers pay above the MMR benchmark, and price premium vs sedans (hedonic)
body_demand <- cars |> filter(!is.na(mmr), body_grp != "Unknown") |> group_by(body_grp) |>
  summarise(units = n(), above_mmr = mean(sellingprice > mmr), price_to_mmr = median(price_to_mmr), .groups = "drop")
write_csv(body_demand, "out/body_demand.csv"); print(body_demand)
kia_st <- target |> slice_head(n = 10) |> mutate(state = reorder(toupper(state), score))
p <- ggplot(kia_st, aes(score, state)) + geom_col(fill = KIA_BLUE, width = 0.7) +
  labs(title = "Expansion priority: top 10 states", subtitle = "Score = market size + Kia share gap + forecast income growth (sum of z-scores)",
       x = "Score", y = NULL)
save_fig(p, "08_target_states.png", 8, 4.5)

sessionInfo() |> capture.output() |> writeLines("out/sessionInfo.txt")
cat("\nDONE\n")
