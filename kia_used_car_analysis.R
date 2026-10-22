# =====================================================================
# BAN 7010 - Team Project (Team Gyoza)
# Company: KIA  |  US used-car market analysis (auction data Dec-2014 to Jul-2015)
# =====================================================================
# Inputs (put in the working directory):
#   car_prices.csv                 
#   bea_sa51.csv                   
#   state_year.rda                 
#   2014_usa_states.csv            
#   elec.csv                       
# Outputs: figures in ./fig, tables in ./out

suppressMessages({
  library(readr); library(dplyr); library(tidyr); library(ggplot2)
  library(lubridate); library(scales); library(broom); library(forecast)
  library(sandwich); library(lmtest); library(cluster); library(jsonlite)
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

# LOAD + CLEAN
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


# ENVIRONMENTAL DATA (BEA income, BLS unemployment, Census population, politics, region)
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

# INDUSTRY STRUCTURE

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

# MARKET LEADERS & THEIR CHARACTERISTICS
peers <- c("Ford", "Chevrolet", "Nissan", "Toyota", "Honda", "Hyundai", "Kia")
leaders <- brand |> filter(Make %in% peers) |>
  select(rank, Make, origin, units, unit_share, avg_price, med_price, avg_age, avg_miles, avg_cond,
         price_to_mmr, suv_share, sedan_share, pickup_share)
write_csv(leaders, "out/leaders_vs_kia.csv"); print(leaders, width = 200)

# leader state footprint: number of states where each is #1
state_leader <- cars |> count(state, Make) |> group_by(state) |> slice_max(n, n = 1) |> ungroup()
print(count(state_leader, Make, sort = TRUE))
write_csv(count(state_leader, Make, sort = TRUE), "out/state_number_one.csv")

# depreciation curves: median price by age for peers
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

# ENVIRONMENTAL FACTORS
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

# market size: what drives used-car volume per capita?
m_vol <- lm(log(units_per_100k) ~ income_k + unemp_2014 + dem_pct + region, data = sd_m)
# Kia share across states, weighted by state volume
m_share <- lm(I(kia_share * 100) ~ income_k + unemp_2014 + dem_pct + region, data = sd_m, weights = units)
print(summary(m_vol)); print(summary(m_share))
write_csv(tidy(m_vol), "out/model_state_volume.csv"); write_csv(glance(m_vol), "out/model_state_volume_fit.csv")
write_csv(tidy(m_share), "out/model_state_kiashare.csv"); write_csv(glance(m_share), "out/model_state_kiashare_fit.csv")

# transaction level
tx <- cars |> inner_join(env, by = "state") |>
  mutate(income_k = dpi_pc_2014 / 1000, dem_pct = dem_share_2012 * 100)
m_logit <- glm(is_ours ~ income_k + unemp_2014 + dem_pct + region, data = tx, family = binomial)
print(summary(m_logit))
# 500k rows but only 33 states: cluster standard errors by state, otherwise p-values are far too optimistic
logit_cl <- coeftest(m_logit, vcov = vcovCL(m_logit, cluster = ~state))
print(logit_cl)
logit_or <- tidy(logit_cl) |> mutate(odds_ratio = exp(estimate))
write_csv(logit_or, "out/model_logit_kia_clustered.csv")

# hedonic price model for Kia: what drives the price of OUR used cars
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

# HOW WILL THE MARKET CHANGE? (time series)
# Long-run: BEA per-capita disposable income 1948-2015, ARIMA forecast to 2025 (national + states)
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

# Short-run: weekly auction volume & Kia share, Jan-Jun 2015
wk <- cars |> filter(sale_date >= as.Date("2014-12-29"), sale_date <= as.Date("2015-06-28")) |>
  mutate(week = floor_date(sale_date, "week", week_start = 1)) |>
  group_by(week) |> summarise(units = n(), kia_share = mean(is_ours), avg_price = mean(sellingprice), .groups = "drop")
write_csv(wk, "out/weekly.csv")
mo <- cars |> mutate(month = floor_date(sale_date, "month")) |> count(month)
write_csv(mo, "out/monthly.csv"); print(mo)

# Structural trend inside the data: product mix by model year (what is coming into the used market next)
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

# RECOMMENDATIONS
# Score states on size, Kia headroom, environmental fit (model b), and income growth
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

# Kia models & specs
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

# consumer behaviour trends
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


# DESCRIPTIVE STATISTICS AND SEGMENTATION / CLUSTERING
# Descriptive statistics of the key numeric variables: whole market vs Kia
desc_vars <- c(sellingprice = "Selling price ($)", mmr = "MMR benchmark ($)", odometer = "Odometer (miles)",
               age = "Vehicle age (years)", condition = "Condition (1-5)")
desc_one <- function(df, grp) {
  bind_rows(lapply(names(desc_vars), function(v) {
    x <- df[[v]]; x <- x[!is.na(x)]
    tibble(group = grp, variable = desc_vars[[v]], n = length(x), mean = mean(x), sd = sd(x),
           min = min(x), q1 = quantile(x, .25), median = median(x), q3 = quantile(x, .75), max = max(x))
  }))
}
desc_tbl <- bind_rows(desc_one(cars, "All makes"), desc_one(filter(cars, is_ours), "Kia"))
write_csv(desc_tbl, "out/descriptive_stats.csv"); print(desc_tbl, n = 20, width = 200)

# Brand segmentation

discontinued <- c("Pontiac", "Saturn", "Mercury", "Hummer", "Oldsmobile", "Saab", "Plymouth")
brand_feat <- brand |> filter(units >= 1000, !Make %in% discontinued) |>
  select(Make, avg_price, avg_age, avg_miles, avg_cond, suv_share, sedan_share, pickup_share)
Xb <- scale(brand_feat[, -1])
set.seed(7010)
sil_b <- sapply(2:6, function(k) mean(silhouette(kmeans(Xb, k, nstart = 50)$cluster, dist(Xb))[, 3]))
k_b <- (2:6)[which.max(sil_b)]
log_line("Brand clustering: ", nrow(brand_feat), " makes; silhouette k=2..6: ",
         paste(round(sil_b, 3), collapse = ", "), " -> k = ", k_b)
set.seed(7010)
km_b <- kmeans(Xb, k_b, nstart = 50)
brand_feat$cluster <- km_b$cluster
# name each segment from its profile
seg_prof <- brand_feat |> group_by(cluster) |> summarise(suv = mean(suv_share), age = mean(avg_age), .groups = "drop")
seg_name <- with(seg_prof, ifelse(suv == max(suv), "SUV specialists",
                           ifelse(age == min(age), "Young, low-mileage volume brands", "Older, sedan-heavy brands")))
brand_feat$segment <- seg_name[match(brand_feat$cluster, seg_prof$cluster)]
brand_seg <- brand_feat |> group_by(cluster) |>
  summarise(segment = first(segment), n_makes = n(), makes = paste(Make, collapse = ", "),
            avg_price = mean(avg_price), avg_age = mean(avg_age), avg_miles = mean(avg_miles),
            suv_share = mean(suv_share), pickup_share = mean(pickup_share), .groups = "drop") |>
  arrange(avg_price)
write_csv(brand_feat, "out/brand_clusters.csv"); write_csv(brand_seg, "out/brand_segments.csv")
print(brand_seg, width = 200)
log_line("Kia's brand segment: ", brand_feat$segment[brand_feat$Make == OUR], " = ",
         brand_seg$makes[brand_seg$cluster == brand_feat$cluster[brand_feat$Make == OUR]])

pc_b <- prcomp(Xb)
pcb <- tibble(Make = brand_feat$Make, PC1 = pc_b$x[, 1], PC2 = pc_b$x[, 2], cluster = factor(brand_feat$segment))
seg_cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#4a3aa7")
p <- ggplot(pcb, aes(PC1, PC2, colour = cluster)) +
  geom_point(size = 3) +
  geom_text(aes(label = Make), vjust = -0.9, size = 2.7, colour = "#52514e") +
  geom_point(data = filter(pcb, Make == OUR), shape = 21, size = 6, stroke = 1.4, colour = "#0b0b0b") +
  scale_colour_manual(values = seg_cols, name = "Segment") +
  labs(title = "Brand segments from k-means clustering",
       subtitle = paste0("Makes with 1,000+ sales; price, age, miles, condition, body mix (k = ", k_b, "). Kia circled."),
       x = paste0("PC1 (", round(summary(pc_b)$importance[2, 1] * 100), "% of variance)"),
       y = paste0("PC2 (", round(summary(pc_b)$importance[2, 2] * 100), "% of variance)"))
save_fig(p, "09_brand_segments.png", 9, 6)

# State market segmentation
state_mix <- cars |> group_by(state) |>
  summarise(suv_share = mean(body_grp == "SUV"), pickup_share = mean(body_grp == "Pickup"), .groups = "drop")
st_feat <- sd_m |> left_join(state_mix, by = "state") |>
  transmute(state, state_name, region, units, kia_share, log_units_pc = log(units_per_100k), avg_price,
            suv_share, pickup_share, income_k, unemp_2014, dem_pct)
Xs <- scale(st_feat |> select(log_units_pc, avg_price, kia_share, suv_share, pickup_share, income_k, unemp_2014, dem_pct))
set.seed(7010)
sil_s <- sapply(2:6, function(k) mean(silhouette(kmeans(Xs, k, nstart = 50)$cluster, dist(Xs))[, 3]))
# k = 3: silhouette is essentially flat for k = 3-6 (~0.20, weak structure); k = 4 only splits off Utah as a

k_s <- 3
log_line("State clustering: ", nrow(st_feat), " states; silhouette k=2..6: ",
         paste(round(sil_s, 3), collapse = ", "), " -> k = ", k_s)
set.seed(7010)
km_s <- kmeans(Xs, k_s, nstart = 50)
st_feat$cluster <- km_s$cluster
sprof <- st_feat |> group_by(cluster) |> summarise(inc = mean(income_k), ks = weighted.mean(kia_share, units), .groups = "drop")
s_name <- with(sprof, ifelse(inc == max(inc), "Affluent coastal, low Kia share",
                      ifelse(ks == max(ks), "Large mid-income, Kia stronghold", "Lower-income, truck-heavy")))
st_feat$segment <- s_name[match(st_feat$cluster, sprof$cluster)]
state_seg <- st_feat |> group_by(cluster) |>
  summarise(segment = first(segment), n_states = n(), states = paste(toupper(state), collapse = ", "), auction_units = sum(units),
            kia_share = weighted.mean(kia_share, units), income_k = mean(income_k), unemp = mean(unemp_2014),
            dem_pct = mean(dem_pct), suv_share = mean(suv_share), pickup_share = mean(pickup_share),
            avg_price = mean(avg_price), .groups = "drop") |>
  arrange(desc(auction_units))
write_csv(st_feat, "out/state_clusters.csv"); write_csv(state_seg, "out/state_segments.csv")
print(state_seg, width = 250)

pc_s <- prcomp(Xs)
pcs <- tibble(state = toupper(st_feat$state), PC1 = pc_s$x[, 1], PC2 = pc_s$x[, 2],
              cluster = factor(st_feat$segment), units = st_feat$units)
p <- ggplot(pcs, aes(PC1, PC2, colour = cluster)) +
  geom_point(aes(size = units), alpha = 0.8) +
  geom_text(aes(label = state), vjust = -1.1, size = 2.9, colour = "#52514e") +
  scale_colour_manual(values = seg_cols, name = "Segment") +
  scale_size_area(max_size = 9, labels = comma, name = "Auction units") +
  labs(title = "State market segments from k-means clustering",
       subtitle = paste0("31 states clustered on volume per capita, price, Kia share, body mix, income, unemployment, politics (k = ", k_s, ")"),
       x = paste0("PC1 (", round(summary(pc_s)$importance[2, 1] * 100), "% of variance)"),
       y = paste0("PC2 (", round(summary(pc_s)$importance[2, 2] * 100), "% of variance)"))
save_fig(p, "10_state_segments.png", 9, 6)

# MAIN EFFECTS MODEL - formatted summary table
main_tbl <- tidy(m_mkt, conf.int = TRUE) |>
  mutate(pct_effect = (exp(estimate) - 1) * 100,
         term = gsub("body_grp", "Body: ", gsub("^Make", "Make: ", term)),
         term = recode(term, "I(odometer/10000)" = "Odometer (per 10,000 miles)", "age" = "Age (per year)",
                       "condition" = "Condition (per point)", "(Intercept)" = "Intercept"))
write_csv(main_tbl, "out/main_effects_model.csv")
fit_main <- glance(m_mkt)
log_line("Main effects model: n = ", comma(fit_main$nobs), ", R2 = ", round(fit_main$r.squared, 3),
         ", adj R2 = ", round(fit_main$adj.r.squared, 3), ", F p-value < 0.001")

# DASHBOARD DATA

dash <- list(
  kpis = list(units = nrow(cars), makes = nrow(brand), kia_units = sum(cars$is_ours), kia_share = nat_kia,
              median_price = median(cars$sellingprice), hhi = round(hhi), cr4 = cr4),
  brands = brand |> slice_head(n = 20) |> select(Make, origin, units, unit_share, avg_price, avg_age, avg_miles, suv_share),
  states = state_df |> left_join(state_fc |> select(state_name, cagr_15_25), by = "state_name") |>
    left_join(st_feat |> select(state, cluster, segment), by = "state") |>
    left_join(target |> select(state, score, missing_units), by = "state") |>
    select(state, state_name, region, units, kia_units, kia_share, avg_price, dpi_pc_2014, unemp_2014,
           dem_share_2012, pop_2014, cagr_15_25, cluster, segment, score, missing_units),
  kia_models = kia_models |> filter(units >= 50),
  body = body_tbl, dep = dep, income_hist = filter(us_dpi, year >= 1980) |> select(year, dpi_pc),
  income_fc = us_fc, mix = mix, brand_segments = brand_feat |> select(Make, segment),
  state_segments = state_seg
)
write_json(dash, "out/dashboard_data.json", auto_unbox = TRUE, digits = 6, pretty = FALSE)

sessionInfo() |> capture.output() |> writeLines("out/sessionInfo.txt")
cat("\nDONE\n")
