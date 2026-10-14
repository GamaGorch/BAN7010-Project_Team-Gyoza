# BAN 7010 Team Project – US Used-Car Market (Team Gyoza)

Analysis of the US used-car auction market (Dec 2014 – Jul 2015) for our chosen company, **Kia**.
Deadline: **October 25, 2026, 11:59 PM**.

## How to run

1. Clone this repo (RStudio: *File → New Project → Version Control → Git*).
2. Copy **`car_prices.csv`** from the course site into this folder. It is not stored on GitHub because it is too large.
3. Install packages once:
   ```r
   install.packages(c("readr","dplyr","tidyr","ggplot2","lubridate","scales",
                      "broom","forecast","sandwich","lmtest"))
   ```
4. Open `kia_used_car_analysis.R` and click **Source**. Charts go to `fig/` and tables to `out/`.

## Files

| File | What it is |
|---|---|
| `kia_used_car_analysis.R` | The full analysis: cleaning, Q1–Q5 |
| `bea_sa51.csv` | BEA per-capita disposable income by state, 1948–2015 |
| `state_year.rda` | BLS state unemployment rates (annual) |
| `2014_usa_states.csv` | Census 2014 state population |
| `elec.csv` | County presidential results 2008–2016 (aggregated to states in the script) |
| `fig/` | Charts produced by the script |
| `out/` | Tables and model results produced by the script |

## Who owns what

| Role | Owner | Covers |
|---|---|---|
| Data lead | | Cleaning, merging, shared script, state targeting (Q5a) |
| Market analyst | | Industry structure and leaders (Q2, Q3) |
| Modeler | | Regressions and forecasts (Q1, Q4) |
| Research & writing | | Literature review, recommendations, final report (Q5b, Q5c) |

## Team rules

- **Pull before you start, push when you stop.** That way nobody works on an old copy.
- **One shared script.** Add your section to `kia_used_car_analysis.R` under its heading; don't make private copies.
- **Short commit messages that say what changed**, e.g. "Add Q2 origin chart".
