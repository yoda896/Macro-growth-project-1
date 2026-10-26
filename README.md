# Macro Project 1 – Growth Detective

Nick Noll, ECON 8030 Macroeconomic Theory (Prof. Zhao), October 2026.
Countries: Japan (advanced), Argentina (emerging), Rwanda (low income). Part 4 uses all 38 OECD members.

## Project structure

- `script for macro project.R` – the whole project in one script, in order:
  - Part 1: data collection and cleaning (PWT 11.0 + World Bank inflation)
  - Part 2: growth rates, averages, volatility, Charts 1–5
  - Part 3: Japan by period, growth accounting, Chart 6, event table
  - Part 4: 38 OECD countries, long-run averages, Chart 7, correlation matrix
  - Tables 1–5 for the report, written to CSV
- `Macro growth project 1.Rproj` – RStudio project file. Open this first so the working directory is the project folder.
- `data for Macro Project 1/` – holds `pwt110.xlsx`. Not uploaded to GitHub (the script writes a `.gitignore` that excludes it, as the assignment asks).
- Outputs the script saves in the project folder:
  - Charts: `growth_chart_1.png`, `Growth_Level_Chart.png`, `ThreeCountry_Growth_Chart.png`, `Solow_model_chart.png`, `Production_Function_Chart.png`, `Japan_Chart.png`, `OECD_ScatterChart.png`
  - Tables: `Table1_Growth.csv`, `Table2_Country.csv`, `Table3_Events.csv`, `Table4_Accounting.csv`, `Table5_Correlations.csv`

The report is submitted on Canvas, not kept in the repo.

## Data sources

1. **Penn World Table 11.0** – Feenstra, R. C., Inklaar, R., & Timmer, M. P. (2015), "The Next Generation of the Penn World Table," *American Economic Review* 105(10), 3150–3182. Downloaded as an Excel file from www.ggdc.net/pwt (sheet "Data"). Variables used: `rgdpna` (real GDP), `pop` (population), `emp` (employment), `rnna` (capital stock), `csh_i` (investment share of GDP), `delta` (depreciation rate), `hc` (human capital index), `rtfpna` (TFP index, 2021 = 1), and `labsh` (labor share, used only to get Japan's capital share α = 1 − labsh for the growth accounting).
2. **World Bank, World Development Indicators** – inflation measured by the GDP deflator (`NY.GDP.DEFL.KD.ZG`), 1960–2023, for the three countries. Downloaded by the script with the `WDI` R package (needs an internet connection). Inflation is collected and merged in Part 1 and comes out in the event table CSV, but it is not used in the report's analysis.

## Preprocessing steps

- One table per country is built from PWT, then the World Bank inflation series is merged on year (years before 1960 are `NA` because the series starts in 1960).
- Missing values are checked with `colSums(is.na(...))`. The only gaps in the chosen variables are TFP: PWT reports it from 1954 for Japan and Argentina and from 1964 for Rwanda (Rwanda's other series start in 1960). These were left as `NA`, not filled in.
- Real GDP per person = `rgdpna / pop`. Yearly growth = percent change from the previous year. Part 2 uses 1961–2023, the first year all three countries have a growth rate.
- Part 3 uses Japan from 1955 (first year with a TFP growth rate) in three periods: before 1990, 1990–2007, 2008–2023. The growth accounting follows Week 5: growth of GDP per person = [α/(1−α)] × growth of K/Y + growth of human capital + growth of workers per person + TFP (the residual), with log growth rates and α = 0.44 (1 − Japan's average `labsh` from 1955).
- Part 4 uses the 38 OECD member countries from 1995 to 2023, the first year every country has every variable (Czechia, Slovakia, Slovenia, Estonia, Latvia and Lithuania enter PWT in 1990 with TFP from 1994). Each country gets one long-run average per variable; log GDP per person in 1995 is the starting-income variable.

## How to replicate

1. Install R (4.x) and RStudio, then the packages:
   `install.packages(c("readxl", "WDI", "dplyr", "ggplot2", "tidyr"))`
2. Download Penn World Table 11.0 as an Excel file (`pwt110.xlsx`) from www.ggdc.net/pwt and put it in a folder named `data for Macro Project 1` inside the project folder.
3. Open `Macro growth project 1.Rproj` in RStudio, then open `script for macro project.R`.
4. Session → Restart R, then Code → Run Region → Run All (Ctrl+Alt+R). The World Bank download needs internet. The script takes about a minute.
5. The seven charts and five CSV tables appear in the project folder, and the console prints `GrowthSummary`, `Part2_Summary`, `Japan_Growth_Accounting`, `Japan_Periods`, `Event_Wide`, `OECD_Averages` and the correlation matrix.
