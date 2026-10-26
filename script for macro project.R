# Macroeconomic Growth Project
#countries chosen: Japan(advanced), Argentina(emerging), Rwanda(low income)

#Data Sources:

#1) Penn World Table 11.0(citation is to the right): Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015),"The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at www.ggdc.net/pwt
#   -> I downloaded the excel file for the data. Variables I used were: 1) rgdpna, 2) pop, 3) emp, 4) rnna, 5)csh_i, 6) delta, 7) hc, 8) rtfpna with respect to each of the 3 countries I picked.
#World Bank -- World development indicators. this was downloaded with the WDI package.
# -> 1) used yearly inflation rate which is measured by the GDP deflator. The indicator for inflation was: NY.GDP.DEFL.KD.ZG   

library(readxl) #used to read the PWT excel File
library(WDI) #need to download WDI package first then allows you to get data from the World developement indicators
library(dplyr)
library(ggplot2)
library(tidyr)


#part 1:

pwt <- read_excel("data for Macro Project 1/pwt110.xlsx", sheet = "Data")  #this line reads the PWT Excel file and loads it into R
writeLines(c(".Rproj.user/", ".Rhistory", ".RData", ".Ruserdata",
             "data for Macro Project 1/"), ".gitignore")
#writeLines 

options("scipen"=999, digits=5) # this line does not allow R to write in scientific notation

WorldBank <- WDI(country = c("JP", "AR", "RW"), indicator = c(inflation = "NY.GDP.DEFL.KD.ZG"), start = 1960, end = 2023)
# above line gets the inflation % per year for Japan, Argentina, and Rwanda for years 1960 to 2023

Japan <- subset(pwt, country =="Japan")
Argentina <- subset(pwt, country =="Argentina")
Rwanda <- subset(pwt, country =="Rwanda")
# the above three lines get all of the variables for each country from PWT

data1 <- data.frame(country = Japan$country, Year = Japan$year, realGDP = Japan$rgdpna, population = Japan$pop, employment = Japan$emp, capitalstock = Japan$rnna, investment = Japan$csh_i, depreciation = Japan$delta, humanCapital = Japan$hc, TotalFactorProductivity = Japan$rtfpna)
data2 <- data.frame(country = Argentina$country, Year = Argentina$year, realGDP = Argentina$rgdpna, population = Argentina$pop, employment = Argentina$emp, capitalstock = Argentina$rnna, investment = Argentina$csh_i, depreciation = Argentina$delta, humanCapital = Argentina$hc, TotalFactorProductivity = Argentina$rtfpna)
data3 <- data.frame(country = Rwanda$country, Year = Rwanda$year, realGDP = Rwanda$rgdpna, population = Rwanda$pop, employment = Rwanda$emp, capitalstock = Rwanda$rnna, investment = Rwanda$csh_i, depreciation = Rwanda$delta, humanCapital = Rwanda$hc, TotalFactorProductivity = Rwanda$rtfpna)
#the above 3 lines now only keeps 8 variables for each country which are 1)realGDP, 2) population, 3) employement, 4) capital stock, 5)investment, 6)depreciation, 7) human capital, and 8) total factor 


Japan_data <- merge(data1, subset(WorldBank, country == "Japan")[, c("year", "inflation")], by.x = "Year", by.y = "year", all.x = TRUE)
Argentina_data <- merge(data2, subset(WorldBank, country == "Argentina")[, c("year", "inflation")], by.x = "Year", by.y = "year", all.x = TRUE)
Rwanda_data <- merge(data3, subset(WorldBank, country == "Rwanda")[, c("year", "inflation")], by.x = "Year", by.y = "year", all.x = TRUE)
#above 3 lines adds the inflation variable from WDI to each country. each year's inflation rate is placed next to the same year in the PWT data. years before 1960 will have an NA in inflation since the WDI data didnt start until 1960.

DataCollection <- rbind(Japan_data, Argentina_data, Rwanda_data)
#above line combines the 3 countries data into 1 table

colSums(is.na(Japan_data))
colSums(is.na(Argentina_data))
colSums(is.na(Rwanda_data))

#Part 2: Comparing Growth across Countries

#Part 2 Step 1: Calculating Growth Rates
# gdp_pc: real GDP per person(capita) = (realGDP/population)
#Growth_gdp_pc: % change in gdp_pc from the previous year

GrowthData <- DataCollection %>%
  arrange(country, Year) %>%
  group_by(country) %>%
  mutate(gdp_pc = realGDP/population,
         growth_gdp_pc = 100 * (gdp_pc/lag(gdp_pc) - 1),
         growth_pop = 100*(population/lag(population) -1)) %>%
  ungroup()

subset(GrowthData, country == "Rwanda" & Year %in% 1993:1995)[, c("country", "Year", "growth_gdp_pc")]

inflation_info <- WDIsearch("NY.GDP.DEFL.KD.ZG", field = "indicator", short = FALSE)
inflation_info$description
#dont necessarily need the above 4 lines above but useful for the definition of inflation.

Part2 <- subset(GrowthData, Year >= 1961) #this line gets the information for each country for years 1961 to 2023

GrowthSummary <- Part2 %>%
  group_by(country) %>%
  summarise(avg_growth = mean(growth_gdp_pc, na.rm = TRUE),
            volatility_sd = sd(growth_gdp_pc, na.rm = TRUE))
GrowthSummary
# lines 74 to 78 groups the data in "Part2" by country.Then it calculates average growth for each country and then calculates the volatility for each country. Then it prints the Growth Summary.

#Part 2 section 2: Visualizing Growth

#Chart 1: Chart for each Country individually with its own seperate scales

AvgLines <- Part2 %>% group_by(country) %>% summarize(avg = mean(growth_gdp_pc)) #this gets the average growth for each country, then creates a 3 row table with each country's average growth.

GrowthChart <- ggplot(Part2, aes(x = Year, y = growth_gdp_pc, colour = country)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_hline(data = AvgLines, aes(yintercept = avg, colour = country), linetype = "dashed") +
  geom_line() +
  facet_wrap(~ country, ncol = 1) +
  coord_cartesian(ylim = c(-20, 20)) +
  labs(title = "Yearly growth of real GDP per person (1961-2023",
       subtitle = "Dashed line = each country's average growth",
       y = "Growth (%)", x = NULL,
       caption = "Source: Penn World Table 11.0. Rwanda 1994 (-40%) and 1995 (+60%) go beyond the chart.") +
  theme_minimal()
  GrowthChart
  ggsave("growth_chart_1.png", GrowthChart, width = 8, height = 7)
    

#Chart 2: GDP per person is in dollars. The chart is a log scale chart. All 3 countries are on the same graph.
  #log scale graph means that the slope of the line for a country is its growth rate. a steeper line means that the country grows faster. if a country's line is less steep, then the country grows at a slower rate.

Growth_Log_Chart <- ggplot(Part2, aes(x = Year, y = gdp_pc, colour = country)) +
            geom_line() +
  scale_y_log10(labels = scales::label_dollar()) +
  labs(title = "Real GDP per person 1961-2023 (log scale)",
       y = "2021 US$ per person", x = NULL,
       caption = "Source: Penn World Table 11.0") +
  theme_minimal()

Growth_Log_Chart
ggsave("Growth_Level_Chart.png", Growth_Log_Chart, width = 8, height = 5)


#Chart 3: Graph shows the Yearly Growth Rates for each country all on 1 graph

ThreeCountry_Growth_Chart <- ggplot(Part2, aes(x = Year, y = growth_gdp_pc, colour = country)) +
  geom_hline(yintercept = 0, colour = "grey60") + 
  geom_line() +
  labs(title = "Yearly growth of real GDP per person for each Country (1961-2023)",
       y = "Growth (%)", x = NULL,
       caption = "Source: Penn World Table 11.0") + 
  theme_minimal()

ThreeCountry_Growth_Chart
ggsave("ThreeCountry_Growth_Chart.png", ThreeCountry_Growth_Chart, width = 8, height = 5)

#Chart 4: Solow Model this chart shows a graph for s, n, and delta.

Solow_model <- Part2 %>%
  transmute(country, Year,
            "investment share of GDP, s(%)" = 100*investment,
            "population growth, n (%)" = growth_pop,
            "depreciation rate, delta (%)" = 100*depreciation) %>%
  pivot_longer(-c(country, Year), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = unique(variable)))

Solow_model_Chart <- ggplot(Solow_model, aes( x = Year, y = value, colour = country)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ variable, ncol = 3, scales = "free_y") +
  labs(title = "Solow Model (1961-2023", 
       y = NULL, x = NULL,
       caption = "Source: Penn World Table 11.0") + 
  theme_minimal()

Solow_model_Chart
ggsave("Solow_model_chart.png", Solow_model_Chart, width = 11, height = 4)

#Chart 5: Production Function Solow Model

Production_Function <- Part2 %>%
  transmute(country, Year,
           "capital to output ratio (K/Y)" = capitalstock/realGDP,
           "Workers per person (L/N" = employment/population,
           "Human capital index (h)" = humanCapital,
           "TFP index (A) (2021 = 1)" = TotalFactorProductivity) %>%
  pivot_longer(-c(country, Year), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = unique(variable)))

Production_Function_Chart <- ggplot(Production_Function, aes(x = Year, y = value, colour = country)) +
  geom_line(na.rm =TRUE) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  labs(title = "Production function's inputs (1961-2023",
       y = NULL, x = NULL,
       caption = "Source: Penn World Table 11.0") +
  theme_minimal()

Production_Function_Chart
ggsave("Production_Function_Chart.png", Production_Function_Chart, width = 9, height = 6)

#Part 2 step 3: variables for each country 
Part2_Summary <- Part2 %>%
  group_by(country) %>%
  summarise(gdp_pc_1961 = first(gdp_pc),
            gdp_pc_2023 = last(gdp_pc),
            avg_investment = 100 * mean(investment),
            avg_pop_growth = mean(growth_pop),
            k_y_1961 = first(capitalstock / realGDP),
            k_y_2023 = last(capitalstock / realGDP),
            hc_1961 = first(humanCapital),
            hc_2023 = last(humanCapital),
            tfp_start = first(na.omit(TotalFactorProductivity)),
            tfp_2023 = last(TotalFactorProductivity))
Part2_Summary

sd(subset(Part2, country == "Rwanda" & !(Year %in% 1994:1995))$growth_gdp_pc)


#Part 3: Growth Changes in Japan
#Japan's growth before 1990, growth from 1990 to 2007, Japan's Growth from 2008 to 2023

#Japan's alpha
Japan_Alpha <- 1 - mean(subset(pwt, country == "Japan" & year >= 1955)$labsh)
Japan_Alpha


Japan_Growth <- GrowthData %>%
  filter(country == "Japan") %>%
  mutate(k_y = capitalstock/realGDP,
         l_n = employment/population,
         growth_gdp = 100 * (realGDP/lag(realGDP) - 1),
         ga_total = 100*(log(gdp_pc) - log(lag(gdp_pc))),
         ga_ky = Japan_Alpha/(1 - Japan_Alpha) * 100 * (log(k_y) - log(lag(k_y))),
         ga_h = 100 * (log(humanCapital) - log(lag(humanCapital))),
         ga_ln = 100 * (log(l_n) - log(lag(l_n))),
         ga_A = ga_total - ga_ky - ga_h - ga_ln) %>%
  filter(Year >= 1955) %>%
  mutate(period = case_when(Year < 1990 ~ "Before 1990",
                            Year <= 2007 ~ "1990-2007",
                            TRUE         ~ "2008-2023"),
         period = factor(period, levels = c("Before 1990", "1990-2007", "2008-2023")))
        
    
    
Japan_Growth_Accounting <- Japan_Growth %>%
  group_by(period) %>%
  summarise(total = mean(ga_total),
            from_capital = mean(ga_ky),
            from_human_capital = mean(ga_h),
            from_TFP = mean(ga_A),
            from_workers_per_person = mean(ga_ln))
Japan_Growth_Accounting


#average growth per period
Japan_Periods <- Japan_Growth %>%
  group_by(period) %>%
  summarize(years = paste(min(Year), max(Year), sep = "-"),
            avg_growth = mean(growth_gdp_pc),
            avg_gdp_growth = mean(growth_gdp),
            volatility_sd = sd(growth_gdp_pc),
            avg_investment = 100 * mean(investment),
            avg_pop_growth = mean(growth_pop))
Japan_Periods

#Part 3 step 2
Japan_Avg <- Japan_Growth %>% group_by(period) %>%
  summarise(start = min(Year), end = max(Year), avg = mean(growth_gdp_pc))

Japan_Growth %>% arrange(growth_gdp_pc) %>% select(Year, growth_gdp_pc) %>% head(10) %>% arrange(Year) %>% select(Year, growth_gdp_pc)
Japan_Growth %>% arrange(desc(growth_gdp_pc)) %>% select(Year, growth_gdp_pc) %>% head(10) %>% arrange(Year) %>% select(Year, growth_gdp_pc)


#Part 3 step 3: chart of Japan's yearly growth with period averages and important events
Japan_Events <- data.frame(
  Year = c(1958, 1965, 1971, 1973, 1991, 1997, 2008, 2011, 2020),
  event = c("Iwato Boom", "Izanagi Boom", "Nixon Shock", "Oil shock", "Bubble bursts", "Banking crisis", "financial crisis", "Earthquake & tsunami", "COVID-19"))

Japan_Chart <- ggplot(Japan_Growth, aes(x = Year, y = growth_gdp_pc)) + 
  annotate("rect", xmin = 1989.5, xmax = 2007.5, ymin = -Inf, ymax = Inf, fill = "grey92") + #this shades 1990-2007 with the color grey92
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(data = Japan_Events, aes(xintercept = Year), colour = "grey50", linetype = "dotted") +
  geom_text(data = Japan_Events, aes(x = Year, y = -6.8, label = event), inherit.aes = FALSE,
            angle = 90, hjust = 0, vjust = -0.4, size = 3, colour = "grey30") +
  geom_segment(data = Japan_Avg, aes(x = start, xend = end, y = avg, yend = avg), inherit.aes = FALSE, colour = "black", linewidth = 0.8) +
  geom_label(data = Japan_Avg, aes(x = c(1980, 2005, 2015.5), y = avg, vjust = c(-0.4, 1.4, 1.4), label = paste0("avg ", sprintf("%.1f", avg), "%")), inherit.aes = FALSE, fontface = "bold", size = 3.5) +
  geom_line(colour = "#00BA38") +
  geom_point(colour = "#00BA38", size = 1) +
  labs(title = "Japan: yearly growth of real GDP per person (1955-2023)",
       subtitle = "Black lines = average growth in each period. Shaded = 1990-2007. dotted lines = important events.",
       y = "Growth (%)", x = NULL, caption = "Source: Penn World Table 11.0") +
  scale_x_continuous(breaks = 1955:2023) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7), panel.grid.minor = element_blank())
Japan_Chart

ggsave("Japan_Chart.png", Japan_Chart, width = 12, height = 5)
 
#Part 3 step 4: how variables affected each event
Event_Years <- data.frame(
  event = c("Iwato Boom", "Izanagi Boom", "Nixon Shock", "Oil Shock", "Bubble bursts", "Banking crisis", "Financial crisis", "Earthquake and Tsunami", "COVID-19", "COVID rebound"),
  before = c(1958, 1965, 1970, 1973, 1990, 1996, 2007, 2010, 2019, 2020),
  impact = c(1960, 1968, 1971, 1974, 1993 , 1998, 2009, 2011, 2020, 2021))

Japan_Vars <- GrowthData %>%
  filter(country == "Japan") %>%
  arrange(Year) %>%
  transmute(Year,
            "GDP per person Growth (%)" = growth_gdp_pc,
            "real GDP growth (%)" = 100 *(realGDP / lag(realGDP) - 1),
            "Population growth (%)" = growth_pop,
            "employment growth (%)" = 100 *(employment / lag(employment) -1),
            "capital stock growth (%)" = 100 * (capitalstock / lag(capitalstock) - 1),
            "investment share (%)" = 100 * investment,
            "depreciation rate (%)" = 100 * depreciation,
            "human capital growth (%)" = 100 * (humanCapital / lag(humanCapital) - 1),
            "TFP growth (%)" = 100 * (TotalFactorProductivity / lag(TotalFactorProductivity) - 1),
            "inflation (%)" = inflation) %>%
  pivot_longer(-Year, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = unique(variable)))

Event_Table <- Event_Years %>%
  left_join(Japan_Vars, by = c("before" = "Year")) %>%
  rename(value_before = value) %>%
  left_join(Japan_Vars, by = c("impact" = "Year", "variable")) %>%
  rename(value_impact = value) %>%
  mutate(change = value_impact - value_before,
         event = factor(event, levels = Event_Years$event))

Event_Wide <- Event_Table %>%
  mutate(cell = paste0(round(value_before, 1), " -> ", round(value_impact, 1))) %>%
  select(variable, event, cell) %>%
  pivot_wider(names_from = event, values_from = cell)
Event_Wide


#Part 4: OECD Countries: Determinants of Growth                        
OECD_codes <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST", "FIN", "FRA", "DEU",
                "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD",
                "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

OECD_data <- pwt %>%
  filter(countrycode %in% OECD_codes, year >= 1994) %>%
         arrange(country, year) %>%
         group_by(country) %>%
         mutate(gdp_pc = rgdpna / pop,
                growth_gdp_pc = 100 * (gdp_pc / lag(gdp_pc) - 1),
                growth_pop = 100 * (pop / lag(pop) - 1),
                growth_tfp = 100 * (rtfpna / lag(rtfpna) - 1)) %>%
           ungroup() %>%
           filter(year >= 1995)

         
   #part 4 step 2: each row is its seperate country with the long-run averages      
OECD_Averages <- OECD_data %>%
  group_by(country, countrycode) %>%
  summarize(avg_growth = mean(growth_gdp_pc),  #average growth of GDP per person (%)
            start_income = log(first(gdp_pc)), #log GDP per person in 1995
            avg_investment = 100 * mean(csh_i),#investment share of GDP (%)
            avg_pop_growth = mean(growth_pop), #population growth (%)
            avg_human_cap = mean(hc),          #human capital index
            avg_tfp_growth = mean(growth_tfp), # TFP growth (%)
            .groups = "drop")
OECD_Averages

#Part 4 step 3: Scatterplot of average growth against each variable
OECD_Long <- OECD_Averages %>%
  pivot_longer(-c(country, countrycode, avg_growth), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = c("start_income", "avg_investment", "avg_pop_growth", "avg_human_cap", "avg_tfp_growth"),
                           labels = c("Log GDP per person in 1995", "Investment share of GDP (%)", "Population growth (%)", "Human capital index", "TFP growth (%)")))


OECD_ScatterChart <- ggplot(OECD_Long, aes(x = value, y = avg_growth)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "grey40", linewidth = 0.6) + #this line gets the best fit line
  geom_point(aes(colour = countrycode == "JPN"), size = 1.8, show.legend = FALSE) +
  geom_text(aes(label = countrycode), size = 2.2, vjust = -0.7, colour = "grey30") +
  scale_colour_manual(values = c("FALSE" = "#619CFF", "TRUE" = "#00BA38")) +
  facet_wrap(~ variable, ncol = 3, scales = "free_x") +
  labs(title = "OECD countries: average growth of GDP per person vs. each variable (1995-2023)",
       subtitle = "One point per country. Line = Best fit. Japan in green.",
       y = "Average growth of GDP per person (%)", x = NULL,
       caption = "Source: Penn World Table 11.0") +
  theme_minimal()
OECD_ScatterChart

ggsave("OECD_ScatterChart.png", OECD_ScatterChart, width = 12, height = 7, bg = "white")


#Part 4 step 4: correlation matrix
OECD_Correlations <- cor(OECD_Averages %>% select(-country, -countrycode))
round(OECD_Correlations, 2)


#The following lines create Tables so I can copy and paste them into the word document

country_order <- c("Japan", "Argentina", "Rwanda")

Table1_Growth <- GrowthSummary %>%
  arrange(factor(country, levels = country_order)) %>%
  transmute("Country" = country,
            "Average Growth (%)" = sprintf("%.2f", avg_growth),
            "Volatility (standard deviation)" = sprintf("%.2f", volatility_sd))

Table2_Country <- Part2_Summary %>%
  arrange(factor(country, levels = country_order)) %>%
  transmute("Country" = country,
            "GDP per person, 2021 US$ (1961 -> 2023)" = paste(prettyNum(round(gdp_pc_1961), big.mark = ","), "->", prettyNum(round(gdp_pc_2023), big.mark = ",")),
            "Investment share of GDP (%)" = sprintf("%.1f", avg_investment),
            "Population growth (%)" = sprintf("%.2f", avg_pop_growth),
            "K/Y (1961 -> 2023)" = paste(sprintf("%.1f", k_y_1961), "->", sprintf("%.1f", k_y_2023)),
            "Human capital index (1961 -> 2023)" = paste(sprintf("%.2f", hc_1961), "->", sprintf("%.2f", hc_2023)),
            "TFP index, 2021 = 1 (1961 -> 2023, Rwanda starts 1964)" = paste(sprintf("%.2f", tfp_start), "->", sprintf("%.2f", tfp_2023)))

Table3_Events <- Event_Table %>%
  mutate(cell = ifelse(is.na(value_before) | is.na(value_impact), "n/a",
                       paste(sprintf("%.1f", value_before), "->", sprintf("%.1f", value_impact))),
         cell = gsub("-0.0", "0.0", cell, fixed = TRUE),
         event = paste(event, before, "->", impact),
         variable = sub("^(.)", "\\U\\1", sub("person Growth", "person growth", as.character(variable)), perl = TRUE)) %>%
  select(variable, event, cell) %>%
  pivot_wider(names_from = event, values_from = cell) %>%
  rename("Variable" = variable)

Table4_Accounting <- Japan_Growth_Accounting %>%
  transmute("Period" = as.character(period),
            "Total" = sprintf("%.2f", total),
            "From capital (K/Y)" = sprintf("%.2f", from_capital),
            "From human capital" = sprintf("%.2f", from_human_capital),
            "From workers per person" = sprintf("%.2f", from_workers_per_person),
            "From TFP" = sprintf("%.2f", from_TFP))

Table5_Correlations <- data.frame(
  "Variable" = c("TFP growth", "Log GDP per person in 1995", "Population growth", "Human capital index", "Investment share of GDP"),
  "Correlation with average growth" = sprintf("%.2f", OECD_Correlations[c("avg_tfp_growth", "start_income", "avg_pop_growth", "avg_human_cap", "avg_investment"), "avg_growth"]),
  check.names = FALSE)


Table1_Growth
Table2_Country
Table3_Events
Table4_Accounting
Table5_Correlations


write.csv(Table1_Growth, "Table1_Growth.csv", row.names = FALSE)
write.csv(Table2_Country, "Table2_Country.csv", row.names = FALSE)
write.csv(Table3_Events, "Table3_Events.csv", row.names = FALSE)
write.csv(Table4_Accounting, "Table4_Accounting.csv", row.names = FALSE)
write.csv(Table5_Correlations, "Table5_Correlations.csv", row.names = FALSE)
