library(readxl)
pwt <- read_excel("data for Macro Project 1/pwt110.xlsx", sheet = "Data")
writeLines(c(".Rproj.user/", ".Rhistory", ".RData", ".Ruserdata",
             "data for Macro Project 1/"), ".gitignore")
