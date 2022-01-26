library(readxl)
data_corp <- read_xlsx("data/corruption-indicator.xlsx", 1, skip=1)
euro_count <- read.csv("data/euro_count_dict.csv")

# missing Andorra, Monaco, San Marino

data_corp <- data_corp %>%
  filter(ISO3 %in% euro_count$ISO3) %>%
  select(c("Country", "ISO3", "CPI score 2020", "Standard error", "Lower CI", "Upper CI"))

euro_count$ISO3[!(euro_count$ISO3 %in% data_corp$ISO3)]

corrupt <- data_corp
rm(data_corp, euro_count)