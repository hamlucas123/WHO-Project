library(readxl)
data_corp <- read_xlsx("~/LSHTM_DC/corruption-indicator.xlsx", 1)
data_corp <- data_corp %>%
  filter(ISO3 %in% euro_count_dict$`Country Code (WorldBank)`) %>%
  select(c("Country", "ISO3", "CPI score 2020", "Standard error", "Lower CI", "Upper CI"))
colnames(data_corp) = data_corp[2,] %>%
tail(data_corp,-2)
