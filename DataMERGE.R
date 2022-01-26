source('eu_vaccine_R_script.R')
source('owidcovidCLEAN.R')
source('worldbankCLEAN.R')
source('wgiCLEAN.R')
source('vax_fluCLEAN.R')
source('vax_childmmrCLEAN.R')
source('eth_fracCLEAN.R')
source('covid_stringencyCLEAN.R')
source('corruption_CLEAN.R')

library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

# worldbank Country Code  / vax_child LOCATION (do first)

df <- merge(worldbank, vax_child, by.x = "Country Code", 
            by.y = "LOCATION", all.x = TRUE, all.y = FALSE)

df <- df %>% 
  pivot_wider(names_from = SUBJECT, values_from = Value)

drops <- c("TIME","NA")
df <- df[ , !(names(df) %in% drops)]

colnames(df)[19] <- "DTP (Child Vax)"
colnames(df)[20] <- "MEASLES (Child Vax)"

# df Country Code / corrupt ISO3

df <- merge(df, corrupt, by.x = "Country Code", 
            by.y = "ISO3", all.x = TRUE, all.y = FALSE)

df <- df[, -c(23:25)]
df <- df[, -c(21)]
colnames(df)[21] <- "CPI score 2020 (Corruption Index)"

# df Country / eth_frac country (check missing)




# df Country Code / stringency Code


# df Country Code / vax_flu LOCATION

# df Country Code / wgi Code (check Andorra)

# eu_vaccines with euro_count_dict.csv to get 3 letter country code
# df Country Code / eu_vaccines Code

# owid_vaccines with euro_count_dict.csv to get 3 letter country code
# df Country Code / owid_vaccines Code


# total <- merge(data frameA,data frameB,by="ID")

