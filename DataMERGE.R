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

euro_count_dict <- read.csv('data/euro_count_dict.csv')

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
colnames(df)[2] <- "Country"

# df Country / eth_frac country (three missing in merge but re-added)

eth_frac[26, 1] <- "Kyrgyz Republic"
eth_frac[38, 1] <- "Russian Federation"
eth_frac[41, 1] <- "Slovak Republic"


df <- merge(df, eth_frac, by.x = "Country", 
            by.y = "country", all.x = TRUE, all.y = FALSE)

df <- df[, -c(25)]

# df Country Code / stringency Code

df <- merge(df, stringency, by.x = "Country Code", 
            by.y = "Code", all.x = TRUE, all.y = FALSE)

# df Country Code / vax_flu LOCATION

df <- merge(df, vax_flu, by.x = "Country Code", 
            by.y = "LOCATION", all.x = TRUE, all.y = FALSE)

df <- df[, -c(27)]
df <- df[, -c(25)]

colnames(df)[26] <- "Flu Vax Value"


# df Country Code / wgi Code (check Andorra)

wgi[1, 2] <- "AND"

df <- merge(df, wgi, by.x = "Country Code", 
            by.y = "Code", all.x = TRUE, all.y = FALSE)

df <- df[, -c(32:33)]

df <- df[, -c(27:29)]

colnames(df)[28] <- "Government Effectiveness Index Rank"
colnames(df)[2] <- "Country"

df$NumSrc <- NULL


# owid_vaccines with euro_count_dict.csv to get 3 letter country code
# df Country Code / owid_vaccines Code

owid_vaccines[30, 1] <- "Moldova (Republic of Moldova)"
owid_vaccines[38, 1] <- "Russian Federation"
owid_vaccines[49, 1] <- "United Kingdom of Great Britain and Northern Ireland"


dfowid <- merge(owid_vaccines, euro_count_dict, by.x = "location", 
              by.y = "Country", all.x = TRUE, all.y = FALSE)


dfowid <- dfowid[, -c(21:23)]
dfowid <- dfowid[, -c(19)]
dfowid <- dfowid[, -c(2)]

df <- merge(df, dfowid, by.x = "Country Code", 
            by.y = "ISO3", all.x = TRUE, all.y = FALSE)

df$location <- NULL

# eu_vaccines (two_letter_country_code) with euro_count_dict.csv (ISO2) to get 3 letter country code
# df Country Code / eu_vaccines Code


dfeu <- merge(eu_vaccines, euro_count_dict, by.x = "two_letter_country_code", 
              by.y = "ISO2", all.x = TRUE, all.y = FALSE)

dfeu[18, 32] <- "LIE"
dfeu[9, 32] <- "GRC"

df <- merge(df, dfeu, by.x = "Country Code", 
            by.y = "ISO3", all.x = TRUE, all.y = TRUE)

df <- df[, -c(72:76)]
df <- df[, -c(44)]

rm(list=setdiff(ls(), "df"))

# write.csv(df,"data/merged_data.csv", row.names = FALSE)




