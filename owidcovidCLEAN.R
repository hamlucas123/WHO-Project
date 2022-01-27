library(readr)
library(readxl)
library(dplyr)
library(lubridate, warn.conflicts = FALSE)

owid <- read_excel("data/owid-covid-data.xlsx")
euro <- read_csv("data/euro_count_dict.csv")

# Filter by European country
owid <- owid %>%
  filter(iso_code %in% euro$ISO3)

# Filter by last date in 2021
owid <- owid[c("location",
               "date",
               "total_vaccinations",
               "people_vaccinated",
               "people_fully_vaccinated",
               "total_boosters",
               "new_vaccinations",
               "new_vaccinations_smoothed",
               "total_vaccinations_per_hundred",
               "people_vaccinated_per_hundred",
               "people_fully_vaccinated_per_hundred",
               "total_boosters_per_hundred",
               "new_vaccinations_smoothed_per_million",
               "new_people_vaccinated_smoothed",
               "new_people_vaccinated_smoothed_per_hundred",
               "population",
               "population_density",
               "aged_65_older")]
# Make a copy
owid2 <- owid

# owid_fix <- owid %>% fill(people_fully_vaccinated, .direction = "up")
owid <- owid %>% group_by(location) %>% fill(c(total_vaccinations:new_vaccinations), .direction="up")

owid <- owid %>% group_by(location) %>% fill(c(total_vaccinations_per_hundred:total_boosters_per_hundred), .direction="up")


# See latest entries
owid2 <- owid2 %>% 
  group_by(location) %>%
  filter(date == max(date))

# Set date for last day of year 2021
owid3 <- owid %>% filter(date == "2021-12-31")

# Add on missing Turkmenistan data 
# Turkmenistan's latest data is 2021-08-29
owid2 <- owid2 %>% filter(location == "Turkmenistan")
dfowid <- rbind(owid3, owid2)
owid_vaccines <- dfowid
rm(dfowid, euro, owid, owid2, owid3)
# rm(list=setdiff(ls(), "owid_vaccines"))
