library(readr)
library(readxl)
library(dplyr)
library(lubridate, warn.conflicts = FALSE)

owid <- read_excel("data/owid-covid-data.xlsx")
euro <- read_csv("data/euro_count_dict.csv")

# Filter by European country
owid <- owid %>%
  filter(iso_code %in% euro$ISO3)

# Filter by last date in 2021 (see Akshay's work)
str(owid)
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

owid2 <- owid
#  filter(date == "2021-12-31")
#owid2 <- owid2 %>% 
#  group_by(location) %>%
#  filter(date == max(date))

owid2 <- owid2 %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

myfunc <- function(x,y){owid2[owid2$date >= x & owid2$date <= y,]}

DATE1 <- as.Date("2021-10-01")
DATE2 <- as.Date("2021-12-31")

Test <- myfunc(DATE1,DATE2)    

Test %>% filter(location == 'Turkmenistan')
