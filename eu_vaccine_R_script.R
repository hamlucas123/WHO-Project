eu_vaccines_raw <- read.csv("/Users/teejay/Documents/GitHub/WHO-Project/data/eu_vaccine.csv")

#Load required packages
library('dplyr') 

#We need observations only upto end of year 2021; Data for upto W03 in 2022 is given in the file
#Filtering unnecessary observations out
eu_vaccines <- (eu_vaccines_raw %>% filter(YearWeekISO != '2022-W03' &
                                           YearWeekISO != '2022-W02' &
                                           YearWeekISO != '2022-W01')) %>% 
#Some countries in addition to country level statistics have also given regional values
#We only want those values that are at the country level
                filter(ReportingCountry == as.character(Region))

#We want data for entire general population with cumulative doses received, exported, 
#first dose, second dose, doseadditional1, and unknowndoses given as of 2021-W52

#TargetGroup == 'ALL' includes vaccination statistics for all adults >18 
#We want vaccination statistics for entire population of the country

countries_reporting_age_below_18 <- c('AT', 'BE', 'BG', 'CZ', 'EE', 'EL', 'ES', 'FI',
                                      'FR', 'HR', 'HU', 'IE', 'IT', 'LT', 'LV', 'MT', 'NL', 
                                      'NO', 'RO', 'SI')

#For the countries that report 'Age<18'
eu_vaccines_cum_all_1 <- eu_vaccines %>%                
                       filter(ReportingCountry %in% countries_reporting_age_below_18) %>%
                       filter(TargetGroup %in% c('ALL','Age<18')) %>% 
                       group_by(ReportingCountry) %>%
                       summarise_at(.vars = vars(NumberDosesReceived,NumberDosesExported,
                                                 FirstDose,SecondDose,DoseAdditional1,UnknownDose),
                                    .funs = sum, na.rm = T)
                       
#For the countries that don't report 'Age<18'                       
eu_vaccines_cum_all_2 <- eu_vaccines %>%
                         filter(!(ReportingCountry %in% countries_reporting_age_below_18)) %>%
                         filter(TargetGroup %in% c('ALL','Age0_4','Age5_9','Age10_14','Age15_17')) %>%
                         group_by(ReportingCountry) %>%
                         summarise_at(.vars = vars(NumberDosesReceived,NumberDosesExported,
                                                  FirstDose,SecondDose,DoseAdditional1,UnknownDose),
                                      .funs = sum, na.rm = T)

#Combining the eu_vaccines_cum_all_1 and eu_vaccines_cum_all_2
eu_vaccines_cum_all <- bind_rows(eu_vaccines_cum_all_1,eu_vaccines_cum_all_2)

#JJ is a single dose vaccine; Some countries have included it as first dose, some as second, some for both
#Individuals who've received JJ (and marked as first dose) should count towards fully vaccinated group 

#Dataframe holding cumulative doses of JJ vaccine given as of 2021-W52 for countries that mention Age<18 
JJ_doses_cum_all_1 <- eu_vaccines %>% 
                    filter(Vaccine == 'JANSS') %>%
                    filter(ReportingCountry %in% countries_reporting_age_below_18) %>%
                    filter(TargetGroup %in% c('ALL','Age<18')) %>% 
                    group_by(ReportingCountry) %>%
                    summarise(FirstDoseJJ = sum(FirstDose),na.rm = T)

#Dataframe holding cumulative doses of JJ vaccine given as of 2021-W52 for countries that mention Age<18 
JJ_doses_cum_all_2 <- eu_vaccines %>% 
                      filter(Vaccine == 'JANSS') %>%
                      filter(!(ReportingCountry %in% countries_reporting_age_below_18)) %>%
                      filter(TargetGroup %in% c('ALL','Age0_4','Age5_9','Age10_14','Age15_17')) %>%
                      group_by(ReportingCountry) %>%
                      summarise(FirstDoseJJ = sum(FirstDose),na.rm = T)

#Combining the JJ_doses_cum_all_1 and JJ_doses_cum_all_2 
JJ_doses_cum_all <- bind_rows(JJ_doses_cum_all_1,JJ_doses_cum_all_2)

#Merging JJ_doses_cum_all with eu_vaccines_cum_all 
#Only 28 of the 30 countries have used JJ; Finald(FI) and Sweden (SE) have not
#FI however provides a Vaccine == 'JANSS' and mentions doses as 0 ; SE does not ; Hence left join 
eu_vaccines_cum_all <- left_join(eu_vaccines_cum_all, JJ_doses_cum_all, by = "ReportingCountry")

#Dataframe holding populations of the 30 EU countries in the eu_vaccines dataset. 
eu_populations <- eu_vaccines %>% filter(TargetGroup == 'ALL') %>%
                  group_by(ReportingCountry) %>% 
                  #There are exactly 30 unique values for populations
                  summarise(Population = max(Population)) 

#Merging the population values to the eu_vaccines_cum_all to calculate percentages
eu_vaccines_cum_all <- inner_join(eu_vaccines_cum_all, eu_populations, by = "ReportingCountry")

#Calculating %s and other columns for eu_vaccines_cum_all 
eu_vaccines_cum_all <- mutate(eu_vaccines_cum_all,
                              NumberDosesAvailable = NumberDosesReceived - NumberDosesExported,
                              Percent_atleast_onedose = FirstDose / Population,
                              #People vaccinated with JJ count towards fully vaccinated
                              Percent_fully_vaccinated = (SecondDose + FirstDoseJJ) / Population,
                              Percent_boosted = DoseAdditional1 / Population)

#Sweden is missing the Percent_fully_vaccinated variable because it is missing the FirstDoseJJ variable
#as sweden didn't use JJ vaccines and has not mentioned JJ in the target group 
#Calculating this value for sweden 
eu_vaccines_cum_all[eu_vaccines_cum_all$ReportingCountry == 'SE', "Percent_fully_vaccinated"] <-
eu_vaccines_cum_all[eu_vaccines_cum_all$ReportingCountry == 'SE', "SecondDose"] / 
eu_vaccines_cum_all[eu_vaccines_cum_all$ReportingCountry == 'SE', "Population"]

#Number of doses available for Malta ('MT') is negative as they had values only number of doses exported but 
#not for the number of doses received (the calculated Number of doses was -341380)
#Replacing this value with NA

eu_vaccines_cum_all[eu_vaccines_cum_all$ReportingCountry == 'MT', "NumberDosesAvailable"] <- NA

#I have checked the % of fully vaccinated people with the EU CDC interactive dashboard of COVID vaccinations
#and found the values calculated in eu_vaccines_cum_all to be matching satisfactorily 

##For vaccination statistics in HCWs:

#Dataframe holding populations of the HCWs among the EU countries that mention it
eu_hcw_populations <- eu_vaccines %>% filter(TargetGroup == 'HCW') %>%
                      group_by(ReportingCountry) %>%
                      #Each of the countries have only one unique value of the denominator for the HCW group 
                      summarise(hcw_population = max(Denominator)) 

#We want data for HCW population with cumulative doses received, exported, 
#first dose, second dose, doseadditional1, and unknowndoses given as of 2021-W52
eu_vaccines_cum_hcw <- eu_vaccines %>% filter(TargetGroup == 'HCW') %>%
                      group_by(ReportingCountry) %>%
                      summarise_at(.vars = vars(FirstDose,SecondDose,DoseAdditional1,UnknownDose),
                                .funs = sum, na.rm = T)

eu_vaccines_cum_hcw <- inner_join(eu_vaccines_cum_hcw, eu_hcw_populations, by = "ReportingCountry")

#Dataframe holding cumulative first doses of JJ vaccine given to HCWs as of 2021-W52
JJ_doses_cum_hcw <- eu_vaccines %>% 
  filter(Vaccine == 'JANSS') %>%
  filter(TargetGroup == 'HCW') %>% 
  group_by(ReportingCountry) %>%
  summarise(FirstDoseJJ_hcw = sum(FirstDose),na.rm = T)

#Adding a column on first doses of JJ vaccine given to HCW to the eu_vaccines_cum_hcw df
eu_vaccines_cum_hcw <- left_join(eu_vaccines_cum_hcw, JJ_doses_cum_hcw, by = "ReportingCountry")

#Calculating %s for eu_vaccines_cum_hcw 
eu_vaccines_cum_hcw <- mutate(eu_vaccines_cum_hcw,
                              percent_atleast_onedose_hcw = FirstDose / hcw_population,
                              #People vaccinated with JJ count towards fully vaccinated
                              percent_fully_vaccinated_hcw = (SecondDose + FirstDoseJJ_hcw) / hcw_population,
                              percent_boosted_hcw = DoseAdditional1 / hcw_population)

#Sweden is missing the Percent_fully_vaccinated variable because it is missing the FirstDoseJJ variable
#as sweden didn't use JJ vaccines and has not mentioned JJ in the target group 
#Calculating this value for sweden 
eu_vaccines_cum_hcw[eu_vaccines_cum_hcw$ReportingCountry == 'SE', "percent_fully_vaccinated_hcw"] <-
  eu_vaccines_cum_hcw[eu_vaccines_cum_hcw$ReportingCountry == 'SE', "SecondDose"] / 
  eu_vaccines_cum_hcw[eu_vaccines_cum_hcw$ReportingCountry == 'SE', "hcw_population"]

#I have checked the % of fully vaccinated HCWs with the EU CDC interactive dashboard of COVID vaccinations
#and found the values calculated in eu_vaccines_cum_hcw to be matching satisfactorily, except for 4 countries
#CZ,HU,IE,IS -- for these the calculated value exceeds 100%; All these countries do have 100% HCW vaccination
#It is likely that the measure of hcw_population is old, or that there were additional people deemed HCW who 
#are otherwise not in healthcare, e.g. volunteers, HCW who came back from retirement for COVID.
#Changing the values to 100% for those that exceed 100%. 

eu_vaccines_cum_hcw <- eu_vaccines_cum_hcw %>% 
                       mutate(percent_atleast_onedose_hcw = replace(percent_atleast_onedose_hcw,
                                                                    percent_atleast_onedose_hcw > 1,
                                                                    1),
                              percent_fully_vaccinated_hcw = replace(percent_fully_vaccinated_hcw,
                                                                     percent_fully_vaccinated_hcw > 1,
                                                                    1))

#Dropping unnecessary columns from the eu_vaccines_cum_hcw dataset and renaming columns to allow for merging
#with eu_vaccines_cum_all without conflicts
eu_vaccines_cum_hcw <- select(eu_vaccines_cum_hcw,-c("FirstDose","SecondDose","DoseAdditional1","na.rm",
                                                     "FirstDoseJJ_hcw"))

eu_vaccines_cum_hcw <- rename(eu_vaccines_cum_hcw, unknown_dose_hcw = UnknownDose)
                         