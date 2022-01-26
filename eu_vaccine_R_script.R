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
                       filter(TargetGroup %in% c('ALL','Age<18','AgeUNK')) %>% 
                       group_by(ReportingCountry) %>%
                       summarise_at(.vars = vars(FirstDose,SecondDose,DoseAdditional1,UnknownDose),
                                    .funs = sum, na.rm = T)
                       
#For the countries that don't report 'Age<18'                       
eu_vaccines_cum_all_2 <- eu_vaccines %>%
                         filter(!(ReportingCountry %in% countries_reporting_age_below_18)) %>%
                         filter(TargetGroup %in% c('ALL','Age0_4','Age5_9','Age10_14','Age15_17','AgeUNK')) %>%
                         group_by(ReportingCountry) %>%
                         summarise_at(.vars = vars(FirstDose,SecondDose,DoseAdditional1,UnknownDose),
                                      .funs = sum, na.rm = T)

#Combining the eu_vaccines_cum_all_1 and eu_vaccines_cum_all_2
eu_vaccines_cum_all <- bind_rows(eu_vaccines_cum_all_1,eu_vaccines_cum_all_2)

#JJ is a single dose vaccine; Some countries have included it as first dose, some as second, some for both
#Individuals who've received JJ (and marked as first dose) should count towards fully vaccinated group 

#Dataframe holding cumulative doses of JJ vaccine given as of 2021-W52 for countries that mention Age<18 
JJ_doses_cum_all_1 <- eu_vaccines %>% 
                    filter(Vaccine == 'JANSS') %>%
                    filter(ReportingCountry %in% countries_reporting_age_below_18) %>%
                    filter(TargetGroup %in% c('ALL','Age<18','AgeUNK')) %>% 
                    group_by(ReportingCountry) %>%
                    summarise(FirstDoseJJ = sum(FirstDose),na.rm = T)

#Dataframe holding cumulative doses of JJ vaccine given as of 2021-W52 for countries that mention Age<18 
JJ_doses_cum_all_2 <- eu_vaccines %>% 
                      filter(Vaccine == 'JANSS') %>%
                      filter(!(ReportingCountry %in% countries_reporting_age_below_18)) %>%
                      filter(TargetGroup %in% c('ALL','Age0_4','Age5_9','Age10_14','Age15_17','AgeUNK')) %>%
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

##For vaccination statistics in LTCF:

#Dataframe holding populations of the LTCF residents among the EU countries that mention it
eu_ltcf_populations <- eu_vaccines %>% filter(TargetGroup == 'LTCF') %>%
  group_by(ReportingCountry) %>%
  #Each of the countries have only one unique value of the denominator for the LTCF group 
  summarise(ltcf_population = max(Denominator)) 

#We want data for LTCF residents with cumulative doses received, exported, 
#first dose, second dose, doseadditional1, and unknowndoses given as of 2021-W52

eu_vaccines_cum_ltcf <- eu_vaccines %>% filter(TargetGroup == 'LTCF') %>%
  group_by(ReportingCountry) %>%
  summarise_at(.vars = vars(FirstDose,SecondDose,DoseAdditional1,UnknownDose),
               .funs = sum, na.rm = T)

eu_vaccines_cum_ltcf <- inner_join(eu_vaccines_cum_ltcf, eu_ltcf_populations, by = "ReportingCountry")

#Dataframe holding cumulative first doses of JJ vaccine given to LTCF residents as of 2021-W52
JJ_doses_cum_ltcf <- eu_vaccines %>% 
  filter(Vaccine == 'JANSS') %>%
  filter(TargetGroup == 'LTCF') %>% 
  group_by(ReportingCountry) %>%
  summarise(FirstDoseJJ_ltcf = sum(FirstDose),na.rm = T)

#Adding a column on first doses of JJ vaccine given to LTCF residents to the eu_vaccines_cum_ltcf df
eu_vaccines_cum_ltcf <- left_join(eu_vaccines_cum_ltcf, JJ_doses_cum_ltcf, by = "ReportingCountry")

#Calculating %s for eu_vaccines_cum_ltcf
eu_vaccines_cum_ltcf <- mutate(eu_vaccines_cum_ltcf,
                              percent_atleast_onedose_ltcf = FirstDose / ltcf_population,
                              #People vaccinated with JJ count towards fully vaccinated
                              percent_fully_vaccinated_ltcf = (SecondDose + FirstDoseJJ_ltcf) / ltcf_population,
                              percent_boosted_ltcf = DoseAdditional1 / ltcf_population)

#Sweden is missing the Percent_fully_vaccinated variable because it is missing the FirstDoseJJ variable
#as sweden didn't use JJ vaccines and has not mentioned JJ in the target group 
#Calculating this value for sweden 
eu_vaccines_cum_ltcf[eu_vaccines_cum_ltcf$ReportingCountry == 'SE', "percent_fully_vaccinated_ltcf"] <-
  eu_vaccines_cum_ltcf[eu_vaccines_cum_ltcf$ReportingCountry == 'SE', "SecondDose"] / 
  eu_vaccines_cum_ltcf[eu_vaccines_cum_ltcf$ReportingCountry == 'SE', "ltcf_population"]

#I have checked the % of fully vaccinated LTCFs and % boosters with the EU CDC interactive dashboard of COVID 
# vaccines & found the values calculated in eu_vaccines_cum_ltcf to be matching satisfactorily, except for 
#Spain (double vac %) and Malta (Booster %). For these, the calculated value exceeds 100%; All these countries 
#do have 100% uptake for these variables 

#Changing the values to 100% for those that exceed 100%. 

eu_vaccines_cum_ltcf <- eu_vaccines_cum_ltcf %>% 
                        mutate(percent_atleast_onedose_ltcf = replace(percent_atleast_onedose_ltcf,
                                                                     percent_atleast_onedose_ltcf > 1,
                                                                     1),
                               percent_fully_vaccinated_ltcf = replace(percent_fully_vaccinated_ltcf,
                                                                      percent_fully_vaccinated_ltcf > 1,
                                                                      1),
                               percent_boosted_ltcf = replace(percent_boosted_ltcf,
                                                              percent_boosted_ltcf > 1,
                                                                      1))

#Dropping unnecessary columns from the eu_vaccines_cum_ltcf dataset and renaming columns to allow for merging
#with eu_vaccines_cum_all without conflicts
eu_vaccines_cum_ltcf <- select(eu_vaccines_cum_ltcf,-c("FirstDose","SecondDose","DoseAdditional1","na.rm",
                                                     "FirstDoseJJ_ltcf"))

eu_vaccines_cum_ltcf <- rename(eu_vaccines_cum_ltcf, unknown_dose_ltcf = UnknownDose)

##For vaccination statistics in those aged >60:

#Dataframe holding populations of the individuals aged > 60 among the EU countries that mention it
eu_above60_populations <- eu_vaccines %>% filter(TargetGroup %in% c("Age60_69","Age70_79","Age80+" )) %>%
  group_by(ReportingCountry, TargetGroup) %>%
  #Each of the countries have only one unique value of the denominator for each of the target groups 
  summarise(above60_population = (max(Denominator))) %>% 
  group_by(ReportingCountry) %>%
  summarise(above60_population = sum(above60_population))

#We want data for individuals above 60 residents with cumulative doses received, exported, 
#first dose, second dose, doseadditional1, and unknowndoses given as of 2021-W52

eu_vaccines_cum_above60 <- eu_vaccines %>% filter(TargetGroup %in% c("Age60_69","Age70_79","Age80+")) %>%
  group_by(ReportingCountry) %>%
  summarise_at(.vars = vars(FirstDose,SecondDose,DoseAdditional1,UnknownDose),
               .funs = sum, na.rm = T)

eu_vaccines_cum_above60 <- inner_join(eu_vaccines_cum_above60, eu_above60_populations, by = "ReportingCountry")

#Dataframe holding cumulative first doses of JJ vaccine given to above 60 people as of 2021-W52
JJ_doses_cum_above60 <- eu_vaccines %>% 
  filter(Vaccine == 'JANSS') %>%
  filter(TargetGroup %in% c("Age60_69","Age70_79","Age80+" )) %>% 
  group_by(ReportingCountry) %>%
  summarise(FirstDoseJJ_above60 = sum(FirstDose),na.rm = T)

#Adding a column on first doses of JJ vaccine given to above 60 people to the eu_vaccines_cum_above60 df
eu_vaccines_cum_above60 <- left_join(eu_vaccines_cum_above60, JJ_doses_cum_above60, by = "ReportingCountry")

#Calculating %s for eu_vaccines_cum_above60
eu_vaccines_cum_above60 <- mutate(eu_vaccines_cum_above60,
                               percent_atleast_onedose_above60 = FirstDose / above60_population,
                               #People vaccinated with JJ count towards fully vaccinated
                               percent_fully_vaccinated_above60 = (SecondDose + FirstDoseJJ_above60) / above60_population,
                               percent_boosted_above60 = DoseAdditional1 / above60_population)

#Sweden and France are missing the Percent_fully_vaccinated variable because they lack the FirstDoseJJ variable
#as neither of these countries used JJ vaccines in the target group above 60
#Calculating this value for sweden 
eu_vaccines_cum_above60[eu_vaccines_cum_above60$ReportingCountry == 'SE', "percent_fully_vaccinated_above60"] <-
  eu_vaccines_cum_above60[eu_vaccines_cum_above60$ReportingCountry == 'SE', "SecondDose"] / 
  eu_vaccines_cum_above60[eu_vaccines_cum_above60$ReportingCountry == 'SE', "above60_population"]

eu_vaccines_cum_above60[eu_vaccines_cum_above60$ReportingCountry == 'FR', "percent_fully_vaccinated_above60"] <-
  eu_vaccines_cum_above60[eu_vaccines_cum_above60$ReportingCountry == 'FR', "SecondDose"] / 
  eu_vaccines_cum_above60[eu_vaccines_cum_above60$ReportingCountry == 'FR', "above60_population"]

#I have checked the % of fully vaccinated above 60 and % boosters above 60 with the EU CDC  dashboard of COVID
# vaccines & found the values calculated in eu_vaccines_cum_above60 to be matching satisfactorily, except for 
#IE, IS, and PT. For these, the calculated value exceeds 100%; All these countries do have 100% uptake 
#for these variables. This can be explained as census data for those above 60 are few years old

#Changing the values to 100% for those that exceed 100%. 

eu_vaccines_cum_above60 <- eu_vaccines_cum_above60 %>% 
  mutate(percent_atleast_onedose_above60 = replace(percent_atleast_onedose_above60,
                                                   percent_atleast_onedose_above60 > 1,
                                                1),
         percent_fully_vaccinated_above60 = replace(percent_fully_vaccinated_above60,
                                                    percent_fully_vaccinated_above60 > 1,
                                                 1))

#Dropping unnecessary columns from the eu_vaccines_cum_ltcf dataset and renaming columns to allow for merging
#with eu_vaccines_cum_all without conflicts
eu_vaccines_cum_above60 <- select(eu_vaccines_cum_above60,-c("FirstDose","SecondDose","DoseAdditional1","na.rm",
                                                       "FirstDoseJJ_above60"))

eu_vaccines_cum_above60 <- rename(eu_vaccines_cum_above60, unknown_dose_above60 = UnknownDose)

#Germany (DE) had not coded target groups like "Age60_69","Age70_79","Age80+" but instead has '1_Age>60'
germany_above60_stats <- eu_vaccines %>% 
                         filter(ReportingCountry == 'DE' & TargetGroup == '1_Age60+') %>%
                         summarise(FirstDose = sum(FirstDose),
                                   SecondDose = sum(SecondDose),
                                   DoseAdditional1 = sum(DoseAdditional1),
                                   above60_population = max(Denominator)) %>% 
                         mutate(percent_atleast_onedose_above60 = FirstDose / above60_population,
                                #Germany has not given JJ to above 60 year olds
                                percent_fully_vaccinated_above60 = SecondDose / above60_population,
                                percent_boosted_above60 = DoseAdditional1 / above60_population,
                                ReportingCountry = 'DE') %>%
                         select(-c(FirstDose,SecondDose,DoseAdditional1))

#Adding germany_above60_stats to eu_vaccines_cum_above60
eu_vaccines_cum_above60 <- bind_rows(eu_vaccines_cum_above60,germany_above60_stats)

#Adding data regarding vaccines cumulative vaccines received and exported as of W52 2021
vaccines_received_exported <- eu_vaccines %>% 
                              filter(TargetGroup == 'ALL') %>%
                              group_by(ReportingCountry) %>% 
                              summarise_at(.vars = vars(NumberDosesReceived,NumberDosesExported),
                                           .funs = sum, na.rm =T)

#Merging all dataframes together to a single one
eu_vaccines_clean <- inner_join(eu_vaccines_cum_all,vaccines_received_exported,by = 'ReportingCountry')

eu_vaccines_clean <- eu_vaccines_clean %>% mutate(available_doses = NumberDosesReceived - NumberDosesExported,
                                                  percent_doses_utilised = (FirstDose + SecondDose + DoseAdditional1 +
                                                                 UnknownDose)/available_doses)

#Number of doses available for Malta ('MT') is negative as they had values only number of doses exported but 
#not for the number of doses received (the calculated Number of doses was -341380)
#Replacing this value with NA

eu_vaccines_clean[eu_vaccines_clean$ReportingCountry == 'MT', c("available_doses","percent_doses_utilised")] <- NA

#Combining statistics for above 60, hcw, ltcf into eu_vaccines_clean
eu_vaccines_clean <- left_join(eu_vaccines_clean,eu_vaccines_cum_hcw, by = 'ReportingCountry')
eu_vaccines_clean <- left_join(eu_vaccines_clean,eu_vaccines_cum_ltcf, by = 'ReportingCountry')
eu_vaccines_clean <- left_join(eu_vaccines_clean,eu_vaccines_cum_above60, by = 'ReportingCountry')

#Removing unnecessary columns and renaming others
eu_vaccines_clean <- select(eu_vaccines_clean,-c("FirstDoseJJ","na.rm"))
eu_vaccines_clean <- rename(eu_vaccines_clean,
                            cum_first_doses = FirstDose,
                            cum_second_doses = SecondDose,
                            cum_additional1_doses = DoseAdditional1,
                            cum_unknown_doses = UnknownDose,
                            total_population = Population,
                            percent_atleast_onedose_total_pop = Percent_atleast_onedose,
                            percent_fully_vaccinated_total_pop = Percent_fully_vaccinated,
                            percent_boosted_total_pop = Percent_boosted,
                            total_doses_received = NumberDosesReceived,
                            total_doses_exported = NumberDosesExported, 
                            two_letter_country_code = ReportingCountry)

#Adding a full name for the ReportingCountry Code

country_names_codes <- data.frame(
                       two_letter_country_code = sort(eu_vaccines_clean$two_letter_country_code),
                       full_country_names =
                        c("Austria", "Belgium", "Bulgaria","Cyprus","Czechia","Germany","Denmark","Estonia","Greece",
                          "Spain","Finland","France","Croatia","Hungary","Ireland","Iceland","Italy",
                          "Liechtenstein", "Lithuania", "Luxembourg", "Latvia","Malta","Netherlands","Norway",
                          "Poland","Portugal","Romania","Sweden","Slovenia","Slovakia"))

eu_vaccines_clean = inner_join(eu_vaccines_clean,country_names_codes)
