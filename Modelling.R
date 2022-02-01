merged_data <- read.csv("data/merged_data.csv")

#Load required packages 

library(tidyverse)
library(dplyr)
library(corrplot)
library(fabricatr)
library(mctest)
library(vortexR)

#Remove the two countries that are not in the WHO EU region
who_eu_53_countries <- merged_data %>% filter(!(Country.x %in% c("Kosovo","Liechtenstein")))
rm(merged_data)

#Exploring the dataset
summary(who_eu_53_countries) 

#There are no countries with missing vaccination data (total population vaccinated)
#Countries for whom data was collected outside of the EU CDC website lack data for percent_fully_vaccinated_total_pop
#Calculating them for the 24 countries missing them

#For countries that lack the EU CDC percent_fully_vaccinated_total_pop statistic for the general population 
vaccination_stats_missing_in_eu_cdc <- 
  who_eu_53_countries %>% filter(is.na(percent_fully_vaccinated_total_pop)) %>% 
  mutate(percent_fully_vaccinated_total_pop = people_fully_vaccinated / population) %>% 
  select(Country.x,percent_fully_vaccinated_total_pop)

#For countries that have the EU CDC percent_fully_vaccinated_total_pop statistic for the general population 
vaccination_stats_in_eu <- who_eu_53_countries %>% 
  filter(!is.na(who_eu_53_countries$percent_fully_vaccinated_total_pop)) %>% 
  select(Country.x,percent_fully_vaccinated_total_pop)

combined_eu_and_not <- rbind(vaccination_stats_in_eu,vaccination_stats_missing_in_eu_cdc)

who_eu_53_countries <- inner_join(who_eu_53_countries,combined_eu_and_not, by = 'Country.x')
who_eu_53_countries <- who_eu_53_countries %>%
  select(-c(percent_fully_vaccinated_total_pop.x)) %>% 
  rename(percent_fully_vaccinated_total_pop = percent_fully_vaccinated_total_pop.y,
         GDP_per_cap = GDP.per.capita..current.US..,
         PHC_1.9 = Poverty.head.count.ratio.at..1.90.a.day....of.population.,
         PHC_3.2 = Poverty.head.count.ratio.at..3.20.a.day....of.population.,
         PHC_5.5 = Poverty.headcount.ratio.at..5.50.a.day....of.population.,
         GINI = Gini.index..World.Bank.estimate.,
         Literacy = Adult.Literacy.rate....of.people.ages.15.and.above.,
         Edu_primary = Education..at.least.completed.primary..population.25..years..total......cumulative.,
         Edu_secondary = Education..at.least.completed.upper.secondary..population.25...total......cumulative.,
         Edu_bachelor = Education..at.least.Bachelor.s.or.equivalent..population.25...total......cumulative.,
         Health_spending_per_cap = Health.expenditure.per.capita..current.US...,
         Hosp_beds_per_1000 = Hospital.beds..per.1.000.people.,
         Nurses_per_1000 = Nurses.and.midwives..per.1.000.people.,
         Docs_per_1000 = Physicians..per.1.000.people.,
         GII = Gender.Inequality.Index..GII.,
         GPI = Gender.Parity.Index..GPI.,
         Ethnic_Frac = ethnicFractionalization,
         Linguist_Frac = linguisticFractionalization,
         Religi_Frac = religiousFractionalization,
         Refugee = Refugee.population.by.country.or.territory.of.asylum,
         Gov_Effective = Government.Effectiveness.Index.Rank,
         CPI = CPI.score.2020..Corruption.Index.,
         DTP = DTP..Child.Vax.,
         Measles = MEASLES..Child.Vax.,
         Flu = Flu.Vax.Value)

#Descriptive statistics
#Group dataset into countries as per their fully_vaccinated_total_population percentages
#as four quartiles

who_eu_53_countries['outcome_quintiles'] <- split_quantile(who_eu_53_countries$percent_fully_vaccinated_total_pop,5)

descriptives <- (who_eu_53_countries[,-c(1,2)] %>% group_by(outcome_quintiles) %>% summarise_all(.funs = mean, na.rm = T))
#View(who_eu_53_countries[,-c(1,2)] %>% group_by(outcome_quintiles) %>% summarise_all(.funs = sd, na.rm = T))

#Complete covariate list has 26 
full_covariate_list = c("GDP_per_cap",
                        "PHC_1.9",
                        "PHC_3.2",
                        "PHC_5.5",
                        "GINI",
                        "Literacy",
                        "Edu_primary",
                        "Edu_secondary",
                        "Edu_bachelor",
                        "Health_spending_per_cap",
                        "Hosp_beds_per_1000",
                        "Nurses_per_1000",
                        "Docs_per_1000",
                        "GII",
                        "GPI",
                        "Ethnic_Frac",
                        "Linguist_Frac",
                        "Religi_Frac",
                        "Refugee",
                        "Gov_Effective",
                        "stringency_index",
                        "CPI",
                        "DTP",
                        "Measles",
                        "Flu",
                        "available_doses")

#summary of the who_eu_53_countries dataframe 
summary(who_eu_53_countries[,full_covariate_list])

#5 covariates - Flu, Measles, DPT, available_doses, education_bachelor are missing in more than 10%
#of all countries ; removing these variables from consideration 

#Univariable analyses among the remaining 21 covariates #beta regression models
summary(betareg(percent_fully_vaccinated_total_pop ~ GDP_per_cap, data = model_data))
summary(betareg(percent_fully_vaccinated_total_pop ~ PHC_1.9, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ PHC_3.2, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ PHC_5.5, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ GINI, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Literacy, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Edu_primary, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Edu_secondary, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Health_spending_per_cap, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Hosp_beds_per_1000, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Nurses_per_1000, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Docs_per_1000, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ GII, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ GPI, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Ethnic_Frac, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Linguist_Frac, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Religi_Frac, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Refugee, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ Gov_Effective, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ stringency_index, data = who_eu_53_countries))
summary(betareg(percent_fully_vaccinated_total_pop ~ CPI, data = who_eu_53_countries))

#Removing all those covariates that do not show trends/associations on descriptives/univariable analyses 
#covariate_list_trimmed has 13
covariate_list_trimmed = c("GDP_per_cap",
                           "PHC_3.2",
                           "PHC_5.5",
                           "Edu_secondary",
                           "Health_spending_per_cap",
                           "Nurses_per_1000",
                           "Docs_per_1000",
                           "GII",
                           "Ethnic_Frac",
                           "Linguist_Frac",
                           "Religi_Frac",
                           "Gov_Effective",
                           "CPI")

# Checking for multicollinearity among the remaining 13 covariates in covariate_list_trimmed

corr_matrix <- cor(who_eu_53_countries[,covariate_list_trimmed], method = "pearson", use = "complete.obs")
corrplot(corr_matrix, type = "upper", order = "hclust",   tl.cex = 0.7,
         tl.col = "black", tl.srt = 45)


##############
#target_group_statistics <- who_eu_53_countries %>% 
 # select(Country.x,
  #       percent_fully_vaccinated_hcw,
   #      percent_fully_vaccinated_ltcf,
    #     percent_fully_vaccinated_above60)
#
#summary(target_group_statistics)
################

model_data <- who_eu_53_countries %>% select(all_of(covariate_list_trimmed),percent_fully_vaccinated_total_pop)

#x <- model.matrix(percent_fully_vaccinated_total_pop ~ .,
           #        model.frame(~ ., model_data, na.action=na.pass))[,-1]
#y <- model_data$percent_fully_vaccinated_total_pop

betareg_full_model <- betareg(percent_fully_vaccinated_total_pop ~ ., model_data)
mctest(betareg_full_model, type="i", corr=TRUE) ;

fit_regression()