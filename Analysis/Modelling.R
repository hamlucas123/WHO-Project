merged_data <- read.csv("data/merged_data.csv")

#Load required packages 

library(tidyverse) 
library(dplyr) # For pipes and data manipulation
library(corrplot) #Plot correlation matrix 
library(fabricatr) #Convert outcome variable to quartiles 
library(betareg) #Run beta regression 
library(mctest) #Run tests for collinearity  

#Remove the two countries that are not in the WHO EU region
who_eu_53_countries <- merged_data %>% filter(!(Country.x %in% c("Kosovo","Liechtenstein")))
rm(merged_data)

#Exploring the dataset
summary(who_eu_53_countries) 

#There are no countries with missing vaccination data (total population vaccinated)
#Countries for whom data was collected outside of the EU CDC website lack data for percent_fully_vaccinated_total_pop
#Calculating them for the 24 countries missing them from the data from the world bank 

who_eu_53_countries <- who_eu_53_countries %>%
                       mutate(proportion_fully_vax_gen_pop = coalesce(percent_fully_vaccinated_total_pop,
                                                  people_fully_vaccinated_per_hundred/100))

#proportion_fully_vax_gen_pop is the proportion of people fully vaccinated (for all 53 countries)

who_eu_53_countries <- who_eu_53_countries %>%
  select(-c(percent_fully_vaccinated_total_pop)) %>% 
  rename(GDP_per_cap = GDP.per.capita..current.US..,
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

who_eu_53_countries['outcome_quartiles'] <- split_quantile(who_eu_53_countries$proportion_fully_vax_gen_pop,4)

descriptives <- (who_eu_53_countries[,-c(1,2)] %>% group_by(outcome_quartiles) %>% summarise_all(.funs = mean, na.rm = T))

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

#5 covariates - Flu, Measles, DPT, available_doses, Edu_bachelor are missing in more than 10%
#of all countries ; removing these variables from consideration 

#Univariable analyses among the remaining 21 covariates #beta regression models

all_univariable_beta_models <- list()

for (i in full_covariate_list[!full_covariate_list %in% c('Flu','DTP','available_doses','Measles','Edu_bachelor')]){
 formula <- as.formula(paste("proportion_fully_vax_gen_pop ~",i))
 model <- summary(betareg(formula, data = who_eu_53_countries))
 all_univariable_beta_models[[paste(i,"_model", sep = '')]] <- model
}

print(all_univariable_beta_models) 
#13 covariates show strong evidence of association with outcome

#Removing all those covariates that do not show trends/associations on descriptives/univariable analyses 
#covariate_list_trimmed has 13 (11 - changed edu secondary to bachelor)
covariate_list_trimmed = c("GDP_per_cap",
                           "PHC_5.5",
                           "Edu_bachelor",
                           "Health_spending_per_cap",
                           "Nurses_per_1000",
                           "Docs_per_1000",
                           "GII",
                           "Ethnic_Frac",
                           "Linguist_Frac",
                           "Gov_Effective",
                           "CPI")

# Plotting bivariate scatter plots between covariates and outcome 

for (i in full_covariate_list){
  scatter.smooth(who_eu_53_countries[,i],who_eu_53_countries$proportion_fully_vax_gen_pop, xlab = i )
}

# Checking for multicollinearity among the remaining 13 covariates in covariate_list_trimmed

corr_matrix <- cor(who_eu_53_countries[,covariate_list_trimmed], method = "pearson", use = "complete.obs")
corrplot(corr_matrix, type = "upper", order = "hclust",   tl.cex = 0.7,
         tl.col = "black", tl.srt = 45)

# Fitting a multi-variable beta regression model with all 13 covariates and running 
# tests for multi-collinearity 

model_data <- who_eu_53_countries %>%
  select(all_of(covariate_list_trimmed),proportion_fully_vax_gen_pop)

betareg_full_model <- betareg(proportion_fully_vax_gen_pop ~ ., model_data)
summary(betareg_full_model)
mctest(betareg_full_model, type="i", corr=TRUE)

# Finding out which subset of covariates is the best for the model

# Initialising the loops with count and an empty list

count = 1
all_multi_beta_models <- list()

# Choose PHC_5.5, or don't include  
for (i in c('1','PHC_5.5')){
  # Choose one among Ethnic_Frac or Linguist_Frac, or don't include either
  for (j in c('1','Ethnic_Frac','Linguist_Frac')){
    # Choose one among GDP_per_cap, Health_spending_per_cap,CPI,Gov_Effective,Nurses_per_1000
    # or don't include any of them
    for (k in c('1','GDP_per_cap','Health_spending_per_cap','CPI','Gov_Effective','GII','Nurses_per_1000')){
      # choose none, 1, or all from Docs_per_1000, Edu_bachelor, 
      for (l in c('1','Docs_per_1000','Edu_bachelor','Docs_per_1000+Edu_bachelor')){
        multi_var_formula <- paste('proportion_fully_vax_gen_pop~',paste(i,paste(j,paste(k,l,sep = '+'),sep = '+'),sep='+'))
        multi_var_model <- betareg(multi_var_formula,model_data)
        all_multi_beta_models[[paste("Model_",count,sep = '')]] <- multi_var_model
        count <- count + 1
    }
    }
    }
}

model_eval <- matrix(ncol = 3, nrow = length(all_multi_beta_models))

for (i in seq(length(all_multi_beta_models))){
  model_eval[i,1] <- i 
  model_eval[i,2] <- all_multi_beta_models[[i]]$pseudo.r.squared
  model_eval[i,3] <- BIC(all_multi_beta_models[[i]])
}

model_eval <- data.frame(model_eval)
names(model_eval) <- c("Model_Number","Pseudo R-Square","BIC")
View(model_eval)

p <- ggplot(model_data, aes(Gov_Effective, Ethnic_Frac))
p <- p + geom_point(aes(size = PHC_5.5))
p <- p + geom_point(aes(color = proportion_fully_vax_gen_pop))
