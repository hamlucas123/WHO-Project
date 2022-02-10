## #############################################################################
## # WGI Cleaning and Compiling
## # WHO Data Challenge Project
## #############################################################################
##

library(dplyr)
library(readxl)

##WGI dataset
gov_eff <- read_excel('data/wgidataset.xlsx', sheet = 4, skip = 13)

#select interested countries in analysing
euro_count_gov_efff <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                "Croatia", "Cyprus", "Czech Republic",
                "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany",
                "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                "Kazakhstan", "Kyrgyz Republic", "Latvia", "Lithuania", "Luxembourg",
                "Malta", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
                "Norway", "Poland", "Portugal", "Moldova", "Romania",
                "Russian Federation", "San Marino", "Serbia", "Slovak Republic", "Slovenia",
                "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan",
                "Ukraine", "United Kingdom", "Uzbekistan")
euro_count <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                              "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                              "Croatia", "Cyprus", "Czechia",
                              "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany",
                              "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                              "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg",
                              "Malta", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
                              "Norway", "Poland", "Portugal", "Republic of Moldova", "Romania",
                              "Russian Federation", "San Marino", "Serbia", "Slovakia", "Slovenia",
                              "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan",
                              "Ukraine", "United Kingdom of Great Britain and Northern Ireland", "Uzbekistan")



gov_eff <- gov_eff[is.element(gov_eff$...1,euro_count_gov_efff),]
gov_eff[gov_eff =='#N/A'] <- NA

gov_eff$'...1'[is.na(gov_eff$'2020...129')] #no data for Monaco and San Marino

gov_eff <- gov_eff[,c(1:2,129:134)] #select only 2020 scores
colnames(gov_eff) <-  c('Country','Code', 'Estimate','StdErr','NumSrc','Rank','Lower','Upper') #rename columns

gov_eff$Country[match(setdiff(euro_count_gov_efff, euro_count), gov_eff$Country)] <- setdiff(euro_count, euro_count_gov_efff) #replace country names

wgi <- gov_eff
rm(gov_eff)
rm(euro_count)
rm(euro_count_gov_efff)
