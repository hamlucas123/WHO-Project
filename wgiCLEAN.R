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
euro_count <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
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


gov_eff_eur <- gov_eff[is.element(gov_eff$...1,euro_count),]
gov_count <- gov_eff_eur$'...1'
gov_eff_eur[gov_eff_eur =='#N/A'] <- NA

gov_count[is.na(gov_eff_eur$'2020...129')] #no data for Monaco and San Marino

gov_eff_eur <- gov_eff_eur[,c(1,129:134)] #select only 2020 scores
colnames(gov_eff_eur) <-  c('Country', 'Estimate','StdErr','NumSrc','Rank','Lower','Upper') #rename columns