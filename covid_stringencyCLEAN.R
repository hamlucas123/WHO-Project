## #############################################################################
## # COVID stringency Cleaning and Compiling
## # WHO Data Challenge Project
## #############################################################################
##

library(dplyr)

#load covid stringency dataset
csi <- read.csv('data/covid-stringency-index.csv')

#subset dataset with European countries
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

#find countries in euro_count also in csi dataset
csi_country <- csi$Entity[is.element(csi$Entity, euro_count)] %>% unique()
setdiff(euro_count, csi_country)
#Moldova -> Republic of Moldova
#United Kingdom -> United Kingdom of Great Britain and Northern Ireland
#Russian Federation -> Russia
csi[csi == 'Moldova'] <- 'Republic of Moldova'
csi[csi == 'United Kingdom'] <- 'United Kingdom of Great Britain and Northern Ireland'
csi[csi == 'Russia'] <- 'Russian Federation'
#missing Armenia, Montenegro, North Macedonia

#subset dataset with European Countries and last week of December
csi<- csi[is.element(csi$Entity, euro_count),]
csi_country <- csi$Entity[is.element(csi$Entity, euro_count)] %>% unique()
setdiff(euro_count, csi_country)


#convert date to date format
csi$Day <- as.Date(csi$Day)

#which countries have data for last week of 2021
missing_count <- setdiff(csi$Entity, with(csi, csi[(Day >= '2021-12-24') & (Day <='2021-12-31'),])$Entity %>% unique())
#Cyprus and Turkmenistan are missing key dates

#taking the last 7 entries for Cyprus and Turkmenistan
missing <- csi[csi$Entity == missing_count,]
missing <- missing[with(missing, order(Entity, Day, decreasing = T)),]
length(which(missing$Entity == 'Turkmenistan'))
missing <- missing[c(1:7,346:353),]

#aggregate csi score
with(csi, csi[(Day >= '2021-12-24') & (Day <='2021-12-31'),]
aggr <- aggregate(stringency_index~Entity+Code, data = csi, FUN = mean)
aggr_missing <- aggregate(stringency_index~Entity+Code, data = missing, FUN = mean)
csi <- rbind(aggr, aggr_missing)
