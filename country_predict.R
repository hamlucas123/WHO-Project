count_pred <- read.csv("~/LSHTM_DC/country-level-predictors.csv")
library(dplyr)
library(tidyr)

#replacing all ".." with NA
count_pred[count_pred==".."] <- NA
#create column with last known value for each predictor
count_pred$lastval <- count_pred[cbind( 1:nrow(count_pred), max.col(!is.na(count_pred),"last") )]
count_pred$lastval <- as.numeric(count_pred$lastval)

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
