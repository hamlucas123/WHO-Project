count_pred <- read.csv("~/LSHTM_DC/country-level-predictors.csv")
library(dplyr)
library(tidyr)

#replacing all ".." with NA
count_pred[count_pred==".."] <- NA
#create column with last known value for each predictor
count_pred$lastval <- count_pred[cbind( 1:nrow(count_pred), max.col(!is.na(count_pred),"last") )]
count_pred$lastval <- as.numeric(count_pred$lastval)

