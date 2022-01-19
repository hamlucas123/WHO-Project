# World Bank Data Cleaning

library(dplyr)
library(tidyr)
library(readr)

df <- read.csv('data/country-level-predictors.csv')

# replacing all ".." with NA
df[df==".."] <- NA

# create column with last known value for each predictor
df$lastval <- df[cbind( 1:nrow(df), max.col(!is.na(df),"last") )]

# If lastval = Series.Code, change to NA
df$value <- ifelse(df$Series.Code == df$lastval, NA, df$lastval)

# rename first column
colnames(df)[1] <- "Country.Name"

# select base columns
df <- df[c("Country.Name","Country.Code","Series.Name", "value")]

# See unique category names
categories <- unique(df$Series.Name) 

# Remove empty 5 last rows
df <- slice(df, 1:(n()-5))

# Pivot dataframe
df <- df %>% pivot_wider(names_from = Series.Name, values_from = value)



