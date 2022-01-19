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

# Select columns we're initially analysing
df <- df[c("Country.Name", "Country.Code", "GDP per capita (current US$)", 
           "Current health expenditure per capita (current US$)", 
           "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)",
           "Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)",
           "Poverty headcount ratio at $5.50 a day (2011 PPP) (% of population)",
           "Gini index (World Bank estimate)",
           "CPIA gender equality rating (1=low to 6=high)",
           "Literacy rate, adult total (% of people ages 15 and above)",
           "School enrollment, primary and secondary (gross), gender parity index (GPI)",
           "Educational attainment, at least completed primary, population 25+ years, total (%) (cumulative)",
           "Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)",
           "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)",
           "Nurses and midwives (per 1,000 people)",
           "Physicians (per 1,000 people)",
           "Hospital beds (per 1,000 people)",
           "Refugee population by country or territory of asylum")]




