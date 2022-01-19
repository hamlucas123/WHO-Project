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

#review column names
colnames(df)

#Change predictor names
names(df)[1] <- 'Country'
names(df)[2] <- 'Country Code'
names(df)[4] <- 'Health expenditure per capita (current US $)'
names(df)[5] <- 'Poverty head count ratio at $1.90 a day (% of population)'
names(df)[6] <- 'Poverty head count ratio at $3.20 a day (% of population)'
names(df)[7] <- "Poverty headcount ratio at $5.50 a day (% of population)" 
names(df)[10] <- 'Adult Literacy rate (% of people ages 15 and above)" '
#Need to review column 11 ??how is it school enrolment and Gender parity
names(df)[12] <- "Education, at least completed primary, population 25+ years, total (%) (cumulative)" 
names(df)[13] <- 'Education, at least completed upper secondary, population 25+, total (%) (cumulative)'
names(df)[14] <- "Education, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)"

#Check missing data for each column
sum(is.na(df[2]))
sum(is.na(df[3]))
sum(is.na(df[4]))

sum(is.na(df[4]))
which(is.na(df[4]))
#two missing values for health expenditure per capita - Kosovo & Liechtenstein
#Kosovo public expenditure on health was 3.5 % of GDP in 2019 - can't find data on per capita
#https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Enlargement_countries_-_health_statistics
#https://www.nationmaster.com/nmx/timeseries/liechtenstein-healthcare-expenditure-on-healthcare-providers
#Liechenstein 9136 euros per capita in 2019

sum(is.na(df[5]))
which(is.na(df[5]))
sum(is.na(df[6]))
which(is.na(df[6]))
sum(is.na(df[7]))
which(is.na(df[7]))
#same countries missing data for all 3 measurements of poverty head count ratio
df[c(2,30,34,43,51),]
#Andorra, Liechtenstein, Monaco, San Marino, Turkmenistan