# World Bank Data Cleaning

library(dplyr)
library(tidyr)
library(readr)
library(readxl)


df <- read.csv('data/rawdata/country-level-predictors.csv')

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

#Review column names
colnames(df)

#Change predictor names
names(df)[1] <- 'Country'
names(df)[2] <- 'Country Code'
names(df)[4] <- 'Health expenditure per capita (current US $)'
names(df)[5] <- 'Poverty head count ratio at $1.90 a day (% of population)'
names(df)[6] <- 'Poverty head count ratio at $3.20 a day (% of population)'
names(df)[7] <- "Poverty headcount ratio at $5.50 a day (% of population)" 
names(df)[10] <- 'Adult Literacy rate (% of people ages 15 and above)'
names(df)[11] <- 'Gender Parity Index (GPI)'
names(df)[12] <- "Education, at least completed primary, population 25+ years, total (%) (cumulative)" 
names(df)[13] <- 'Education, at least completed upper secondary, population 25+, total (%) (cumulative)'
names(df)[14] <- "Education, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)"

#Check value ranges/missing data

summary(as.numeric(unlist(df[,3])))
#no missing data for GDP per capita, numbers look ok

summary(as.numeric(unlist(df[,4])))
which(is.na(df[4]))
#two missing values for health expenditure per capita - Kosovo & Liechtenstein

summary(as.numeric(unlist(df[,5])))
summary(as.numeric(unlist(df[,6])))
summary(as.numeric(unlist(df[,7])))
which(is.na(df[5]))
which(is.na(df[6]))
which(is.na(df[7]))
df[c(2,30,34,43,51),]
#same countries missing data for all 3 measurements of poverty head count ratio - #Andorra, Liechtenstein, Monaco, San Marino, Turkmenistan

summary(as.numeric(unlist(df[,8])))
#5 missing for Gini index

summary(as.numeric(unlist(df[,9])))
#44 indicators missing for CPIA gender inequality rating.. may need to exclude this or find a similar alternative

summary(as.numeric(unlist(df[,10])))
#22 countries missing data for literacy rate.. same as above

summary(as.numeric(unlist(df[,11])))
#4 missing for Gender parity index

summary(as.numeric(unlist(df[,12])))
#10 missing for school enrolment - primary

summary(as.numeric(unlist(df[,13])))
#4 missing for school enrolment - secondary

summary(as.numeric(unlist(df[,14])))
#9 missing for school enrolment - bachelors

summary(as.numeric(unlist(df[,15])))
#2 missing nurses/1,000

summary(as.numeric(unlist(df[,16])))
#2 missing physicians/1,000

summary(as.numeric(unlist(df[,17])))
#2 missing hospital beds per 1,000

summary(as.numeric(unlist(df[,18])))
#3 missing for refugee population

# Change columns to numeric
df[,3:18] <- sapply(df[,3:18],as.numeric)

### KOSOVO
# Health expenditure: 3.5% of GDP in 2019
# GDP in 2019: 7.9 Billion USD
# GDP Per Capita in 2019: 4,416.108
# Health expenditure per capita:

df$`Health expenditure per capita (current US $)`[df$Country == "Kosovo"] <- 0.035 * 4416.108

### LIECHTENSTEIN
# 1.1199 average exchange rate in 2019 (Euros to USD) 
# Health expenditure per capita in 2019: 9136.24 euros
# Result makes sense because Liechtenstein and Switzerland always have top 2 in health expenditure

df$`Health expenditure per capita (current US $)`[df$Country == "Liechtenstein"] <- 9136.24 * 1.1199

### Replace CPIA Gender Equality Rating with Gender Inequality Index (GII) (2019)
gender <- read_excel('data/gender-inequality.xlsx', skip=7)

# Select 2nd and 3rd column
gender <- gender[, 2:3]

# Change Column names
colnames(gender) <- c("Country", "Gender Inequality Index (GII)")

# Round values to 3 decimal places
gender[,2] <- sapply(gender[,2], as.numeric)

# Filter by countries we want
gender <- subset(gender, Country %in% df$Country)

# Join gender with larger dataframe
df <- merge(df, gender, by.x = "Country", 
                   by.y = "Country", all.x = TRUE, all.y = FALSE)

# Drop CPIA Gender Equality
df$`CPIA gender equality rating (1=low to 6=high)` <- NULL

# missing values
# Czech Republic: 0.136
df$`Gender Inequality Index (GII)`[df$Country == "Czech Republic"] <- 0.136

# Kyrgyz Republic: 0.369
df$`Gender Inequality Index (GII)`[df$Country == "Kyrgyz Republic"] <- 0.369

# Moldova: 0.204
df$`Gender Inequality Index (GII)`[df$Country == "Moldova"] <- 0.204

# Slovak Republic: 0.191
df$`Gender Inequality Index (GII)`[df$Country == "Slovak Republic"] <- 0.191

# Replace literacy rates
literacy <- read.csv('data/cross-country-literacy-rates.csv')

# Get latest literacy values
literacy <- literacy %>% 
  group_by(Entity) %>%
  slice(which.max(Year))

# Merge new literacy values
df <- merge(df, literacy, by.x = "Country", 
            by.y = "Entity", all.x = TRUE, all.y = FALSE)

# Fills NAs with more complete dataset values
df$`Adult Literacy rate (% of people ages 15 and above)` <- ifelse(is.na(df$`Adult Literacy rate (% of people ages 15 and above)`),
                                                                   df$Literacy.rates..World.Bank..CIA.World.Factbook..and.other.sources.,
                                                                   df$`Adult Literacy rate (% of people ages 15 and above)`)

# missing values for literacy
# Czech Republic: 99.00000
df$`Adult Literacy rate (% of people ages 15 and above)`[df$Country == "Czech Republic"] <- 99

# Slovak Republic: 99.60000
df$`Adult Literacy rate (% of people ages 15 and above)`[df$Country == "Slovak Republic"] <- 99.6

# Drop unwanted columns
df$Code <- NULL
df$Year <- NULL
df$Literacy.rates..World.Bank..CIA.World.Factbook..and.other.sources. <- NULL

rm(gender)
rm(literacy)
worldbank <- df
rm(df)
rm(categories)
