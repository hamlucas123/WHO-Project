# World Bank Data Cleaning

df <- read.csv('data/country-level-predictors.csv')

# replacing all ".." with NA
# df[df==".."] <- NA

# create column with last known value for each predictor
df$lastval <- df[cbind( 1:nrow(df), max.col(!is.na(df),"last") )]
df$lastval <- as.numeric(df$lastval)