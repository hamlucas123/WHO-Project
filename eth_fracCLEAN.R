data <- read.csv("~/LSHTM_DC/ethnicfrac.csv")
# data from https://worldpopulationreview.com/country-rankings/most-racially-diverse-countries
# 51 countries included 
# missing Montenegro, North Macedonia

euro_count<-c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                "Belarus", "Belgium", "Bosnia and Herzegovina","Bulgaria",
                "Croatia", "Cyprus", "Czechia", 
                "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany",
                "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy",
                "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg",
                "Malta", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
                "Norway", "Poland", "Portugal", "Republic of Moldova", "Romania", 
                "Russian Federation", "San Marino", "Serbia", "Slovakia", "Slovenia",
                "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan",
                "Ukraine", "United Kingdom of Great Britain and Northern Ireland", "Uzbekistan")



eth_frac <- data %>%
  filter(country %in% euro_count) 

euro_count[!(euro_count %in% eth_frac$country)]
