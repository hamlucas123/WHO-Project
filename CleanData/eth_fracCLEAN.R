data <- read.csv("data/ethnicfrac.csv")
euro_count <- read.csv("data/euro_count_dict.csv")
# data from https://worldpopulationreview.com/country-rankings/most-racially-diverse-countries
# 51 countries included 
# missing Montenegro, North Macedonia

colnames(data)[1] <- 'country'
eth_missing <- data %>%
  filter(country == "Russia")
eth_frac <- data %>%
  filter(country %in% euro_count$Country | country %in% euro_count$Other.Name.Variations 
         | country %in% euro_count$Other.Name.Variations1) %>%
  rbind(eth_missing) %>%
  arrange(country)

euro_count[!(euro_count %in% eth_frac$country)]
rm(data)
rm(euro_count)
rm(eth_missing)
