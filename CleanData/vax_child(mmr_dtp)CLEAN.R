datavax_child <- read.csv("data/OWD_child.csv")
euro_count_dict <- read.csv("data/euro_count_dict.csv")
#29 countries included
colnames(datavax_child)[1] <- 'LOCATION'
datavax_child1 <- datavax_child %>%
  filter(LOCATION %in% euro_count_dict$`Country Code (WorldBank)`) %>%
  filter(TIME == "2018")

length(unique(datavax_child1$LOCATION))
