datavax_child <- read.csv("data/OWD_child.csv")
euro_count <- read.csv("data/euro_count_dict.csv")
#29 countries included
datavax_child1 <- datavax_child %>%
  filter(LOCATION %in% euro_count$ISO3) %>%
  filter(TIME == "2018") %>%
  select(c("LOCATION", "SUBJECT", "TIME", "Value"))

length(unique(datavax_child1$LOCATION))
