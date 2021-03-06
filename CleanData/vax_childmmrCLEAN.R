datavax_child <- read.csv("data/OWD_child.csv")
euro_count <- read.csv("data/euro_count_dict.csv")
#29 countries included
colnames(datavax_child)[1] <- 'LOCATION'
datavax_child1 <- datavax_child %>%
  filter(LOCATION %in% euro_count$ISO3) %>%
  filter(TIME == "2018") %>%
  select(c("LOCATION", "SUBJECT", "TIME", "Value"))

length(unique(datavax_child1$LOCATION))
vax_child <- datavax_child1
rm(euro_count)
rm(datavax_child)
rm(datavax_child1)
