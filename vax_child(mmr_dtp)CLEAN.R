datavax_child <- read.csv("~/LSHTM_DC/OWD_child.csv")
#29 countries included
datavax_child1 <- datavax_child %>%
  filter(LOCATION %in% euro_count_dict$`Country Code (WorldBank)`) %>%
  filter(TIME == "2018")

length(unique(datavax_child1$LOCATION))
