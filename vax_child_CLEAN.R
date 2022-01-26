datavax <- read.csv("data/global-vaccination-coverage.csv")

vax <- datavax %>%
  filter(Entity %in% euro_count_dict$`Country Names (WHO)`) %>%
  filter(Year=="2019")
