dataflu <- read.csv("data/OWD_flu65.csv")
euro_count <- read.csv("data/euro_count_dict.csv")
# 23 countries included

dataflu <- dataflu %>%
  filter(LOCATION %in% euro_count$ISO3) %>%
  filter(TIME == "2018") %>%
  select(c("LOCATION", "TIME", "Value"))
