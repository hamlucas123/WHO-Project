dataflu <- read.csv("~/LSHTM_DC/DP_LIVE_21012022160052490.csv")
# 23 countries included
dataflu <- dataflu %>%
  filter(LOCATION %in% euro_count_dict$`Country Code (WorldBank)`) %>%
  filter(TIME == "2018")
