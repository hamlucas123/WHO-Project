library(dplyr)
library(ggplot2)

setwd ("~/WHO-Project")
tot_dat <- read.csv("data/merged_data.csv")
tot_dat <- tot_dat %>%
  mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred))

# find cut off for 25th and 85th percentile
split_dat <- quantile(tot_dat$vax_rate, probs = c(0.25, 0.85), na.rm = TRUE)

# for the 25th percentile
low_q <- tot_dat %>%
  filter(vax_rate <= split_dat[[1]])

low_qN <- low_q[,sapply(low_q, is.numeric)]
low_se <- apply(low_qN, 2, sd, na.rm=TRUE)
low_mean <- apply(low_qN, 2, mean, na.rm=TRUE)
low_sum <- as.data.frame(c(low_se, low_mean), row.names = names(low_q))

# for 85th percentile
upp_q <- tot_dat %>%
  filter(vax_rate >= split_dat[[2]])
