library(dplyr)
library(ggplot2)

setwd ("~/WHO-Project")
tot_dat <- read.csv("data/merged_data.csv")
tot_dat <- tot_dat %>%
  mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred))

# find cut off for 25th and 85th percentile
split_dat <- quantile(tot_dat$vax_rate, probs = c(0.25, 0.85), na.rm = TRUE)

# function to find summary statistics
sumfun <- function(x) {
  n <- sum(!is.na(x))
  m <- mean(na.omit(x))
  s <- sd(na.omit(x))
  se <- s/sqrt(n)
  c(N=n, mean=m, sd = s, se = se, lwr=m-1.96*se, upr=m+1.96*se)
}

# for the 25th percentile
low_q <- tot_dat %>%
  filter(vax_rate <= split_dat[[1]])

low_qN <- low_q[,sapply(low_q, is.numeric)]



lowQ1 <- apply(low_qN, MARGIN = 2, FUN = sumfun)


# for 85th percentile
upp_q <- tot_dat %>%
  filter(vax_rate >= split_dat[[2]])

upp_qN <- upp_q[,sapply(upp_q, is.numeric)]
uppQ1 <- apply(upp_qN, MARGIN = 2, FUN = sumfun)


