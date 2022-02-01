#install.packages("expss")
library(dplyr)
library(ggplot2)
library(expss)
library(tidyr)

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
  c(N=n, mean=m, lwr=m-1.96*se, upr=m+1.96*se)
}

# table of predictors and categories
healthsys <- c("Hospital.beds..per.1.000.people.", "Health.expenditure.per.capita..current.US...", 
               "Physicians..per.1.000.people." )
gen_se <- c("GDP.per.capita..current.US..", "Poverty.head.count.ratio.at..1.90.a.day....of.population.", 
            "Gini.index..World.Bank.estimate.", "Adult.Literacy.rate....of.people.ages.15.and.above.")
soc <- c("Gender.Inequality.Index..GII.", "linguisticFractionalization", "ethnicFractionalization",
         "Refugee.population.by.country.or.territory.of.asylum")
gov <- c("stringency_index", "Government.Effectiveness.Index.Rank", "CPI.score.2020..Corruption.Index.")


# for the 25th percentile
low_q <- tot_dat %>%
  filter(vax_rate <= split_dat[[1]])

low_qN <- low_q[,sapply(low_q, is.numeric)]
lowQ1 <- t(as.data.frame(apply(low_qN, MARGIN = 2, FUN = sumfun)))

lowQ1<- cbind(pred = rownames(lowQ1), data.frame(lowQ1, row.names = NULL))

lowQ1$cat <- ifelse(lowQ1$pred %in% healthsys, 1, 
                      ifelse(lowQ1$pred %in% gen_se, 2,
                             ifelse(lowQ1$pred %in% soc, 3,
                                    ifelse(lowQ1$pred %in% gov, 4,
                                           0))))
levels()

lowQ1 %>% 
  cat <- i 
  
  tab_cells(Health.expenditure.per.capita..current.US..., Hospital.beds..per.1.000.people.) %>%
  tab_cols(N, Mean, lwr, upr) %>% 
  tab_pivot()
lowQ1[N,]
# for 85th percentile
upp_q <- tot_dat %>%
  filter(vax_rate >= split_dat[[2]])

upp_qN <- upp_q[,sapply(upp_q, is.numeric)]
uppQ1 <- apply(upp_qN, MARGIN = 2, FUN = sumfun)


