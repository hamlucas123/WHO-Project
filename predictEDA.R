#install.packages("expss")
#install.packages("fabricatr")
library(htmlTable)
library(magrittr)
library(dplyr)
library(ggplot2)
library(expss)
library(tidyr)
library(fabricatr)


setwd ("~/WHO-Project")
tot_dat <- read.csv("data/merged_data.csv")
tot_dat <- tot_dat %>%
  mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred)) 


# find cut off for each percentile
split_dat <- quantile(tot_dat$vax_rate, probs = c(0.25,0.5,0.75,1), na.rm = TRUE)

# function to find summary statistics
sumfun <- function(x) {
  n <- sum(!is.na(x))
  m <- mean(na.omit(x))
  s <- sd(na.omit(x))
  c(N=n, mean=m, sd = s)
}

# table of predictors and categories
healthsys <- c("Hospital.beds..per.1.000.people.", "Health.expenditure.per.capita..current.US...", 
               "Physicians..per.1.000.people." )
gen_se <- c("GDP.per.capita..current.US..", "Poverty.head.count.ratio.at..3.20.a.day....of.population.", 
            "Poverty.headcount.ratio.at..5.50.a.day....of.population.",
            "Education..at.least.completed.upper.secondary..population.25...total......cumulative.")
soc <- c("Gender.Inequality.Index..GII.", "linguisticFractionalization", "ethnicFractionalization",
         "religiousFractionalization", 
         "Refugee.population.by.country.or.territory.of.asylum")
gov <- c("stringency_index", "Government.Effectiveness.Index.Rank", "CPI.score.2020..Corruption.Index.")

#############################################
# for up to the 25th percentile
datQ1 <- tot_dat %>%
  filter(vax_rate <= split_dat[[1]])

# transpose data so that predictors are in 1 column
datQ1_n <- datQ1[,sapply(datQ1, is.numeric)]
datQ1_tab <- t(as.data.frame(apply(datQ1_n, MARGIN = 2, FUN = sumfun)))
datQ1_tab<- cbind(pred = rownames(datQ1_tab), data.frame(datQ1_tab, row.names = NULL))

#categorising predictors
datQ1_tab$cat <- ifelse(datQ1_tab$pred %in% healthsys, 1, 
                      ifelse(datQ1_tab$pred %in% gen_se, 2,
                             ifelse(datQ1_tab$pred %in% soc, 3,
                                    ifelse(datQ1_tab$pred %in% gov, 4,
                                           0))))
datQ1_tab <- datQ1_tab %>%
  filter(!(cat==0))

#############################################
# for 25th-50th percentile
datQ2 <- tot_dat %>%
  filter(vax_rate <= split_dat[[2]] & vax_rate > split_dat[[1]])

datQ2_n <- datQ2[,sapply(datQ2, is.numeric)]
datQ2_tab <- t(as.data.frame(apply(datQ2_n, MARGIN = 2, FUN = sumfun)))
datQ2_tab<- cbind(pred = rownames(datQ2_tab), data.frame(datQ2_tab, row.names = NULL))

#categorising predictors
datQ2_tab$cat <- ifelse(datQ2_tab$pred %in% healthsys, 1, 
                        ifelse(datQ2_tab$pred %in% gen_se, 2,
                               ifelse(datQ2_tab$pred %in% soc, 3,
                                      ifelse(datQ2_tab$pred %in% gov, 4,
                                             0))))
datQ2_tab <- datQ2_tab %>%
  filter(!(cat==0))

#############################################
# for 50th-75th percentile
datQ3 <- tot_dat %>%
  filter(vax_rate <= split_dat[[3]] & vax_rate > split_dat[[2]])

datQ3_n <- datQ3[,sapply(datQ3, is.numeric)]
datQ3_tab <- t(as.data.frame(apply(datQ3_n, MARGIN = 2, FUN = sumfun)))
datQ3_tab<- cbind(pred = rownames(datQ3_tab), data.frame(datQ3_tab, row.names = NULL))

#categorising predictors
datQ3_tab$cat <- ifelse(datQ3_tab$pred %in% healthsys, 1, 
                        ifelse(datQ3_tab$pred %in% gen_se, 2,
                               ifelse(datQ3_tab$pred %in% soc, 3,
                                      ifelse(datQ3_tab$pred %in% gov, 4,
                                             0))))
datQ3_tab <- datQ3_tab %>%
  filter(!(cat==0))

#############################################
# for above 75th percentile
datQ4 <- tot_dat %>%
  filter(vax_rate > split_dat[[3]])

datQ4_n <- datQ4[,sapply(datQ4, is.numeric)]
datQ4_tab <- t(as.data.frame(apply(datQ4_n, MARGIN = 2, FUN = sumfun)))
datQ4_tab<- cbind(pred = rownames(datQ4_tab), data.frame(datQ4_tab, row.names = NULL))

#categorising predictors
datQ4_tab$cat <- ifelse(datQ4_tab$pred %in% healthsys, 1, 
                        ifelse(datQ4_tab$pred %in% gen_se, 2,
                               ifelse(datQ4_tab$pred %in% soc, 3,
                                      ifelse(datQ4_tab$pred %in% gov, 4,
                                             0))))
datQ4_tab <- datQ4_tab %>%
  filter(!(cat==0))

#############################################
# combining tables for Q1, Q2, Q3, Q4
# was unable to inner_join via pipeline
Q_tab <- inner_join(datQ1_tab[1:4], datQ2_tab[1:4], by = "pred", suffix = c(".Q1", ".Q2"))
Q_tab1 <-inner_join(datQ3_tab[1:4], datQ4_tab[1:4], by = "pred", suffix = c(".Q3",".Q4"))
Q_FIN <- inner_join(Q_tab, Q_tab1, by = "pred")

