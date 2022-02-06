#Load packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(extrafont)
library(ggpubr)

setwd("/Users/Haj/Documents/Health Data Science/Modules II/WHO-Project")
#Load data
merg_dat <-  read.csv(file = 'data/merged_data.csv')
#Check data
head(merg_dat)
colnames(merg_dat)

#remove kosovo and liech
merg_dat <- merg_dat[-c(30,55),]


#Review subpopulation data
sum(is.na(merg_dat['percent_fully_vaccinated_ltcf']))
#40 missing for LTCF
sum(is.na(merg_dat["percent_fully_vaccinated_hcw"]))
#36 missing for HCW
sum(is.na(merg_dat["percent_fully_vaccinated_above60"]))
#25 missing for those above 60

#Review GDP
sum(is.na(merg_dat[,3]))
#No missing values for GDP
names(merg_dat)[3] <- 'GDP'

#setup 
#store df predictors as a matrix for corr function
merg_dat_matrix <- scale(merg_dat[,3:70])
colnames(merg_dat)
#correlation of subpop with all predictors
#first for those above 60
cor_above60 <- cor(merg_dat_matrix[,'percent_fully_vaccinated_above60'],merg_dat_matrix[,1:27], use="pairwise.complete.obs")
#find where corr >0.5 or < -0.5
highly_corr_above60 <- cor_above60[1,which(cor_above60 > 0.5 | cor_above60 < -0.5)]
highly_corr_above60<- as.data.frame(highly_corr_above60)

#drop irrelevant rows
rownames(highly_corr_above60)
highly_corr_above60 <- highly_corr_above60[1:11,, drop=FALSE]

print(highly_corr_above60)
nrow(highly_corr_above60)
#11 rows that are highly correlated

#COR FOR HCW
cor_hcw <- cor(merg_dat_matrix[,'percent_fully_vaccinated_hcw'],merg_dat_matrix[,1:27], use="pairwise.complete.obs")
cor_hcw
#find where corr >0.5 or < -0.5
highly_corr_hcw <- cor_hcw[1,which(cor_hcw > 0.5 | cor_hcw < -0.5)]
highly_corr_hcw <- as.data.frame(highly_corr_hcw)
#drop irrelevant rows
View(highly_corr_hcw)
rownames(highly_corr_hcw)
highly_corr_hcw <- highly_corr_hcw[1,, drop=FALSE]
print(highly_corr_hcw)
#only 1 predictor highly correlated with hcw

#COR FOR LTCF
cor_ltcf <- cor(merg_dat_matrix[,'percent_fully_vaccinated_ltcf'],merg_dat_matrix[,1:27], use="pairwise.complete.obs")
#find where corr >0.5 or < -0.5
highly_corr_ltcf <- cor_ltcf[1,which(cor_ltcf > 0.5 | cor_ltcf < -0.5)]
highly_corr_ltcf <- as.data.frame(highly_corr_ltcf)
#drop irrelevant rows
View(highly_corr_ltcf)
rownames(highly_corr_ltcf)
#none correlated with LTCF

#double checking
#above 60
test_above60 <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat[,3:27],use="pairwise.complete.obs")
sum(test_above60 > 0.5 | test_above60 < -0.5, na.rm = TRUE)

#hcw
test_hcw <- cor(merg_dat$percent_fully_vaccinated_hcw,merg_dat[,3:27],use="pairwise.complete.obs")
sum(test_hcw > 0.5 | test_hcw < -0.5, na.rm = TRUE)

#ltcf
test_ltcf <- cor(merg_dat$percent_fully_vaccinated_ltcf,merg_dat[,3:27],use="pairwise.complete.obs")
sum(test_ltcf> 0.5 | test_ltcf < -0.5, na.rm =TRUE)


#choosing predictors
#order values
highly_corr_above60[order(-highly_corr_above60$highly_corr_above60), , drop = FALSE]
#most corr = flu vax, gov eff, hee, cpi, , gdp, pov head count, hosp bed, gii, ph 5.5
#from models with gen population- phr 5.5 a day, Govern effectiveness, health exp, gii

#plot for over 60s
#Government eff and 60+ fully vaccinated (1st)
cor_above60_goveff <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Government.Effectiveness.Index.Rank, use="pairwise.complete.obs")
cor_above60_goveff
goveff_60plus <- ggplot(merg_dat, aes(x=Government.Effectiveness.Index.Rank, y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits =c(50,100)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Government Effectiveness Index',
       size = 'Older Adults population 
    (thousands)', 
       color='Older Adults Boosted (%)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.title = element_text(colour = "black", family='Tahoma', size = 10),
        legend.text = element_text(colour = "black", family='Tahoma', size = 10)) +
  scale_color_gradient(low="#0091ff",high='springgreen1')+
  stat_cor(method="pearson",label.y=0)
goveff_60plus


#GII and fully vaccinated above 60 (2nd)
cor_above60_genderinequality <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Gender.Inequality.Index..GII., use="pairwise.complete.obs")
cor_above60_genderinequality
gii_60plus <- ggplot(merg_dat, aes(x=Gender.Inequality.Index..GII., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0.05, 0.5, by = 0.05), limits =c(0.05,0.3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Gender Inequality Index (GII)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
gii_60plus
summary(merg_dat$Gender.Inequality.Index..GII.)

#CPI and 60 + full vaccinated (3rd)
cor_above60_cpi <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$CPI.score.2020..Corruption.Index., use="pairwise.complete.obs")
cor_above60_cpi
cpi_60plus <- ggplot(merg_dat, aes(x=CPI.score.2020..Corruption.Index., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits =c(40,90)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(x ='Corruption Perceptions Index (CPI)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
cpi_60plus

#phr5.5 and 60+ fully vaccinated (4th)
cor_above60_phr5 <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Poverty.headcount.ratio.at..5.50.a.day....of.population., use="pairwise.complete.obs")
cor_above60_phr5
phr5_60plus <- ggplot(merg_dat, aes(x=Poverty.headcount.ratio.at..5.50.a.day....of.population., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 7, by = 2), limits =c(0,7)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Poverty head count ratio 
at 5.50 (US Dollars) a day') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
phr5_60plus

#GDP and 60+ fully vaccinated (5th)
cor_above60_gdp <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$GDP, use="pairwise.complete.obs")
cor_above60_gdp
gdp_60plus <- ggplot(merg_dat, aes(x=GDP/1000, y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 125, by = 25), limits =c(0,125)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Gross Domestic Product (GDP) 
per capita (thousand current US Dollars)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
gdp_60plus
summary(merg_dat$GDP)

#health exp and older adults (6th/org legend)
cor_above60_healthexp <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Health.expenditure.per.capita..current.US..., use="pairwise.complete.obs")
cor_above60_healthexp 
healthexp_60plus <- ggplot(merg_dat, aes(x=Health.expenditure.per.capita..current.US..., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 9000, by = 2000), limits =c(50,9000)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(x ='Health Expenditure 
per capita (Current US Dollars)', 
       size = 'Older Adults population 
    (thousands)', 
       color='Older Adults Boosted (%)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.title.y = element_blank(),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = 'none') +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
healthexp_60plus
summary(merg_dat$Health.expenditure.per.capita..current.US...)





#hosp beds and 60+ fully vaccinated (7th)
cor_above60_hospbeds <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Hospital.beds..per.1.000.people., use="pairwise.complete.obs")
cor_above60_hospbeds
hospbeds_60plus <- ggplot(merg_dat, aes(x=Hospital.beds..per.1.000.people., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 11, by = 2), limits =c(0,10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Hospital beds per 1,000 people') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
hospbeds_60plus

#flu vax beds and 60+ fully vaccinated (8th)
cor_above60_flu <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Flu.Vax.Value, use="pairwise.complete.obs")
cor_above60_flu
flu_60plus <- ggplot(merg_dat, aes(x=Flu.Vax.Value, y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 80, by = 10), limits =c(10,80)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Flu Vax Value -[NEED TO ADJUST]') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
flu_60plus

# nurses/1000 and 60+ fully vaccinated (9th)
cor_above60_nurses <- cor(merg_dat$percent_fully_vaccinated_above60,merg_dat$Nurses.and.midwives..per.1.000.people., use="pairwise.complete.obs")
cor_above60_nurses
nurses_60plus <- ggplot(merg_dat, aes(x=Nurses.and.midwives..per.1.000.people., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(2, 20, by = 4), limits =c(2,20)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs( 
       x ='Nurses and midwives per 1,000 people') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.position = "none") +
  scale_color_gradient(low="#0091ff",high='springgreen1') +
  stat_cor(method="pearson", label.y=0)
nurses_60plus 


#combine plots into one
comb_plots <- ggarrange(goveff_60plus,
          gii_60plus,
          cpi_60plus,
          phr5_60plus,
          gdp_60plus,
          healthexp_60plus,
          hospbeds_60plus,
          flu_60plus,
          nurses_60plus, 
          ncol =3,
          nrow= 3,
          common.legend= TRUE,
          legend = 'bottom')
#add title with annotation
comb_plot_labels <- annotate_figure(comb_plots,
                top = text_grob('Percentage of Older Adults completely Vaccinated against Covid-19 by correlated predictors', color = 'black',
                                face = 'bold',size='16'))
comb_plot_labels

#save tables for correlations
cor_above60 <- as.data.frame(cor_above60)
cor_hcw <- as.data.frame(cor_hcw)
cor_ltcf <- as.data.frame(cor_ltcf)

#drop columns that may be irrelevant
cor_above60 <- select(cor_above60,-c(total_vaccinations,people_vaccinated,DTP..Child.Vax.,MEASLES..Child.Vax.,stringency_index,
                                     Education..at.least.completed.primary..population.25..years..total......cumulative.,
                                     Education..at.least.completed.upper.secondary..population.25...total......cumulative.,
                                     Education..at.least.Bachelor.s.or.equivalent..population.25...total......cumulative.,
                                     Refugee.population.by.country.or.territory.of.asylum,
                                     linguisticFractionalization,
                                     ethnicFractionalization,
                                     religiousFractionalization))
cor_hcw <- select(cor_hcw,-c(total_vaccinations,people_vaccinated,DTP..Child.Vax.,MEASLES..Child.Vax.,stringency_index,
                             Education..at.least.completed.primary..population.25..years..total......cumulative.,
                             Education..at.least.completed.upper.secondary..population.25...total......cumulative.,
                             Education..at.least.Bachelor.s.or.equivalent..population.25...total......cumulative.,
                             Refugee.population.by.country.or.territory.of.asylum,
                             ethnicFractionalization,
                             linguisticFractionalization,
                             religiousFractionalization,
                             Flu.Vax.Value))
cor_ltcf <- select(select(cor_ltcf,-c(total_vaccinations,people_vaccinated,DTP..Child.Vax.,MEASLES..Child.Vax.,stringency_index,
                                      Education..at.least.completed.primary..population.25..years..total......cumulative.,
                                      Education..at.least.completed.upper.secondary..population.25...total......cumulative.,
                                      Education..at.least.Bachelor.s.or.equivalent..population.25...total......cumulative.,
                                      Refugee.population.by.country.or.territory.of.asylum,
                                      ethnicFractionalization,
                                      linguisticFractionalization,
                                      religiousFractionalization,
                                      Flu.Vax.Value)))

cor_above60 <- select(cor_above60,-c(linguisticFractionalization))

#write files
write.csv(cor_above60,'Correlation table Older Adults.csv')
write.csv(cor_hcw, 'Correlation table Health Care Workers.csv')
write.csv(cor_ltcf,'Correlation table LTCF')
