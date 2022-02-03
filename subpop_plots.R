#Load packages
library(ggplot2)
library(dpylr)
library(tidyverse)
install.packages('extrafont')
library(extrafont)

setwd("/Users/Haj/Documents/Health Data Science/Modules II/WHO-Project")
#Load data
merg_dat <-  read.csv(file = 'data/merged_data.csv')
#Check data
head(merg_dat)
colnames(merg_dat)

#Review subpopulation data
sum(is.na(merg_dat['percent_fully_vaccinated_ltcf']))
#42 missing for LTCF
sum(is.na(merg_dat["percent_fully_vaccinated_hcw"]))
#38 missing for HCW
sum(is.na(merg_dat["percent_fully_vaccinated_above60"]))
#27 missing for those above 60

#Review GDP
sum(is.na(merg_dat[,3]))
#No missing values for GDP
names(merg_dat)[3] <- 'GDP'

#Plot GDP with subpopulations
#GDP and HCW fully vaccinated
gdp_hcw <- ggplot(merg_dat, aes(x=GDP/1000, y=percent_fully_vaccinated_hcw*100)) + geom_point(
  (aes(size=hcw_population/1000, color=percent_boosted_hcw*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 125, by = 25), limits =c(0,125)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                       limits =c(0,100)) +
  labs(y='Health Care Workers Fully Vaccinated (%)', 
       x ='GDP per capita (thousand current US Dollars)', 
       title = 'Percentage of Health Care Workers (HCW) Fully Vaccinated 
       by Gross Domestic Product (GDP)',
       size = 'HCW population 
    (thousands)', 
       color='HCW Boosted (%)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.title = element_text(colour = "black", family='Tahoma', size = 10),
        legend.text = element_text(colour = "black", family='Tahoma', size = 10)) +
        scale_color_gradient(low="#0091ff",high='springgreen1')
gdp_hcw
#not so much

colnames(merg_dat)
#GDP and 60+ fully vaccinated
gdp_60plus <- ggplot(merg_dat, aes(x=GDP/1000, y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 125, by = 25), limits =c(0,125)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='GDP per capita (thousand current US Dollars)', 
       title = 'Percentage of Older Adults Fully Vaccinated 
       by Gross Domestic Product (GDP)',
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
  scale_color_gradient(low="#0091ff",high='springgreen1')
gdp_60plus
#seems to be a strong corr here 

#GDP and LTCF
gdp_ltcf <- ggplot(merg_dat, aes(x=GDP/1000, y=percent_fully_vaccinated_ltcf*100)) + geom_point(
  (aes(size=ltcf_population/1000, color=percent_boosted_ltcf*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 125, by = 25), limits =c(0,125)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Population in LTCF Fully Vaccinated (%)', 
       x ='GDP per capita (thousand current US Dollars)', 
       title = 'Percentage of those in Long-Term Care Facilities (LTCF) Fully Vaccinated 
       by Gross Domestic Product (GDP)',
       size = 'LTCF population 
    (thousands)', 
       color='LTCF Boosted (%)') +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold",hjust = 0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 12),
        axis.text.y = element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "dark grey"),
        legend.title = element_text(colour = "black", family='Tahoma', size = 10),
        legend.text = element_text(colour = "black", family='Tahoma', size = 10)) +
  scale_color_gradient(low="#0091ff",high='springgreen1')
gdp_ltcf

#basic plots phc3.2 with subpop
plot(merg_dat$Poverty.head.count.ratio.at..3.20.a.day....of.population.,merg_dat$percent_fully_vaccinated_hcw)
plot(merg_dat$Poverty.head.count.ratio.at..3.20.a.day....of.population.,merg_dat$percent_fully_vaccinated_above60)
plot(merg_dat$Poverty.head.count.ratio.at..3.20.a.day....of.population.,merg_dat$percent_fully_vaccinated_ltcf)

colnames(merg_dat)
#basic plots gii with subpop
plot(merg_dat$Gender.Inequality.Index..GII.,merg_dat$percent_fully_vaccinated_hcw)
plot(merg_dat$Gender.Inequality.Index..GII.,merg_dat$percent_fully_vaccinated_above60)
#strong assoc above
plot(merg_dat$Gender.Inequality.Index..GII.,merg_dat$percent_fully_vaccinated_ltcf)

#GII and fully vaccinated above 60
gii_60plus <- ggplot(merg_dat, aes(x=Gender.Inequality.Index..GII., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.05), limits =c(0,0.3)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Gender Inequality Index', 
       title = 'Percentage of Older Adults
       Fully Vaccinated by Gender Inequality Index (GGI)',
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
  scale_color_gradient(low="#0091ff",high='springgreen1')
gii_60plus

#basic plots health exp with subpop
plot(merg_dat$Health.expenditure.per.capita..current.US...,merg_dat$percent_fully_vaccinated_hcw)
plot(merg_dat$Health.expenditure.per.capita..current.US...,merg_dat$percent_fully_vaccinated_above60)
#strong assoc above
plot(merg_dat$Health.expenditure.per.capita..current.US...,merg_dat$percent_fully_vaccinated_ltcf)

#health exp and older adults
healthexp_60plus <- ggplot(merg_dat, aes(x=Health.expenditure.per.capita..current.US..., y=percent_fully_vaccinated_above60*100)) + geom_point(
  (aes(size=above60_population/1000, color=percent_boosted_above60*100)),alpha=.6) +
  scale_x_continuous(breaks = seq(0, 10000, by = 2000), limits =c(0,8000)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20),
                     limits =c(0,100)) +
  labs(y='Older Adults Fully Vaccinated (%)', 
       x ='Health Expenditure per capita (Current US Dollars)', 
       title = 'Percentage of Older Adults
       Fully Vaccinated by Health Expenditure per Capita',
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
  scale_color_gradient(low="#0091ff",high='springgreen1')
healthexp_60plus
