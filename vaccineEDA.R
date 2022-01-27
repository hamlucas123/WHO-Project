## #############################################################################
## # EDA Vaccination Rates
## # WHO Data Challenge Project
## #############################################################################
##
source('DataMERGE.R')

library(ggplot2)
library(dplyr)

df <- df %>% mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred))

vax_pop_plot <- ggplot(data = df[complete.cases(df$vax_rate),],
                   aes(x = reorder(Country.x, vax_rate),
                       y = vax_rate)) + 
  geom_point(aes(size = population)) + 
  labs(x = 'Country', y = 'Vaccination rate by total population (%)')+
  scale_x_discrete(guide = guide_axis(n.dodge=5)) +
  ylim(0, 100)
vax_pop_plot

vax_plot <- ggplot(data = df[complete.cases(df$vax_rate),],
                       aes(x = reorder(Country.x, vax_rate),
                           y = vax_rate)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Country', y = 'Vaccination rate by total population (%)', title = 'COVID-19 Fully vaccinated rate of WHO Europe countries')+
  scale_x_discrete(guide = guide_axis(n.dodge=5)) +
  ylim(0, 100)+
  geom_hline(yintercept=70, linetype="dashed", color = "red")
  
vax_plot
