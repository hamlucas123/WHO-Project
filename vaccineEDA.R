## #############################################################################
## # EDA Vaccination Rates
## # WHO Data Challenge Project
## #############################################################################
##
source('DataMERGE.R')

library(ggplot2)
library(dplyr)

df <- df %>% mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred))

summarise(df,
          Mean = mean(na.omit(vax_rate)),
          SD = sd(na.omit(vax_rate)),
          Low = quantile(na.omit(vax_rate), 0.025), High = quantile(na.omit(vax_rate), 0.975))

sum(na.omit(df$vax_rate) < 70)
sum(na.omit(df$vax_rate) > 70)

df %>%
  select(Country.x, vax_rate) %>%
  filter(vax_rate == max(na.omit(vax_rate)))

df %>%
  select(Country.x, vax_rate) %>%
  filter(vax_rate == min(na.omit(vax_rate)))

vax_pop_plot <- ggplot(data = df[complete.cases(df$vax_rate),],
                       aes(x = reorder(`Country Code`, vax_rate),
                           y = vax_rate,
                           size = population/1000000)) + 
  geom_point() + 
  labs(x = 'Country', y = 'Percentage population fully vaccinated (%)', title = 'COVID-19 Fully vaccinated rate of WHO Europe countries',
       size = 'Population per million')+
  scale_x_discrete(guide = guide_axis(n.dodge=5)) +
  ylim(0, 100)+
  scale_size_continuous(breaks = c(25,50,75,100,125),
                        range = c(1,5)) +
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  annotate('text', y=72, x= 'UKR', label ='70% vaccination rate', size = 3)

vax_pop_plot

ggsave(
  filename ='vax_scatter.png',
  device = 'png',
  path = 'plots',
  dpi = 'print'
)


vax_plot <- ggplot(data = df[complete.cases(df$vax_rate),],
                   aes(x = reorder(`Country Code`,vax_rate),
                       y = vax_rate)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Country', y = 'Vaccination rate by total population (%)', title = 'COVID-19 Fully vaccinated rate of WHO Europe countries')+
  scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  ylim(0, 100)+
  geom_hline(yintercept=70, linetype="dashed", color = "red")+
  annotate('text', y=72, x= 'UKR', label ='70% vaccination rate', size = 3)

vax_plot

ggsave(
  filename ='vax_bar.png',
  device = 'png',
  path = 'plots',
  dpi = 'print'
)
