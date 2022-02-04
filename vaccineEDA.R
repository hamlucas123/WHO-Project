## #############################################################################
## # EDA Vaccination Rates
## # WHO Data Challenge Project
## #############################################################################
##
source('DataMERGE.R')

#install.packages('geofacet')
#install.packages('lemon')
#install.packages('kableExtra')

library(ggplot2)
library(geofacet)
library(dplyr)
library(lemon)
library(kableExtra)

euro_count <- read.csv('data/euro_count_dict.csv')

df <- df %>% mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred))

df <- df[!df$Country.x == 'Liechtenstein',]

df <- df %>% mutate(x = rep(1, nrow(df)))
df <- df %>% mutate(y = rep(1, nrow(df)))

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

table <- df %>% 
  select('Country' = Country.x, 
         'Code' = `Country Code`,
         'Vaccination rate' = vax_rate,
         'Total Population' = population)

table %>%
  kbl(caption = 'Vaccination rate and Population of WHO Europe Countries') %>%
  kable_classic() %>%
  kable_styling() %>%
  save_kable(file = "plots/table.pdf", self_contained = T)


mygrid <- data.frame(
  row = c(1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8),
  col = c(1, 5, 6, 7, 8, 1, 2, 5, 4, 8, 9, 8, 5, 7, 4, 6, 10, 11, 12, 9, 10, 11, 12, 6, 2, 3, 5, 4, 7, 8, 12, 10, 11, 3, 9, 6, 5, 2, 1, 4, 8, 7, 7, 8, 6, 4, 5, 9, 7, 8, 3, 10, 11),
  code = c("IS", "NO", "SE", "FI", "EE", "IE", "GB", "DK", "NL", "LV", "BY", "LT", "DE", "PL", "BE", "CZ", "RU", "KZ", "KG", "UA", "GE", "UZ", "TJ", "SK", "AD", "FR", "AT", "LU", "RS", "RO", "TM", "AM", "AZ", "MC", "MD", "HU", "SI", "ES", "PT", "CH", "BG", "BA", "ME", "MK", "HR", "IT", "SM", "TR", "AL", "GR", "MT", "CY", "IL"),
  name = c("Iceland", "Norway", "Sweden", "Finland", "Estonia", "Ireland", "United Kingdom", "Denmark", "Netherlands", "Latvia", "Belarus", "Lithuania", "Germany", "Poland", "Belgium", "Czechia", "Russian Federation", "Kazakhstan", "Kyrgyz Republic", "Ukraine", "Georgia", "Uzbekistan", "Tajikistan", "Slovakia", "Andorra", "France", "Austria", "Luxembourg", "Serbia", "Romania", "Turkmenistan", "Armenia", "Azerbaijan", "Monaco", "Moldova", "Hungary", "Slovenia", "Spain", "Portugal", "Switzerland", "Bulgaria", "Bosnia and Herzegovina", "Montenegro", "Macedonia", "Croatia", "Italy", "San Marino", "Turkey", "Albania", "Greece", "Malta", "Cyprus", "Israel"),
  stringsAsFactors = FALSE
)

euro_count <- euro_count[match(mygrid$code, euro_count$ISO2),]
mygrid$code <- euro_count$ISO3

euro_grid <- ggplot(df, aes(x, y)) +
  facet_geo(~`Country Code`, grid = mygrid, label = 'code')+
  geom_point(aes(colour = vax_rate, size = population/1000000 )) +
  scale_colour_gradient2(low = "red",
                         mid = 'blue',
                         high = "blue",
                         midpoint = 70)+
  labs(x = '', y = '', title = '',
       size = 'Population per million', colour = 'Vaccination Rate (%)')+
  scale_size_continuous(breaks = c(25,50,75,100,125),
                        range = c(1,5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

euro_grid

vul_pop <- df %>%
  select(`Country Code`, colnames(df)[c(59,64,69)])

vul_pop_long <- pivot_longer(
  data            = vul_pop,
  cols            = colnames(df)[c(59,64,69)],
  names_to        = 'group',
  values_to       = 'vax_rate')

vul_pop_grid <- ggplot(vul_pop_long, aes(group, vax_rate*100, fill = group)) +
  facet_geo(~`Country Code`, grid = mygrid, label = 'code')+
  geom_col()+
  labs(x = '', y = '', title = '',
       size = 'Population per million', fill = 'Vaccination Rate (%)')+
  scale_size_continuous(breaks = c(25,50,75,100,125),
                        range = c(1,5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

vul_pop_grid



#############################end###################################

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

missing <- c("Andorra","Armenia","Azerbaijan",'Georgia','Israel',"Kazakhstan", "Kyrgyz Republic", "Liechtenstein","Monaco",'North Macedonia','San Marino','Tajikistan','Turkmenistan','Uzbekistan')

count <- df$Country.x

count[count == 'Czech Republic'] <- 'Czechia'
count[count =='Moldova'] <- 'Moldova, Republic of'
count[count == 'Slovak Republic'] <- 'Slovakia'
count[count == 'North Macedonia'] <- 'Macedonia, the former Yugoslav Republic of'

df <- df %>% mutate(country = count)


euro_grid_70 <- ggplot(df[complete.cases(df$vax_rate)], aes(x, y)) +
  facet_geo(~country, grid = 'europe_countries_grid1', label = 'code') +
  geom_point(aes(colour = vax_rate, size = population/1000000 )) +
  scale_colour_gradient2(low = "red",
                         mid = 'blue',
                       high = "blue",
                       midpoint = 70)+
  labs(x = '', y = '', title = '',
       size = 'Population per million', colour = 'Vaccination Rate (%)')+
  scale_size_continuous(breaks = c(25,50,75,100,125),
                        range = c(1,5))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

euro_grid_70

ggsave(
  filename ='euro_grid_70.png',
  device = 'png',
  path = 'plots',
  dpi = 'print'
)

euro_grid <- ggplot(df[complete.cases(df$vax_rate)], aes(x, y)) +
  facet_geo(~country, grid = 'europe_countries_grid1', label = 'code') +
  geom_point(aes(colour = vax_rate, size = population/1000000 )) +
  scale_colour_gradient(low ='white',
                        high = 'black')+
  labs(x = '', y = '')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
euro_grid

euro_grid_nopop <- ggplot(df[complete.cases(df$vax_rate)], aes(x, y)) +
  facet_geo(~country, grid = 'europe_countries_grid1', label = 'name') +
  geom_point(aes(colour = vax_rate), size = 5) +
  scale_colour_gradient(low ='grey',
                        high = 'black')+
  labs(x = '', y = '')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

euro_grid_nopop

euro_grid_nopop_70 <- ggplot(df[complete.cases(df$vax_rate)], aes(x, y)) +
  facet_geo(~country, grid = 'europe_countries_grid1', label = 'name') +
  geom_point(aes(colour = vax_rate), size = 5) +
  scale_colour_gradient2(low ='yellow',
                         mid = 'black',
                        high = 'black',
                        midpoint = 70)+
  labs(x = '', y = '')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
euro_grid_nopop_70

library(lemon)
knit_print.data.frame <- lemon_print


