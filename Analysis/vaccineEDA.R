## #############################################################################
## # EDA Vaccination Rates
## # WHO Data Challenge Project
## #############################################################################
##

df <- read.csv('data/merged_data.csv')

#install.packages('geofacet')
#install.packages('lemon')
#install.packages('kableExtra')

library(ggplot2)
library(geofacet)
library(dplyr)
library(kableExtra)
library(tidyr)

euro_count <- read.csv('data/euro_count_dict.csv')

df <- df %>% mutate(vax_rate = coalesce(percent_fully_vaccinated_total_pop*100, people_fully_vaccinated_per_hundred))

df <- df[!df$Country.x == 'Liechtenstein',]
df <- df[!df$Country.x == 'Kosovo',]

df <- df %>% mutate(x = rep(1, nrow(df)))
df <- df %>% mutate(y = rep(1, nrow(df)))

## summary statistics ####

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

## Table of vaccinations ####

table <- df %>% 
  select('Country' = Country.x, 
         'Code' = `Country.Code`,
         'Vaccination rate' = vax_rate,
         'Total Population' = population)

table %>%
  kbl(caption = 'Vaccination rate and Population of WHO Europe Countries') %>%
  kable_classic() %>%
  kable_styling() %>%
  save_kable(file = "plots/table.pdf", self_contained = T)

## Vulnerable population bar chart ####
vul_pop <- df %>%
  select(`Country.x`, colnames(df)[c(59,64,69,71)])

vul_pop <- vul_pop[complete.cases(vul_pop),]
vul_pop <- vul_pop[with(vul_pop, order(-vax_rate)),]
vul_pop$vax_rate <- vul_pop$vax_rate/100

vul_pop_long <- pivot_longer(
  data            = vul_pop,
  cols            = colnames(df)[c(59,64,69,71)],
  names_to        = 'group',
  values_to       = 'vaccine')

vul_pop_long$group <- factor(vul_pop_long$group, labels = c("Aged above 60 years",
                                                            "Healthcare Worker",
                                                            'Long Term Care Facilities',
                                                            'General Population'))


vul_pop_grid <- ggplot(vul_pop_long)+
  geom_bar(aes(x = group, y = vaccine*100, fill = group), position = "stack", stat = "identity")+
  facet_wrap(~Country.x)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_blank(),
        strip.background =element_rect(fill="black"),
        strip.text = element_text(colour = 'white'))+
  labs(
    y = 'Vaccination rate (%)',
    fill = 'Population Groups')

vul_pop_grid

##world map ####
library(grid)
library(rworldmap)
worldMap <- getMap()

count <- worldMap$NAME[which(worldMap$NAME%in%df$Country.x)]

setdiff(df$Country.x,count)

df$Country.x[df$Country.x == 'Bosnia and Herzegovina'] <- 'Bosnia and Herz.'
df$Country.x[df$Country.x == 'Czech Republic'] <- 'Czech Rep.'
df$Country.x[df$Country.x == 'Kyrgyz Republic'] <- 'Kyrgyzstan'
df$Country.x[df$Country.x == 'North Macedonia'] <- 'Macedonia'
df$Country.x[df$Country.x == 'Russian Federation'] <- 'Russia'
df$Country.x[df$Country.x == 'Slovak Republic'] <- 'Slovakia'

count <- worldMap$NAME[which(worldMap$NAME%in%df$Country.x)]
setdiff(df$Country.x,count)

indEuro <- which(worldMap$NAME%in%df$Country.x)

euroCoords <- lapply(indEuro, function(i){
  euro_df <- 
    data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  euro_df$region = as.character(worldMap$NAME[i])
  colnames(euro_df) <- list('long','lat','region')
  return(euro_df)
})

euroCoords <- do.call('rbind',euroCoords)

europeUnionTable <- data.frame(country = df$Country.x, value = df$vax_rate)

euroCoords$value <- europeUnionTable$value[match(euroCoords$region,europeUnionTable$country)]

region.lab.data <- euroCoords[,-4] %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) %>%
  filter(region %in% c('United Kingdom','France','Germany','Kazakhstan','Sweden','Ukraine','Bosnia and Herz','Turkey','Hungary','Poland','Lithuania',
                       'Spain','Italy','Israel','Iceland','Finland','Turkmenistan','Tajikistan','Greece','Croatia','Serbia','Andorra','Estonia','Belarus',
                       'Georgia','Uzebekistan','Kyrgyzstan','Netherlands')) %>%
  rbind(c('Russia',65.105951,58.5067))

region.lab.data[2:3] <- lapply(region.lab.data[2:3], as.numeric)

map_plot <- ggplot() +
  geom_polygon(data = euroCoords,
               aes(x = long, y= lat, group = region, fill = value),
               colour = "black", size = 0.3)+
  scale_fill_gradient2(low = "#cc0000",
                       mid = '#3366ff',
                       high = "black",
                       midpoint = 70,
                       limits = c(0,100))+
  theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
    #panel.background = element_rect(fill = NA, colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    #rect = element_blank(),
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+
  coord_map(xlim = c(-20, 80),  ylim = c(32, 71))+
  labs(x = '', y = '', title = '',
       fill = 'Vaccination Rate (%)')+
  geom_label(aes(x = long, y = lat, label = region),
            data = region.lab.data,
            size = 2.75,
            check_overlap = TRUE,
            nudge_y = 0.8) +
  geom_point(aes(x = long, y = lat),
             data = region.lab.data)
map_plot

aspect_ratio <- 2.5
height <- 7

ggsave(
  filename ='map_plot.png',
  device = 'png',
  path = 'plots',
  dpi = 'print',
  height = 5 , width = 8
)
