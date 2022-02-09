# Metadata editor to cut down to important variables

library(readr)
library(knitr)
metadata <- read_csv("metadata/metadata.csv")

# gdp per cap X
# phc 1X
# phc 2X
# phc 3X
# giniX
# literacyX
# education primaryX
# education secondaryX
# health spending X
# hospital beds / thousandX
# nurses / midwives / thousandX
# docs / thousandX
# gii 
# gpiX
# ethnic fracX
# linguistic fracX
# religious fracX
# refugeeX
# gov effectivenessX
# stringency indexX
# cpiX
# education bachelorX

sub_meta<- metadata[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 20, 21, 22, 23),]

new_row <- c("Gender Inequality Index (GII)",
                                  "Shows the loss in potential human development due to disparity between female and male achievements in three dimensions: reproductive health, empowerment and the labour market. Overall, the GII reflects how women are disadvantaged in these dimensions.",
                                  "United Nations Development Program (UNDP)",
                                  "Social")
sub_meta <- rbind(sub_meta, new_row)
metadata <- rbind(metadata, new_row)

sub_meta$`Missing Values` <- c('0', '0', '4', '4', '4', '4', '0', '3', '8', '2', '7', '0', '0', '0', '2', '3', '2', '2', '2', '3', '3', '4')

sub_meta <- sub_meta[,c(1, 3, 5)]

write.csv(metadata, 'metadata/metadata.csv')

kable(sub_meta,caption='Abridged Metadata')

write.csv(sub_meta, 'metadata/sub_meta.csv')

sub_meta_table <- table(sub_meta)

sub_meta_table

sub_meta$Source <- c("World Bank/OECD", "WHO", "World Bank", "World Bank", "World Bank", "World Bank", "UNESCO", "UNESCO", "UNESCO", "UNESCO", "UNESCO", "WHO", "WHO", "WHO", "UNHCR", "Transparency International", "Pew Research, Harvard University", "Pew Research, Harvard University", "Pew Research, Harvard University", "Oxford COVID-19 GOvernment Response Tracker", "World Bank", "UNDP")

library(ggplot2)
library(gridExtra)

# c("World Bank/OECD", "WHO", "World Bank", "World Bank", "World Bank", "World Bank", "UNESCO", "UNESCO", "UNESCO", "UNESCO", "UNESCO", "WHO", "WHO", "WHO", "UNHCR", "Transparency International", "Pew Research, Harvard University", "Pew Research, Harvard University", "Pew Research, Harvard University", "Oxford COVID-19 GOvernment Response Tracker", "World Bank", "UNDP")
png("metadata/test.png", height = 500, width=1000)
p<-tableGrob(sub_meta)
grid.arrange(p)
dev.off()
