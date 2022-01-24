# country namnes from WHO
euro_count<-c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
              "Belarus", "Belgium", "Bosnia and Herzegovina","Bulgaria",
              "Croatia", "Cyprus", "Czechia", 
              "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany",
              "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy",
              "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Luxembourg",
              "Malta", "Moldova (Republic of Moldova)", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
              "Norway", "Poland", "Portugal", "Romania", 
              "Russian Federation", "San Marino", "Serbia", "Slovakia", "Slovenia",
              "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan",
              "Ukraine", "United Kingdom of Great Britain and Northern Ireland", "Uzbekistan")

# creating df, columns named
eu_heads <- c("Country", "ISO3", "ISO2", "Other Name Variations")
euro_count_dict <- as.data.frame(matrix(,ncol = 4, nrow=53))
names(euro_count_dict) <- eu_heads

#replace column with euro_count sorted alphabetically
euro_count_dict$Country <- sort(euro_count)

# code used to find missing countries 
euro_count[!(euro_count %in% iso2_code$Definition)]

# for iso3 codes
iso3_code <- read.xlsx("~/LSHTM_DC/CLASS.xls", 1)
iso3_missing <- iso3_code %>% #some names don't matched, need to specifically filter 
  filter( NA..1 %in% c("Czech Republic","Kyrgyz Republic", "Moldova", "Slovak Republic", "United Kingdom"))

# for iso2 codes
iso2_code <- read.xlsx("~/LSHTM_DC/iso_2digit_alpha_country_codes.xls",1)
colnames(iso2_code) <- iso2_code[1,]
iso2_code <- tail(iso2_code,-1)
iso2_missing <- iso2_code %>%
  filter(Definition %in% c("Kazakstan", "Moldova, Republic of", "Macedonia, The Former Yugoslav Republic Of", 
                           "Russia Federation", "Republic of Serbia"))

# filter for eu countries 
euiso3_code <- iso3_code %>%
  filter(NA..1 %in% euro_count) %>%
  bind_rows(iso3_missing) %>%
  select(NA..1, NA..2) %>%
  arrange(NA..1)
names(euiso3_code) <- c("Country", "ISO3")

euiso2_code <- iso2_code %>%
  filter(Definition %in% euro_count_dict$Country | Definition %in% euro_count_dict$`Other Name Variations`) %>%
  bind_rows(iso2_missing) %>%
  arrange(Definition)
#reordering rows so it matches with dict
euiso2_code <- euiso2_code[c(1:29,31:35,30,36:38,40:42,39,43:nrow(euiso2_code)),]


#putting data into euro_count_dict
euro_count_dict$ISO3 <- euiso3_code$ISO3
euro_count_dict$ISO2 <- euiso2_code$`Code Value`
euro_count_dict$`Other Name Variations` <-ifelse(!(euro_count_dict$Country==euiso3_code$Country), 
                                                 paste(euiso3_code$Country), NA)
euro_count_dict$`Other Name Variations1` <-ifelse(!(euro_count_dict$Country==euiso2_code$Definition)&
                                                    !(euro_count_dict$`Other Name Variations`==euiso2_code$Definition), 
                                                  paste(euiso2_code$Definition), NA)

save(euro_count_dict, file=("data/euro_count_dict.csv")) 
