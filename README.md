# WHO-Project

This work was produced for WHO as part of the Data Challenge project at LSHTM.

## Installation
You can install the necessary packages with:
```{r message = FALSE, warning = FALSE}
install.packages("dplyr")
install.packages("ggplot2")
install.packages("xlsx")
install.packages("tidyverse")
install.packages("corrplot") #Plot correlation matrix 
install.packages("fabricatr") #Convert outcome variable to quartiles 
install.packages("betareg") #Run beta regression 
install.packages("mctest")
install.packages("stringr")
install.packages("data.table")
install.packages("kableExtra")
webshot::install_phantomjs()
install.packages("htmlTable")
install.packages("tidyr")
install.packages("extrafont")
install.packages("ggpubr")
install.packages("geofacet")
install.packages("lemon")

```
## Initial setup
To run the script:
1. Set working directory to source file location
2. Select all code and press run

## Introduction

This project aims to identify the most important country-level predictors that could reduce COVID-19 mortality and morbidity, one of four vaccination programme goals set by WHO. This can be achieved by increasing COVID-19 vaccine uptake and increasing coverage in vulnerable groups within the countries of the European region, as identified by WHO. The list of EU region countries can be found from: https://www.euro.who.int/en/countries. The three highest-priority groups identified by WHO include: Older adults, Healthcare workers, and Long-term care facilities residents.

This repository contains information needed to identify these predictors. The information has been divided into different folders and are as follows:
1. metadata
2. data
3. CleanData
4. Analysis


## Metadata
This folder holds the script used to generate the comma-separated value file containing indicator names and definitions for the merged dataset.

1. editor.R
2. metadata.csv

## Data
This folder holds all the raw data collected from various data sources, and the merged dataset. Data sources used: The World Bank, EU CDC, Our World in Data (OWID), UNDP, World Population Review.

1. OWD_child.csv
2. OWD_flu65.csv
3. corruption-indicator.xlsx
4. country-level-predictors.csv
5. covid-stringency-index.csv
6. cross-country-literacy-rates.csv
7. data_undp_org_all.csv
8. ethnicfrac.csv
9. eu_vaccine.csv
10. euro_count_dict.csv
11. gender_inequality.xlsx
12. iso_2digit_alpha_country_codes.xls
13. merged_data.csv
14. owid-covid-data.xlsx
15. vaccinations.csv
16. wgidataset.xlsx

These files have been merged in the "DataMERGE.R" script.
The merged dataset can be found under "merged_data.csv".

## CleanData
This folder holds the scripts used to clean the raw data from sources. Also in this folder, is the 'DataMerge.R' file which contains the code needed to combine the cleaned data into one dataset that can be used for analysis.

1. DataMERGE.R
2. corruption_CLEAN.R
3. covid_stringencyCLEAN.R
4. eth_fracCLEAN.R
5. eu_vaccine_R_script.R
6. owidcovidCLEAN.R
7. vax_childmmrCLEAN.R
8. vax_fluCLEAN.R
9. wgiCLEAN.R
10. worldbankCLEAN.R


## Analysis
This folder holds the scripts used to perform 
1. Modelling.R

//  
2. predictEDA.R
// summary table, mean and standard deviation for each selected country-level predictor 
3. subpop_plots.R
//
4. vaccineEDA.R





