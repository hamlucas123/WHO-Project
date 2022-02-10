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
install.packages("readr")
install.packages("readxl")
install.packages("lubridate")
install.packages("gridExtra")
```
## Initial setup
Please refer to the Analysis section to see which scripts to run.


To run the script:
1. Set working directory to source file location
2. Select all code and press run

## Introduction

This project aims to identify the most important country-level predictors that could reduce COVID-19 mortality and morbidity, one of four vaccination programme goals set by WHO. This can be achieved by increasing COVID-19 vaccine uptake and increasing coverage in vulnerable groups within the countries of the European region, as identified by WHO. The list of EU region countries can be found from: https://www.euro.who.int/en/countries. The highest-priority groups identified by WHO include: Older adults, Healthcare workers due to their increased risk of mortality; this project will also look into Long-term care facilities residents, a high-risk group identified by ECDC.

This repository contains information needed to identify these predictors and the outputs of relevant scripts. The information has been divided into different folders and are as follows:
1. metadata
2. data
3. CleanData
4. Analysis
5. plots
6. tables


## Metadata
This folder holds the script used to generate the comma-separated value file containing indicator names and definitions for the merged dataset.

1. editor.R
2. metadata.csv

## Data
This folder holds all the raw data collected from various data sources, and the merged dataset. Data sources used: The World Bank, EU CDC, Our World in Data (OWID), Organisation for Economic Co-operation and Development (OECD), United Nations Educational Scientific and Cultural Organisation (UNESCO), United Nations Development Programme (UNDP), World Population Review, Luxembourg Income Study database, The Worldwide Governance Indicator (WGI), The World Population Review, Transparency International.

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
5. euro_count_dict.R
6. eu_vaccine_R_script.R
7. owidcovidCLEAN.R
8. vax_childmmrCLEAN.R
9. vax_fluCLEAN.R
10. wgiCLEAN.R
11. worldbankCLEAN.R


## Analysis
This folder holds the scripts used to perform the analysis. 
1. Modelling.R

- Used to plot scatter plots between each of the potential country-level predictors and the outcome variable (proportion of fully vaccinated in the general population).
- Performs multiple univariable beta regression analysis between each potential predictor and outcome.
- Runs a correlation matrix among potential predictors and plots it.
- Runs a multivariable beta regression model with all potential predictors and calculates VIF andn tolerance scores (to determine multicollinearity).
- Searches for the best model by performing an exhaustive subset search among all covariates (given no multicollinearity) and ranks models based on R-Squared statistic.
- Runs likelihood ratio tests among the top 10 models to choose the final one. 


2. predictEDA.R

-  Tabulated country-level predictors according to quartiles of fully vaccinated countries 
-  For each country-level predictor, the number of countries included is shown. The mean and standard deviation are also calculated for each quartile.
-  Table of countries in each quartile produced 


3. subpop_plots.R

- Used to produce correlation matrices between the subpopulations and the predictors.
- Graphs showing the correlations between Older Adults and COVID vaccination uptake is also in this script.

4. vaccineEDA.R

- Summary statistics for vaccination
- Table of vaccination rates across WHO Europe countries
- Bar chart of vaccination rate in vulnerable populations
- World map figure of vaccination rate across WHO Europe countries

## Plots
This folder contains the graphical outputs from scripts in the Analysis folder.
1. Country_Quartile.png
2. euro_grid.png
3. euro_grid_70.png
4. map_plot.png
5. split_vax.pdf
6. table.html
7. vax_bar.png
8. vax_scatter.png
9. vul_plot_grid.png
10. vul_po.png
11. vul_pop_grid.png
12. vul_pop_plot.png

## Table
This folder contains the correlation tables provided by the subpop_plot.R script from the Analysis folder.
1. Correlation table Health Care Workers.csv
2. Correlation table LTCF.csv
3. Correlation table Older Adults.csv
4. Merge Corr table.csv


