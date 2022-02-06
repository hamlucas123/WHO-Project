# WHO-Project
WHO Data Challenge project for LSHTM
(to do: include description of files)

This file contains information on how to run the R scripts for the project.
These scripts are used to clean and process the raw data from various sources to investigate country-level predictors that could increase COVID-19 vaccine uptake within EU countries as identified by WHO.

The list of EU countries can be found from: https://www.euro.who.int/en/countries
Data sources used: The World Bank, EU CDC, Our World in Data (OWID), UNDP, World Population Review

///////////////////////////////////////////////////////////////

To generate the analysis, the following scripts are to be used:
1. Modelling.R
// 
2. predictEDA.R
// summary table, mean and standard deviation for each selected country-level predictor 
3. subpop_plots.R
//
4. vaccineEDA.R
//

//////////////////////////////////////////////////////////////

Please install the required packages by running the following codes:
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


To run the script:
1. Set working directory to source file location
2. Select all code and press run

///////////////////////////////////////////////////////////////

Folder: data
The following files are the resulting datasets after cleaning:
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
