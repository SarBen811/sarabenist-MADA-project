###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rda file in the processed_data folder
#



## ---- packages --------
#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing/cleaning
library(tidyr) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here) #to set paths
library(naniar) #to look for missing data
library(pillar)

## ---- loaddata --------
#path to data
data_location <- here::here("data","raw_data","202206-repository-tb.xlsx")

#load data and assign to rawdata
rawdata <- readxl::read_excel(data_location)


## ---- exploredata1 --------
#take a look at the data
dplyr::glimpse(rawdata)

#another view of the data
skimr::skim(rawdata)

#check for missing data by each column
gg_miss_var(rawdata)

## ---- exploredata2 --------
#look at what indicators are in the dataset
unique(rawdata$indicator_name)

#count of the number of countries
length(unique(rawdata$setting))


## ---- cleandata1 --------
# Pivot data wider so each indicator has a column
 wide_data <- rawdata %>% 
  select(c(setting, year, indicator_name,
           indicator_abbr, dimension, subgroup,
           estimate, population)) %>% 
  pivot_wider(names_from = "indicator_name", values_from = "estimate")
summarywide <- tbl_sum(wide_data)
data1 = here("results", "processing", "summarywide.rds")
saveRDS(summarywide, file = data1)

#subset high burden countries
highburden <- wide_data %>% 
  filter(setting %in% c("Brazil", "Central African Republic", "Congo",
           "Ethiopia", "Gabon", "Kenya",
           "Lesotho", "Liberia", "Namibia",
           "Thailand", "Uganda", "United Republic of Tanzania",
           "Botswana", "Cameroon", "Eswatini",
           "Guinea", "Guinea-Bissau", "Malawi",
           "Russian Federation", "Zimbabwe", "China",
           "Democratic Republic of the Congo", "India",
           "Indonesia", "Mozambique", "Myanmar",
           "Nigeria", "Philippines", "South Africa",
           "Zambia", "Sierra Leone", "Angola",
           "Bangladesh", "Democratic People's Republic of Korea",
           "Mongolia", "Pakistan", "Papua New Guinea",
           "Viet Nam", "Azerbaijan", "Belarus", "Kazakhstan",
           "Kyrgyzstan", "Nepal", "Peru", "Republic of Moldova",
           "Somalia", "Tajikistan", "Ukraine",
           "Uzbekistan", "Zimbabwe")) 
unique(highburden$setting)
summaryhb <- tbl_sum(highburden)
data2 = here("results", "processing", "summaryhighburden")
saveRDS(summaryhb, file = data2)

## ---- cleandata2 --------
# look at each indicator to check for cleaning requirements
indicator <- unique(wide_data$indicator_abbr)
fig2 = here("results", "processing", "indicators.rds")
saveRDS(indicator, file = fig2)

# BCG coverage indicator
wide_data %>% 
  filter(indicator_abbr == "bcg") %>% 
  select(c(1,2,3,4,5,6,7)) %>% 
  summary()

# MDR-TB percentage indicator
wide_data %>% 
  filter(indicator_abbr == "drug_resistance") %>% 
  select(c(1,2,3,4,5,6,14)) %>% 
  summary()

# case detection rate indicator
wide_data %>% 
  filter(indicator_abbr == "cdr") %>% 
  select(c(1,2,3,4,5,6,11)) %>% 
  summary()

## ---- cleandata3 --------
# TB spread by cough
wide_data %>% 
  filter(indicator_abbr == "tb_cough") %>% 
  select(c(1,2,3,4,5,6,8)) %>% 
  summary()

# TB spread by cough - female
wide_data %>% 
  filter(indicator_abbr == "tb_cough_f") %>% 
  select(c(1,2,3,4,5,6,9)) %>% 
  summary()

# TB spread by cough - male
wide_data %>% 
  filter(indicator_abbr == "tb_cough_m") %>% 
  select(c(1,2,3,4,5,6,10)) %>% 
  summary()

## ---- cleandata4 --------
# TB incidence
wide_data %>% 
  filter(indicator_abbr == "incidence") %>% 
  select(c(1,2,3,4,5,6,12)) %>% 
  summary()

# TB mortality
wide_data %>% 
  filter(indicator_abbr == "mortality") %>% 
  select(c(1,2,3,4,5,6,13)) %>% 
  summary()

## ---- cleandata5 --------
# Desire to keep TB of family member a secret
wide_data %>% 
  filter(indicator_abbr == "tb_att") %>% 
  select(c(1,2,3,4,5,6,15)) %>% 
  summary()

# Male desire to keep TB of family member a secret
wide_data %>% 
  filter(indicator_abbr == "tb_att_m") %>% 
  select(c(1,2,3,4,5,6,16)) %>% 
  summary()

# Female desire to keep TB of family member a secret
wide_data %>% 
  filter(indicator_abbr == "tb_att_f") %>% 
  select(c(1,2,3,4,5,6,20)) %>% 
  summary()

## ---- cleandata6 --------
# TB prevalence
wide_data %>% 
  filter(indicator_abbr == "prevalence_place") %>% 
  select(c(1,2,3,4,5,6,17)) %>% 
  summary()

# Prevalence to notification ratio
wide_data %>% 
  filter(indicator_abbr == "p:n") %>% 
  select(c(1,2,3,4,5,6,18)) %>% 
  summary()

## ---- cleandata7 --------
# Percentage suffering catastrophic costs due to TB
wide_data %>% 
  filter(indicator_abbr == "catacost") %>% 
  select(c(1,2,3,4,5,6,19)) %>% 
  summary()

## ---- savedata --------
# all done, data is clean now. 
# Let's assign at the end to some final variable
# makes it easier to add steps above

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rda")
save(wide_data, highburden, file = save_data_location)



## ---- notes --------
# anything you don't want loaded into the Quarto file but 
# keep in the R file, just give it its own label and then don't include that label
# in the Quarto file

# Saving data as RDS:
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata



