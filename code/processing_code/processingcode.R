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
slice(wide_data)


## ---- cleandata2 --------
# create new subsets by indicator since I want to compare 
# each indicator to TB outcomes
unique(wide_data$indicator_abbr)

# BCG coverage indicator
bcg <- wide_data %>% 
  filter(indicator_abbr == "bcg") %>% 
  select(c(1,2,3,4,5,6,7))
summary(bcg)

# MDR-TB percentage indicator
drug_resistance <- wide_data %>% 
  filter(indicator_abbr == "drug_resistance") %>% 
  select(c(1,2,3,4,5,6,14)) 
summary(drug_resistance)

# case detection rate indicator
cdr <- wide_data %>% 
  filter(indicator_abbr == "cdr") %>% 
  select(c(1,2,3,4,5,6,11))
summary(cdr)

## ---- cleandata3 --------
# TB spread by cough
tb_cough <- wide_data %>% 
  filter(indicator_abbr == "tb_cough") %>% 
  select(c(1,2,3,4,5,6,8))
summary(tb_cough)

# TB spread by cough - female
tb_cough_f <- wide_data %>% 
  filter(indicator_abbr == "tb_cough_f") %>% 
  select(c(1,2,3,4,5,6,9))
summary(tb_cough_f)

# TB spread by cough - male
tb_cough_m <- wide_data %>% 
  filter(indicator_abbr == "tb_cough_m") %>% 
  select(c(1,2,3,4,5,6,10))
summary(tb_cough_m)

## ---- cleandata4 --------
# TB incidence
incidence <- wide_data %>% 
  filter(indicator_abbr == "incidence") %>% 
  select(c(1,2,3,4,5,6,12))
summary(incidence)

# TB mortality
mortality <- wide_data %>% 
  filter(indicator_abbr == "mortality") %>% 
  select(c(1,2,3,4,5,6,13))
summary(mortality)

## ---- cleandata5 --------
# Desire to keep TB of family member a secret
tb_att <- wide_data %>% 
  filter(indicator_abbr == "tb_att") %>% 
  select(c(1,2,3,4,5,6,15)) 
summary(tb_att)

# Male desire to keep TB of family member a secret
tb_att_m <- wide_data %>% 
  filter(indicator_abbr == "tb_att_m") %>% 
  select(c(1,2,3,4,5,6,16)) 
summary(tb_att_m)

# Female desire to keep TB of family member a secret
tb_att_f <- wide_data %>% 
  filter(indicator_abbr == "tb_att_f") %>% 
  select(c(1,2,3,4,5,6,20))
summary(tb_att_f)

## ---- cleandata6 --------
# TB prevalence
prevalence_place <- wide_data %>% 
  filter(indicator_abbr == "prevalence_place") %>% 
  select(c(1,2,3,4,5,6,17))
summary(prevalence_place)

# Prevalence to notification ratio
pn <- wide_data %>% 
  filter(indicator_abbr == "p:n") %>% 
  select(c(1,2,3,4,5,6,18))
summary(pn)

## ---- cleandata7 --------
# Percentage suffering catastrophic costs due to TB
catacost <- wide_data %>% 
  filter(indicator_abbr == "catacost") %>% 
  select(c(1,2,3,4,5,6,19))
summary(catacost)


## ---- cleandata8 --------
# address missing data
# BCG missing population & bcg data 
# due to missing info for dimension levels
bcg <- bcg %>% 
  drop_na()
# cdr missing data for cdr & pop nas
# due to missing age group levels
cdr <- cdr %>% 
  drop_na()
# tb_cough missing data for pop & cough % 
# due to missing sex levels
tb_cough <- tb_cough %>% 
  drop_na()
# tb_cough f missing data for pop & cough % 
# due to missing education levels
tb_cough_f <- tb_cough_f %>% 
  drop_na()
# tb_cough m missing data for pop & cough % 
# due to missing education levels
tb_cough_m <- tb_cough_m %>% 
  drop_na()
# tb_att missing data for pop & attitude % 
# due to missing sex levels
tb_att <- tb_att %>% 
  drop_na()
# tb_att m missing data for pop & attitude % 
# due to missing education levels
tb_att_m <- tb_att_m %>% 
  drop_na()
# tb_att f missing data for pop & attitude % 
# due to missing education levels
tb_att_f <- tb_att_f %>% 
  drop_na()

## ---- cleandata9 --------
# prevalence no pop data
prevalence_place <- prevalence_place %>% 
  select(-population)
# pn no pop data
pn <- pn %>% 
  select(-population)
# catacost missing pop data but
# will not drop because not completely missing


## ---- savedata --------
# all done, data is clean now. 
# Let's assign at the end to some final variable
# makes it easier to add steps above

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rda")
save(bcg, catacost, cdr,
        drug_resistance, incidence,
        mortality, pn, prevalence_place,
        tb_att, tb_att_f, tb_att_m,
        tb_cough, tb_cough_f, tb_cough_m, wide_data, file = save_data_location)



## ---- notes --------
# anything you don't want loaded into the Quarto file but 
# keep in the R file, just give it its own label and then don't include that label
# in the Quarto file

# Saving data as RDS:
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata



