###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

## ---- packages --------
#load needed packages
library(here) 
library(tidymodels)
library(tidyverse)
library(ggplot2)


## ---- loaddata --------
#path to data
data_location <- here::here("data","processed_data","processeddata.rda")

#load data. 
load(data_location)

## ---- fitmodel1 ---------
######################################
#Data fitting/statistical analysis
######################################


# fit linear model using TB incidence as outcome, subgroup as predictor
lm_mod <- linear_reg() #set linear regression

#fit model with high burden countries
inclm_fit <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup,
      data = highburden) #fit linear model
incfittable <- tidy(inclm_fit) 
print(incfittable) #produce tidy table of fitted model

table_file1 = here("results", "model fit tables", "incfittable.rds")
saveRDS(incfittable, file = table_file1) #save summary table

#fit model with full data
inclm_fitFD <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup,
      data = wide_data) #fit linear model
incfittable2 <- tidy(inclm_fitFD) 
print(incfittable2) #produce tidy table of fitted model

table_file2 = here("results", "model fit tables","incfittableFD.rds")
saveRDS(incfittable2, file = table_file2) #save summary table

## ---- pred1 ---------
#prediction model using subgroup and setting
predinclm_fit <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup + setting,
      data = highburden) #fit linear model
predincfittable <- tidy(predinclm_fit) 
print(predincfittable)

new_points <- expand.grid(subgroup = c("Female", "Male"),
                          setting = "India") #predicting Indian incidence based on sex
incpred <- predict(predinclm_fit,
                   new_data = new_points) #predictions
CIincpred <- predict(predinclm_fit,
                     new_data = new_points,
                     type = "conf_int") #confidence intervals for predictions
preddata <- new_points %>% #create plot data
  bind_cols(incpred) %>% 
  bind_cols(CIincpred)

predplot <- ggplot(preddata, aes(subgroup))+
  geom_point(aes(y = .pred))+ #plot predictions with confidence intervals
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = .2) +
  labs(y = "TB Incidence (per 100 000 population)")
predplot

saveRDS(predplot, file = here("results", "model fit tables", "prediction1.rds"))

## ---- fitmodel2 ---------
# fit linear model using TB mortality as outcome, subgroup and setting as predictor

#fit model with high burden countries
mortlm_fit <- lm_mod %>% 
  fit(`TB mortality (deaths per 100 000 population)` ~ subgroup,
      data = highburden) #fit linear model
mortfittable <- tidy(mortlm_fit) 
print(mortfittable) #produce tidy table of fitted model

table_file3 = here("results", "model fit tables", "mortfittable.rds")
saveRDS(mortfittable, file = table_file3) #save summary table

#fit model with full data
mortlm_fitFD <- lm_mod %>% 
  fit(`TB mortality (deaths per 100 000 population)` ~ subgroup,
      data = wide_data) #fit linear model
mortfittableFD <- tidy(mortlm_fitFD) 
print(mortfittableFD) #produce tidy table of fitted model

table_file4 = here("results", "model fit tables", "mortfittableFD.rds")
saveRDS(mortfittableFD, file = table_file4)
  
## ---- fitmodel3 ---------
# fit linear model TB prevalence as outcome, subgroups as predictors

#fit model with high burden countries
prevlm_fit <- lm_mod %>% 
  fit(`TB prevalence (cases per 100 000 population)` ~ subgroup,
      data = highburden) #fit linear model
prevfittable <- tidy(prevlm_fit) 
print(prevfittable) #produce tidy table of fitted model

table_file5 = here("results", "model fit tables", "prevfittable.rds")
saveRDS(prevfittable, file = table_file5) #save summary table

#fit model with full data
prevlm_fitFD <- lm_mod %>% 
  fit(`TB prevalence (cases per 100 000 population)` ~ subgroup,
      data = wide_data) #fit linear model
prevfittableFD <- tidy(prevlm_fitFD) 
print(prevfittableFD) #produce tidy table of fitted model

table_file6 = here("results", "model fit tables", "prevfittableFD.rds")
saveRDS(prevfittableFD, file = table_file6)

## ---- fitmodel4 ---------
# fit linear model with BCG coverage as outcome, subgroups as predictors

#fit model with high burden countries
BCGlm_fit <- lm_mod %>% 
  fit(`BCG immunization coverage among one-year-olds (%)` ~ subgroup,
      data = highburden) #fit linear model
BCGfittable <- tidy(BCGlm_fit) 
print(BCGfittable) #produce tidy table of fitted model

table_file7 = here("results", "model fit tables", "BCGfittable.rds")
saveRDS(BCGfittable, file = table_file7) #save summary table

#fit model with full data
BCGlm_fitFD <- lm_mod %>% 
  fit(`BCG immunization coverage among one-year-olds (%)` ~ subgroup,
      data = wide_data) #fit linear model
BCGfittableFD <- tidy(BCGlm_fitFD) 
print(BCGfittableFD) #produce tidy table of fitted model

table_file8 = here("results", "model fit tables", "BCGfittableFD.rds")
saveRDS(BCGfittableFD, file = table_file8)

## ---- fitmodel5 ---------
# fit linear model with catastrophic costs as outcome, subgroups as predictors

#fit model with high burden countries
costlm_fit <- lm_mod %>% 
  fit(`Families affected by TB facing catastrophic costs due to TB (%)` ~ subgroup,
      data = highburden) #fit linear model
costfittable <- tidy(costlm_fit) 
print(costfittable) #produce tidy table of fitted model

table_file9 = here("results", "model fit tables", "costfittable.rds")
saveRDS(costfittable, file = table_file9) #save summary table

#fit model with full data
costlm_fitFD <- lm_mod %>% 
  fit(`Families affected by TB facing catastrophic costs due to TB (%)` ~ subgroup,
      data = wide_data) #fit linear model
costfittableFD <- tidy(costlm_fitFD) 
print(costfittableFD) #produce tidy table of fitted model

table_file10 = here("results", "model fit tables", "costfittableFD.rds")
saveRDS(costfittableFD, file = table_file10)

## ---- fitmodel6 ---------
# fit linear model with case detection rate as outcome, subgroups as predictors

#fit model with high burden countries
cdrlm_fit <- lm_mod %>% 
  fit(`Case detection rate (%)` ~ subgroup,
      data = highburden) #fit linear model
cdrfittable <- tidy(cdrlm_fit) 
print(cdrfittable) #produce tidy table of fitted model

table_file11 = here("results", "model fit tables", "cdrfittable.rds")
saveRDS(cdrfittable, file = table_file11) #save summary table

#fit model with full data
cdrlm_fitFD <- lm_mod %>% 
  fit(`Case detection rate (%)` ~ subgroup,
      data = wide_data) #fit linear model
cdrfittableFD <- tidy(cdrlm_fitFD) 
print(cdrfittableFD) #produce tidy table of fitted model

table_file12 = here("results", "model fit tables", "cdrfittableFD.rds")
saveRDS(cdrfittableFD, file = table_file12)

## ---- fitmodel7 ---------
# fit linear model with prevalence to notification ratio as outcome, subgroups as predictors

#fit model with high burden countries
pnrlm_fit <- lm_mod %>% 
  fit(`Prevalence to notification ratio (years)` ~ subgroup,
      data = highburden) #fit linear model
pnrfittable <- tidy(pnrlm_fit) 
print(pnrfittable) #produce tidy table of fitted model

table_file13 = here("results", "model fit tables", "pnrfittable.rds")
saveRDS(pnrfittable, file = table_file13) #save summary table

#fit model with full data
pnrlm_fitFD <- lm_mod %>% 
  fit(`Prevalence to notification ratio (years)` ~ subgroup,
      data = wide_data) #fit linear model
pnrfittableFD <- tidy(pnrlm_fitFD) 
print(pnrfittableFD) #produce tidy table of fitted model

table_file14 = here("results", "model fit tables", "pnrfittableFD.rds")
saveRDS(pnrfittableFD, file = table_file14)

## ---- fitmodel8 ---------
# fit linear model with male attitude as outcome, subgroups as predictors

#fit model with high burden countries
attlm_fit <- lm_mod %>% 
  fit(`People who would want a family member's TB kept secret - Male (%)` ~ subgroup,
      data = highburden) #fit linear model
attfittable <- tidy(attlm_fit) 
print(attfittable) #produce tidy table of fitted model

table_file15 = here("results", "model fit tables", "attfittable.rds")
saveRDS(attfittable, file = table_file15) #save summary table

#fit model with full data
attlm_fitFD <- lm_mod %>% 
  fit(`People who would want a family member's TB kept secret - Male (%)` ~ subgroup,
      data = wide_data) #fit linear model
attfittableFD <- tidy(attlm_fitFD) 
print(attfittableFD) #produce tidy table of fitted model

table_file16 = here("results","model fit tables", "attfittableFD.rds")
saveRDS(attfittableFD, file = table_file16)

## ---- fitmodel9 ---------
# fit linear model with male knowledge as outcome, subgroups as predictors

#fit model with high burden countries
knowlm_fit <- lm_mod %>% 
  fit(`People who report TB is spread through coughing - Male (%)` ~ subgroup,
      data = highburden) #fit linear model
knowfittable <- tidy(knowlm_fit) 
print(knowfittable) #produce tidy table of fitted model

table_file17 = here("results", "model fit tables", "knowfittable.rds")
saveRDS(knowfittable, file = table_file17) #save summary table

#fit model with full data
knowlm_fitFD <- lm_mod %>% 
  fit(`People who report TB is spread through coughing - Male (%)` ~ subgroup,
      data = wide_data) #fit linear model
knowfittableFD <- tidy(knowlm_fitFD) 
print(knowfittableFD) #produce tidy table of fitted model

table_file18 = here("results", "model fit tables", "knowfittableFD.rds")
saveRDS(knowfittableFD, file = table_file18)
