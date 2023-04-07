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
library(rpart.plot)
library(vip)


## ---- loaddata --------
#path to data
data_location <- here::here("data","processed_data","processeddata.rda")

#load data. 
load(data_location)

## ---- splitdata --------
#split data into training and test sets for wide_data
wddatasplit <- initial_split(wide_data, prop = 3/4)
wdtest_data <- testing(wddatasplit)
wdtrain_data <- training(wddatasplit)

#set seed for reproducibility
set.seed(626)

#create cross-validation folds
wdfolds <- vfold_cv(wdtrain_data, v = 5)

#split data into training and test sets for highburden
hbdatasplit <- initial_split(highburden, prop = 3/4)
hbtest_data <- testing(hbdatasplit)
hbtrain_data <- training(hbdatasplit)

#set seed for reproducibility
set.seed(626)

#create cross-validation folds
hbfolds <- vfold_cv(hbtrain_data, v = 5)


## ---- wdincidencemodel ---------
#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                 data = wdtrain_data)
#create tree model
treemodel <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

#create work flow
incwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(increcipe)

#create grid of tuning parameters
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)
#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
inctree_resamp <- incwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
inctreeplot <- inctree_resamp %>% autoplot()
inctreeplot

#show the best model
inctree_resamp %>% 
  show_best()

#select best model
wdincbest_tree <- inctree_resamp %>% select_best("rmse")

#create final workflow
wdincfinal_wf <- incwflow %>% finalize_workflow(wdincbest_tree)

#fit the final workflow to the training data
wdincfinal_fit <- wdincfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdincfitted <- augment(wdincfinal_fit, wdtrain_data) %>% 
  select(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB incidence (new infections per 100 000 population)`, .pred)
wdincfitted

#fit final workflow to test data
wdincfittest <- wdincfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdincfittest %>% collect_metrics()

#check important variables of model
wdincfittest %>% extract_fit_parsnip() %>% vip()


## ---- hbincidencemodel ---------
#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                    data = hbtrain_data)
#create tree model
treemodel <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

#create work flow
incwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(increcipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
inctree_resamp <- incwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
inctreeplot <- inctree_resamp %>% autoplot()
inctreeplot

#show the best model
inctree_resamp %>% 
  show_best()

#select best model
hbincbest_tree <- inctree_resamp %>% select_best("rmse")

#create final workflow
hbincfinal_wf <- incwflow %>% finalize_workflow(hbincbest_tree)

#fit the final workflow to the training data
hbincfinal_fit <- hbincfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbincfitted <- augment(wdincfinal_fit, hbtrain_data) %>% 
  select(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB incidence (new infections per 100 000 population)`, .pred)
hbincfitted

#fit final workflow to test data
hbincfittest <- hbincfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbincfittest %>% collect_metrics()

#check important variables of model
hbincfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbincfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdmortalitymodel ---------
#recipe predicting TB mortality
mortrecipe <- recipe(`TB mortality (deaths per 100 000 population)` ~.,
                 data = wdtrain_data)
#create tree model
treemodel <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

#create workflow
mortwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(mortrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
morttree_resamp <- mortwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
morttreeplot <- morttree_resamp %>% autoplot()
morttreeplot

#show the best model
morttree_resamp %>% 
  show_best()

#select best model
wdmortbest_tree <- morttree_resamp %>% select_best("rmse")

#create final workflow
wdmortfinal_wf <- mortwflow %>% finalize_workflow(wdmortbest_tree)

#fit the final workflow to the training data
wdmortfinal_fit <- wdmortfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdmortfitted <- augment(wdmortfinal_fit, wdtrain_data) %>% 
  select(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB mortality (deaths per 100 000 population)`, .pred)
wdmortfitted
#fit final workflow to test data
wdmortfittest <- wdmortfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdmortfittest %>% collect_metrics()

#check important variables of model
wdmortfittest %>% extract_fit_parsnip() %>% vip()

## ---- hbmortalitymodel ---------
#recipe predicting TB mortality
mortrecipe <- recipe(`TB mortality (deaths per 100 000 population)` ~.,
                     data = hbtrain_data)
#create tree model
treemodel <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

#create workflow
mortwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(mortrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
morttree_resamp <- mortwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
morttreeplot <- morttree_resamp %>% autoplot()
morttreeplot

#show the best model
morttree_resamp %>% 
  show_best()

#select best model
hbmortbest_tree <- morttree_resamp %>% select_best("rmse")

#create final workflow
hbmortfinal_wf <- mortwflow %>% finalize_workflow(hbmortbest_tree)

#fit the final workflow to the training data
hbmortfinal_fit <- hbmortfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbmortfitted <- augment(hbmortfinal_fit, hbtrain_data) %>% 
  select(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB mortality (deaths per 100 000 population)`, .pred)
hbmortfitted
#fit final workflow to test data
hbmortfittest <- hbmortfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbmortfittest %>% collect_metrics()

#check important variables of model
hbmortfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbmortfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdprevalencemodel -----
#recipe predicting TB prevalence
prevrecipe <- recipe(`TB prevalence (cases per 100 000 population)` ~.,
                 data = wdtrain_data)

#create workflow
prevwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(prevrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
prevtree_resamp <- prevwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
prevtreeplot <- prevtree_resamp %>% autoplot()
prevtreeplot

#show the best model
prevtree_resamp %>% 
  show_best()

#select best model
wdprevbest_tree <- prevtree_resamp %>% select_best("rmse")

#create final workflow
wdprevfinal_wf <- prevwflow %>% finalize_workflow(wdprevbest_tree)

#fit the final workflow to the training data
wdprevfinal_fit <- wdprevfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdprevfitted <- augment(wdprevfinal_fit, wdtrain_data) %>% 
  select(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB prevalence (cases per 100 000 population)`, .pred)
wdprevfitted
#fit final workflow to test data
wdprevfittest <- wdprevfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdprevfittest %>% collect_metrics()

#check important variables of model
wdprevfittest %>% extract_fit_parsnip() %>% vip()

## ---- hbprevalencemodel -----
#recipe predicting TB prevalence
prevrecipe <- recipe(`TB prevalence (cases per 100 000 population)` ~.,
                     data = hbtrain_data)

#create workflow
prevwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(prevrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
prevtree_resamp <- prevwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
prevtreeplot <- prevtree_resamp %>% autoplot()
prevtreeplot

#show the best model
prevtree_resamp %>% 
  show_best()

#select best model
hbprevbest_tree <- prevtree_resamp %>% select_best("rmse")

#create final workflow
hbprevfinal_wf <- prevwflow %>% finalize_workflow(hbprevbest_tree)

#fit the final workflow to the training data
hbprevfinal_fit <- hbprevfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbprevfitted <- augment(hbprevfinal_fit, hbtrain_data) %>% 
  select(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB prevalence (cases per 100 000 population)`, .pred)
hbprevfitted
#fit final workflow to test data
hbprevfittest <- hbprevfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbprevfittest %>% collect_metrics()

#check important variables of model
hbprevfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbprevfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdbcgmodel --------
#recipe predicting TB bcg coverage
bcgrecipe <- recipe(`BCG immunization coverage among one-year-olds (%)` ~.,
                     data = wdtrain_data)

#create workflow
bcgwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(bcgrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
bcgtree_resamp <- bcgwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
bcgtreeplot <- bcgtree_resamp %>% autoplot()
bcgtreeplot

#show the best model
bcgtree_resamp %>% 
  show_best()

#select best model
wdbcgbest_tree <- bcgtree_resamp %>% select_best("rmse")

#create final workflow
wdbcgfinal_wf <- bcgwflow %>% finalize_workflow(wdbcgbest_tree)

#fit the final workflow to the training data
wdbcgfinal_fit <- wdbcgfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdbcgfitted <- augment(wdbcgfinal_fit, wdtrain_data) %>% 
  select(`BCG immunization coverage among one-year-olds (%)`, .pred) %>% 
  rmse(truth = `BCG immunization coverage among one-year-olds (%)`, .pred)
wdbcgfitted
#fit final workflow to test data
wdbcgfittest <- wdbcgfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdbcgfittest %>% collect_metrics()

#check important variables of model
wdbcgfittest %>% extract_fit_parsnip() %>% vip()

## ---- hbbcgmodel ---------
#recipe predicting BCG coverage
bcgrecipe <- recipe(`BCG immunization coverage among one-year-olds (%)` ~.,
                     data = hbtrain_data)

#create workflow
bcgwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(bcgrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
bcgtree_resamp <- bcgwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
bcgtreeplot <- bcgtree_resamp %>% autoplot()
bcgtreeplot

#show the best model
bcgtree_resamp %>% 
  show_best()

#select best model
hbbcgbest_tree <- bcgtree_resamp %>% select_best("rmse")

#create final workflow
hbbcgfinal_wf <- bcgwflow %>% finalize_workflow(hbbcgbest_tree)

#fit the final workflow to the training data
hbbcgfinal_fit <- hbbcgfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbbcgfitted <- augment(hbbcgfinal_fit, hbtrain_data) %>% 
  select(`BCG immunization coverage among one-year-olds (%)`, .pred) %>% 
  rmse(truth = `BCG immunization coverage among one-year-olds (%)`, .pred)
hbbcgfitted
#fit final workflow to test data
hbbcgfittest <- hbbcgfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbbcgfittest %>% collect_metrics()

#check important variables of model
hbbcgfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbbcgfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdcatacostmodel --------
#recipe predicting catastrophic cost
catarecipe <- recipe(`Families affected by TB facing catastrophic costs due to TB (%)` ~.,
                     data = wdtrain_data)

#create workflow
catawflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(catarecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
catatree_resamp <- catawflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
catatreeplot <- catatree_resamp %>% autoplot()
catatreeplot

#show the best model
catatree_resamp %>% 
  show_best()

#select best model
wdcatabest_tree <- catatree_resamp %>% select_best("rmse")

#create final workflow
wdcatafinal_wf <- catawflow %>% finalize_workflow(wdcatabest_tree)

#fit the final workflow to the training data
wdcatafinal_fit <- wdcatafinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdcatafitted <- augment(wdcatafinal_fit, wdtrain_data) %>% 
  select(`Families affected by TB facing catastrophic costs due to TB (%)`, .pred) %>% 
  rmse(truth = `Families affected by TB facing catastrophic costs due to TB (%)`, .pred)
wdcatafitted
#fit final workflow to test data
wdcatafittest <- wdcatafinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdcatafittest %>% collect_metrics()

#check important variables of model
wdcatafittest %>% extract_fit_parsnip() %>% vip()

## ---- hbcatacostmodel ---------
#recipe predicting catastrophic cost
catarecipe <- recipe(`Families affected by TB facing catastrophic costs due to TB (%)` ~.,
                     data = hbtrain_data)

#create workflow
catawflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(catarecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
catatree_resamp <- catawflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
catatreeplot <- catatree_resamp %>% autoplot()
catatreeplot

#show the best model
catatree_resamp %>% 
  show_best()

#select best model
hbcatabest_tree <- catatree_resamp %>% select_best("rmse")

#create final workflow
hbcatafinal_wf <- catawflow %>% finalize_workflow(hbcatabest_tree)

#fit the final workflow to the training data
hbcatafinal_fit <- hbcatafinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbcatafitted <- augment(hbcatafinal_fit, hbtrain_data) %>% 
  select(`Families affected by TB facing catastrophic costs due to TB (%)`, .pred) %>% 
  rmse(truth = `Families affected by TB facing catastrophic costs due to TB (%)`, .pred)
hbcatafitted
#fit final workflow to test data
hbcatafittest <- hbcatafinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbcatafittest %>% collect_metrics()

#check important variables of model
hbcatafittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbcatafinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdcasedetectmodel --------
#recipe predicting TB case detection
caserecipe <- recipe(`Case detection rate (%)` ~.,
                     data = wdtrain_data)

#create workflow
casewflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(caserecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
casetree_resamp <- casewflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
casetreeplot <- casetree_resamp %>% autoplot()
casetreeplot

#show the best model
casetree_resamp %>% 
  show_best()

#select best model
wdcasebest_tree <- casetree_resamp %>% select_best("rmse")

#create final workflow
wdcasefinal_wf <- casewflow %>% finalize_workflow(wdcasebest_tree)

#fit the final workflow to the training data
wdcasefinal_fit <- wdcasefinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdcasefitted <- augment(wdcasefinal_fit, wdtrain_data) %>% 
  select(`Case detection rate (%)`, .pred) %>% 
  rmse(truth = `Case detection rate (%)`, .pred)
wdcasefitted
#fit final workflow to test data
wdcasefittest <- wdcasefinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdcasefittest %>% collect_metrics()

#check important variables of model
wdcasefittest %>% extract_fit_parsnip() %>% vip()

## ---- hbcasedetectmodel --------
#recipe predicting TB case detection
caserecipe <- recipe(`Case detection rate (%)` ~.,
                     data = hbtrain_data)

#create workflow
casewflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(caserecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
casetree_resamp <- casewflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
casetreeplot <- casetree_resamp %>% autoplot()
casetreeplot

#show the best model
casetree_resamp %>% 
  show_best()

#select best model
hbcasebest_tree <- casetree_resamp %>% select_best("rmse")

#create final workflow
hbcasefinal_wf <- casewflow %>% finalize_workflow(hbcasebest_tree)

#fit the final workflow to the training data
hbcasefinal_fit <- hbcasefinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbcasefitted <- augment(hbcasefinal_fit, hbtrain_data) %>% 
  select(`Case detection rate (%)`, .pred) %>% 
  rmse(truth = `Case detection rate (%)`, .pred)
hbcasefitted
#fit final workflow to test data
hbcasefittest <- hbcasefinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbcasefittest %>% collect_metrics()

#check important variables of model
hbcasefittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbcasefinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdptnmodel ---------
#recipe predicting TB prevalence to notification
ptnrecipe <- recipe(`Prevalence to notification ratio (years)` ~.,
                     data = wdtrain_data)

#create workflow
ptnwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(ptnrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
ptntree_resamp <- ptnwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
ptntreeplot <- ptntree_resamp %>% autoplot()
ptntreeplot

#show the best model
ptntree_resamp %>% 
  show_best()

#select best model
wdptnbest_tree <- ptntree_resamp %>% select_best("rmse")

#create final workflow
wdptnfinal_wf <- ptnwflow %>% finalize_workflow(wdptnbest_tree)

#fit the final workflow to the training data
wdptnfinal_fit <- wdptnfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdptnfitted <- augment(wdptnfinal_fit, wdtrain_data) %>% 
  select(`Prevalence to notification ratio (years)`, .pred) %>% 
  rmse(truth = `Prevalence to notification ratio (years)`, .pred)
wdptnfitted
#fit final workflow to test data
wdptnfittest <- wdptnfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdptnfittest %>% collect_metrics()

#check important variables of model
wdptnfittest %>% extract_fit_parsnip() %>% vip()

## ---- hbptnmodel --------
#recipe predicting TB prevalence to notifcation
ptnrecipe <- recipe(`Prevalence to notification ratio (years)` ~.,
                     data = hbtrain_data)

#create workflow
ptnwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(ptnrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
ptntree_resamp <- ptnwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
ptntreeplot <- ptntree_resamp %>% autoplot()
ptntreeplot

#show the best model
ptntree_resamp %>% 
  show_best()

#select best model
hbtnbest_tree <- ptntree_resamp %>% select_best("rmse")

#create final workflow
hbptnfinal_wf <- ptnwflow %>% finalize_workflow(hbtnbest_tree)

#fit the final workflow to the training data
hbptnfinal_fit <- hbptnfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbptnfitted <- augment(hbptnfinal_fit, hbtrain_data) %>% 
  select(`Prevalence to notification ratio (years)`, .pred) %>% 
  rmse(truth = `Prevalence to notification ratio (years)`, .pred)
hbptnfitted
#fit final workflow to test data
hbptnfittest <- hbptnfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbptnfittest %>% collect_metrics()

#check important variables of model
hbptnfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbptnfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdmaleattitudemodel --------
#recipe predicting male attitude
attrecipe <- recipe(`People who would want a family member's TB kept secret - Male (%)` ~.,
                     data = wdtrain_data)

#create workflow
attwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(attrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
atttree_resamp <- attwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
atttreeplot <- atttree_resamp %>% autoplot()
atttreeplot

#show the best model
atttree_resamp %>% 
  show_best()

#select best model
wdattbest_tree <- atttree_resamp %>% select_best("rmse")

#create final workflow
wdattfinal_wf <- attwflow %>% finalize_workflow(wdattbest_tree)

#fit the final workflow to the training data
wdattfinal_fit <- wdattfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdattfitted <- augment(wdattfinal_fit, wdtrain_data) %>% 
  select(`People who would want a family member's TB kept secret - Male (%)`, .pred) %>% 
  rmse(truth = `People who would want a family member's TB kept secret - Male (%)`, .pred)
wdattfitted
#fit final workflow to test data
wdattfittest <- wdattfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdattfittest %>% collect_metrics()

#check important variables of model
wdattfittest %>% extract_fit_parsnip() %>% vip()

## ---- hbmaleattitubemodel ---------
#recipe predicting male attitude
attrecipe <- recipe(`People who would want a family member's TB kept secret - Male (%)` ~.,
                     data = hbtrain_data)

#create workflow
attwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(attrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
atttree_resamp <- attwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
atttreeplot <- atttree_resamp %>% autoplot()
atttreeplot

#show the best model
atttree_resamp %>% 
  show_best()

#select best model
hbattbest_tree <- atttree_resamp %>% select_best("rmse")

#create final workflow
hbattfinal_wf <- attwflow %>% finalize_workflow(hbattbest_tree)

#fit the final workflow to the training data
hbattfinal_fit <- hbattfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbattfitted <- augment(hbattfinal_fit, hbtrain_data) %>% 
  select(`People who would want a family member's TB kept secret - Male (%)`, .pred) %>% 
  rmse(truth = `People who would want a family member's TB kept secret - Male (%)`, .pred)
hbattfitted
#fit final workflow to test data
hbattfittest <- hbattfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbattfittest %>% collect_metrics()

#check important variables of model
hbattfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbattfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- wdmaleknowledgemodel ---------
#recipe predicting male knowledge
knowrecipe <- recipe(`People who report TB is spread through coughing - Male (%)`
 ~.,
                     data = wdtrain_data)

#create workflow
knowwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(knowrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
knowtree_resamp <- knowwflow %>% 
  tune_grid(
    resamples = wdfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
knowtreeplot <- knowtree_resamp %>% autoplot()
knowtreeplot

#show the best model
knowtree_resamp %>% 
  show_best()

#select best model
wdknowbest_tree <- knowtree_resamp %>% select_best("rmse")

#create final workflow
wdknowfinal_wf <- knowwflow %>% finalize_workflow(wdknowbest_tree)

#fit the final workflow to the training data
wdknowfinal_fit <- wdknowfinal_wf %>% fit(wdtrain_data)

#find RMSE for model with training data
wdknowfitted <- augment(wdknowfinal_fit, wdtrain_data) %>% 
  select(`People who report TB is spread through coughing - Male (%)`
, .pred) %>% 
  rmse(truth = `People who report TB is spread through coughing - Male (%)`
, .pred)
wdknowfitted
#fit final workflow to test data
wdknowfittest <- wdknowfinal_fit %>% last_fit(wddatasplit)

#find RMSE for model with test data
wdknowfittest %>% collect_metrics()

#check important variables of model
wdknowfittest %>% extract_fit_parsnip() %>% vip()

## ---- hbmaleknowledgemodel ---------
#recipe predicting male knowledge
knowrecipe <- recipe(`People who report TB is spread through coughing - Male (%)`
 ~.,
                     data = hbtrain_data)

#create workflow
knowwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(knowrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
knowtree_resamp <- knowwflow %>% 
  tune_grid(
    resamples = hbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
knowtreeplot <- knowtree_resamp %>% autoplot()
knowtreeplot

#show the best model
knowtree_resamp %>% 
  show_best()

#select best model
hbknowbest_tree <- knowtree_resamp %>% select_best("rmse")

#create final workflow
hbknowfinal_wf <- knowwflow %>% finalize_workflow(hbknowbest_tree)

#fit the final workflow to the training data
hbknowfinal_fit <- hbknowfinal_wf %>% fit(hbtrain_data)

#find RMSE for model with training data
hbknowfitted <- augment(hbknowfinal_fit, hbtrain_data) %>% 
  select(`People who report TB is spread through coughing - Male (%)`
, .pred) %>% 
  rmse(truth = `People who report TB is spread through coughing - Male (%)`
, .pred)
hbknowfitted
#fit final workflow to test data
hbknowfittest <- hbknowfinal_fit %>% last_fit(hbdatasplit)

#find RMSE for model with test data
hbknowfittest %>% collect_metrics()

#check important variables of model
hbknowfittest %>% extract_fit_parsnip() %>% vip()

#plot decision tree
hbknowfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)
