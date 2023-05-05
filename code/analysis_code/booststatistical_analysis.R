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
library(bonsai)

## ---- loaddata --------
#path to data
data_location <- here::here("data","processed_data","processeddata.rda")

#load data. 
load(data_location)

## ---- splitdata --------
#split data into training and test sets for wide_data
obbdatasplit <- initial_split(obmodel, prop = 3/4)
obbtest_data <- testing(obbdatasplit)
obbtrain_data <- training(obbdatasplit)

#set seed for reproducibility
set.seed(626)

#create cross-validation folds
obbfolds <- vfold_cv(obbtrain_data, v = 5)

#split data into training and test sets for highburden
hbbdatasplit <- initial_split(hbmodel, prop = 3/4)
hbbtest_data <- testing(hbbdatasplit)
hbbtrain_data <- training(hbbdatasplit)

#set seed for reproducibility
set.seed(626)

#create cross-validation folds
hbbfolds <- vfold_cv(hbbtrain_data, v = 5)

#split data into training and test sets for other burden prevalence
obbprevsplit <- initial_split(obmodelprev, prop = 3/4)
obbprevtest_data <- testing(obbprevsplit)
obbprevtrain_data <- training(obbprevsplit)

#set seed for reproducibility 
set.seed(626)

#create cross-validation folds
obbprevfolds <- vfold_cv(obbprevtrain_data, v = 5)

#split data into training and test sets for high burden prevalence
hbbprevsplit <- initial_split(hbmodelprev, prop = 3/4)
hbbprevtest_data <- testing(hbbprevsplit)
hbbprevtrain_data <- training(hbbprevsplit)

#set seed for reproducibility 
set.seed(626)

#create cross-validation folds
hbbprevfolds <- vfold_cv(hbbprevtrain_data, v = 5)

## ---- obbincidencemodel ---------
#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                    data = obbtrain_data)
#create tree model
treemodel <- boost_tree(trees = tune(),
                        tree_depth = tune(),
                        min_n = tune()) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

#create work flow
incwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(increcipe)

#create grid of tuning parameters
tree_grid <- grid_regular(tree_depth(),
                          trees(),
                          min_n())
#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
inctree_resamp <- incwflow %>% 
  tune_grid(
    resamples = obbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
inctreeplot <- inctree_resamp %>% autoplot()
inctreeplot
treeplot1 = here("results", "statistical_analysis", "tune plots", "obbinctreeplot.rds")
saveRDS(inctreeplot, file = treeplot1)

#show the best model
inctree_resamp %>% 
  show_best()

#select best model
obbincbest_tree <- inctree_resamp %>% select_best("rmse")

#create final workflow
obbincfinal_wf <- incwflow %>% finalize_workflow(obbincbest_tree)

#fit the final workflow to the training data
obbincfinal_fit <- obbincfinal_wf %>% fit(obbtrain_data)

#find RMSE for model with training data
obbincfitted <- augment(obbincfinal_fit, obbtrain_data) 

#save for later comparison: RMSE based on predictions from best model
obbinc_rmse <- inctree_resamp %>% 
  collect_predictions(parameters = obbincbest_tree) %>% 
  rmse(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  mutate(model = "obbinc")
obbinc_rmse

#fit final workflow to test data
obbincfittest <- obbincfinal_fit %>% last_fit(obbdatasplit)

#find RMSE for model with test data
obbincfittest %>% collect_metrics()

#create residuals from predictions and outcome values
obbincfitted <- obbincfitted %>% 
  mutate(.resid = `TB incidence (new infections per 100 000 population)` - .pred)

#plot residuals and predictions
obbincresidplot <- obbincfitted %>% ggplot(aes(.pred, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title = "Other Burden Incidence Residuals")
obbincresidplot



## ---- hbbincidencemodel ---------
#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                    data = hbbtrain_data)

#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                    data = hbbtrain_data)

#create tree model
treemodel <- boost_tree(trees = tune(),
                        tree_depth = tune(),
                        min_n = tune()) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

#create work flow
incwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(increcipe)

#create grid of tuning parameters
tree_grid <- grid_regular(tree_depth(),
                          trees(),
                          min_n())

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
inctree_resamp <- incwflow %>% 
  tune_grid(
    resamples = hbbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
inctreeplot <- inctree_resamp %>% autoplot()
inctreeplot
treeplot2 = here("results", "statistical_analysis", "tune plots", "hbbinctreeplot.rds")
saveRDS(inctreeplot, file = treeplot2)

#show the best model
inctree_resamp %>% 
  show_best()

#select best model
hbbincbest_tree <- inctree_resamp %>% select_best("rmse")

#create final workflow
hbbincfinal_wf <- incwflow %>% finalize_workflow(hbbincbest_tree)

#fit the final workflow to the training data
hbbincfinal_fit <- hbbincfinal_wf %>% fit(hbbtrain_data)

#find RMSE for model with training data
hbbincfitted <- augment(hbbincfinal_fit, hbbtrain_data) 

#fit final workflow to test data
hbbincfittest <- hbbincfinal_fit %>% last_fit(hbbdatasplit)

#find RMSE for model with test data
hbbincfittest %>% collect_metrics()

#save for later comparison
hbbinc_rmse <- inctree_resamp %>% 
  collect_predictions(parameters = hbbincbest_tree) %>% 
  rmse(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  mutate(model = "hbbinc")
hbbinc_rmse

#create residuals from predictions and outcome values
hbbincfitted <- hbbincfitted %>% 
  mutate(.resid = `TB incidence (new infections per 100 000 population)` - .pred)

#plot residuals and predictions
hbbincresidplot <- hbbincfitted %>% ggplot(aes(.pred, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title = "High Burden Incidence Residuals")
hbbincresidplot

## ---- obbmortalitymodel ---------
#recipe predicting TB mortality
mortrecipe <- recipe(`TB mortality (deaths per 100 000 population)` ~.,
                     data = obbtrain_data)
#create workflow
mortwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(mortrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
morttree_resamp <- mortwflow %>% 
  tune_grid(
    resamples = obbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
morttreeplot <- morttree_resamp %>% autoplot()
morttreeplot
treeplot3 = here("results", "statistical_analysis", "tune plots", "obbmorttreeplot.rds")
saveRDS(morttreeplot, file = treeplot3)

#show the best model
morttree_resamp %>% 
  show_best()

#select best model
obbmortbest_tree <- morttree_resamp %>% select_best("rmse")

#create final workflow
obbmortfinal_wf <- mortwflow %>% finalize_workflow(obbmortbest_tree)

#fit the final workflow to the training data
obbmortfinal_fit <- obbmortfinal_wf %>% fit(obbtrain_data)

#find RMSE for model with training data
obbmortfitted <- augment(obbmortfinal_fit, obbtrain_data)

#save for later comparison: RMSE based on predictions from best model
obbmort_rmse <- morttree_resamp %>% 
  collect_predictions(parameters = obbmortbest_tree) %>% 
  rmse(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  mutate(model = "obbmort")
obbmort_rmse

#fit final workflow to test data
obbmortfittest <- obbmortfinal_fit %>% last_fit(obbdatasplit)

#find RMSE for model with test data
obbmortfittest %>% collect_metrics()

#create residuals from predictions and outcome values
obbmortfitted <- obbmortfitted %>% 
  mutate(.resid = `TB mortality (deaths per 100 000 population)` - .pred)

#plot residuals and predictions
obbmortresidplot <- obbmortfitted %>% ggplot(aes(.pred, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title = "Other Burden Mortality Residuals")
obbmortresidplot

## ---- hbbmortalitymodel ---------
#recipe predicting TB mortality
mortrecipe <- recipe(`TB mortality (deaths per 100 000 population)` ~.,
                     data = hbbtrain_data)
#create workflow
mortwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(mortrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
morttree_resamp <- mortwflow %>% 
  tune_grid(
    resamples = hbbfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
morttreeplot <- morttree_resamp %>% autoplot()
morttreeplot
treeplot3 = here("results", "statistical_analysis", "tune plots", "hbbmorttreeplot.rds")
saveRDS(morttreeplot, file = treeplot3)

#show the best model
morttree_resamp %>% 
  show_best()

#select best model
hbbmortbest_tree <- morttree_resamp %>% select_best("rmse")

#create final workflow
hbbmortfinal_wf <- mortwflow %>% finalize_workflow(hbbmortbest_tree)

#fit the final workflow to the training data
hbbmortfinal_fit <- hbbmortfinal_wf %>% fit(hbbtrain_data)

#find RMSE for model with training data
hbbmortfitted <- augment(hbbmortfinal_fit, hbbtrain_data)

#save for later comparison: RMSE based on predictions from best model
hbbmort_rmse <- morttree_resamp %>% 
  collect_predictions(parameters = hbbmortbest_tree) %>% 
  rmse(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  mutate(model = "hbbmort")
hbbmort_rmse

#fit final workflow to test data
hbbmortfittest <- hbbmortfinal_fit %>% last_fit(hbbdatasplit)

#find RMSE for model with test data
hbbmortfittest %>% collect_metrics()

#create residuals from predictions and outcome values
hbbmortfitted <- hbbmortfitted %>% 
  mutate(.resid = `TB mortality (deaths per 100 000 population)` - .pred)

#plot residuals and predictions
hbbmortresidplot <- hbbmortfitted %>% ggplot(aes(.pred, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title = "High Burden Mortality Residuals")
hbbmortresidplot

## ---- obbprevalencemodel -----
#recipe predicting TB prevalence
prevrecipe <- recipe(`TB prevalence (cases per 100 000 population)` ~.,
                     data = obbprevtrain_data)

#create workflow
prevwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(prevrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
prevtree_resamp <- prevwflow %>% 
  tune_grid(
    resamples = obbprevfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
prevtreeplot <- prevtree_resamp %>% autoplot()
prevtreeplot

treeplot5 = here("results", "statistical_analysis", "tune plots", "obbprevtreeplot.rds")
saveRDS(prevtreeplot, file = treeplot5)

#show the best model
prevtree_resamp %>% 
  show_best()

#select best model
obbprevbest_tree <- prevtree_resamp %>% select_best("rmse")

#create final workflow
obbprevfinal_wf <- prevwflow %>% finalize_workflow(obbprevbest_tree)

#fit the final workflow to the training data
obbprevfinal_fit <- obbprevfinal_wf %>% fit(obbprevtrain_data)

#find RMSE for model with training data
obbprevfitted <- augment(obbprevfinal_fit, obbprevtrain_data) 

#save for later comparison: RMSE based on predictions from best model
obbprev_rmse <- prevtree_resamp %>% 
  collect_predictions(parameters = obbprevbest_tree) %>% 
  rmse(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  mutate(model = "obbprev")
obbprev_rmse

#fit final workflow to test data
obbprevfittest <- obbprevfinal_fit %>% last_fit(obbprevsplit)

#find RMSE for model with test data
obbprevfittest %>% collect_metrics()

#create residuals from predictions and outcome values
obbprevfitted <- obbprevfitted %>% 
  mutate(.resid = `TB prevalence (cases per 100 000 population)` - .pred)

#plot residuals and predictions
obbprevresidplot <- obbprevfitted %>% ggplot(aes(.pred, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title = "Other Burden Prevalence Residuals")
obbprevresidplot

## ---- hbbprevalencemodel -----
#recipe predicting TB prevalence
prevrecipe <- recipe(`TB prevalence (cases per 100 000 population)` ~.,
                     data = hbbprevtrain_data)

#create workflow
prevwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(prevrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
prevtree_resamp <- prevwflow %>% 
  tune_grid(
    resamples = hbbprevfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
prevtreeplot <- prevtree_resamp %>% autoplot()
prevtreeplot

treeplot6 = here("results", "statistical_analysis", "tune plots", "hbbprevtreeplot.rds")
saveRDS(prevtreeplot, file = treeplot6)

#show the best model
prevtree_resamp %>% 
  show_best()

#select best model
hbbprevbest_tree <- prevtree_resamp %>% select_best("rmse")

#create final workflow
hbbprevfinal_wf <- prevwflow %>% finalize_workflow(hbbprevbest_tree)

#fit the final workflow to the training data
hbbprevfinal_fit <- hbbprevfinal_wf %>% fit(hbbprevtrain_data)

#find RMSE for model with training data
hbbprevfitted <- augment(hbbprevfinal_fit, hbbprevtrain_data) 

#save for later comparison: RMSE based on predictions from best model
hbbprev_rmse <- prevtree_resamp %>% 
  collect_predictions(parameters = hbbprevbest_tree) %>% 
  rmse(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  mutate(model = "hbbprev")
hbbprev_rmse

#fit final workflow to test data
hbbprevfittest <- hbbprevfinal_fit %>% last_fit(hbbprevsplit)

#find RMSE for model with test data
hbbprevfittest %>% collect_metrics()

#create residuals from predictions and outcome values
hbbprevfitted <- hbbprevfitted %>% 
  mutate(.resid = `TB prevalence (cases per 100 000 population)` - .pred)

#plot residuals and predictions
hbbprevresidplot <- hbbprevfitted %>% ggplot(aes(.pred, .resid))+
  geom_point()+
  geom_hline(yintercept = 0)+
  labs(title = "High Burden Prevalence Residuals")
hbbprevresidplot

## ---- compare -----
#create summary table of RMSE for the models
comparebt <- bind_rows(obbinc_rmse, hbbinc_rmse, obbmort_rmse, hbbmort_rmse, obbprev_rmse, hbbprev_rmse)
comparebt
table_file1 = here("results", "statistical_analysis", "btsummaryrmse.rds")
saveRDS(comparebt, file = table_file1)