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

## ---- basicmodels ---------
# fit linear model with TB incidence as outcome, subgroup as predictor
lm_mod <- linear_reg()

#fit model with high burden countries
inclm_fit <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup,
      data = hbmodel) #fit linear model
incfittable <- tidy(inclm_fit) 
print(incfittable) #produce tidy table of fitted model

table_file1 = here("results", "statistical_analysis", "model fit tables", "incfittable.rds")
saveRDS(incfittable, file = table_file1) #save summary table

#fit model with full data
inclm_fitFD <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup,
      data = obmodel) #fit linear model
incfittable2 <- tidy(inclm_fitFD) 
print(incfittable2) #produce tidy table of fitted model

table_file2 = here("results", "statistical_analysis", "model fit tables","incfittableFD.rds")
saveRDS(incfittable2, file = table_file2) #save summary table

#prediction model using subgroup and setting
predinclm_fit <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup + setting,
      data = hbmodel) #fit linear model
predincfittable <- tidy(predinclm_fit) 
print(predincfittable)

new_points <- expand.grid(subgroup = c("Female", "Male"),
                          setting = "Central African Republic") #predicting CAR incidence based on sex
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
  labs(y = "TB Incidence (per 100 000 population)", 
       title = "Predicted TB incidence for Central African Republic")
predplot

saveRDS(predplot, file = here("results", "statistical_analysis", "model fit tables", "prediction1.rds"))

# fit linear model using TB mortality as outcome, subgroup and setting as predictor

#fit model with high burden countries
mortlm_fit <- lm_mod %>% 
  fit(`TB mortality (deaths per 100 000 population)` ~ subgroup,
      data = hbmodel) #fit linear model
mortfittable <- tidy(mortlm_fit) 
print(mortfittable) #produce tidy table of fitted model

table_file3 = here("results", "statistical_analysis", "model fit tables", "mortfittable.rds")
saveRDS(mortfittable, file = table_file3) #save summary table

#fit model with full data
mortlm_fitFD <- lm_mod %>% 
  fit(`TB mortality (deaths per 100 000 population)` ~ subgroup,
      data = otherburden) #fit linear model
mortfittableFD <- tidy(mortlm_fitFD) 
print(mortfittableFD) #produce tidy table of fitted model

table_file4 = here("results", "statistical_analysis", "model fit tables", "mortfittableFD.rds")
saveRDS(mortfittableFD, file = table_file4)

# fit linear model TB prevalence as outcome, subgroups as predictors

#fit model with high burden countries
prevlm_fit <- lm_mod %>% 
  fit(`TB prevalence (cases per 100 000 population)` ~ subgroup,
      data = hbmodelprev) #fit linear model
prevfittable <- tidy(prevlm_fit) 
print(prevfittable) #produce tidy table of fitted model

table_file5 = here("results", "statistical_analysis", "model fit tables", "prevfittable.rds")
saveRDS(prevfittable, file = table_file5) #save summary table

#fit model with full data
prevlm_fitFD <- lm_mod %>% 
  fit(`TB prevalence (cases per 100 000 population)` ~ subgroup,
      data = obmodelprev) #fit linear model
prevfittableFD <- tidy(prevlm_fitFD) 
print(prevfittableFD) #produce tidy table of fitted model

table_file6 = here("results", "statistical_analysis", "model fit tables", "prevfittableFD.rds")
saveRDS(prevfittableFD, file = table_file6)


## ---- splitdata --------
#split data into training and test sets for wide_data
obdtdatasplit <- initial_split(obmodel, prop = 3/4)
obdttest_data <- testing(obdtdatasplit)
obdttrain_data <- training(obdtdatasplit)

#set seed for reproducibility
set.seed(626)

#create cross-validation folds
obdtfolds <- vfold_cv(obdttrain_data, v = 5)

#split data into training and test sets for highburden
hbdtdatasplit <- initial_split(hbmodel, prop = 3/4)
hbdttest_data <- testing(hbdtdatasplit)
hbdttrain_data <- training(hbdtdatasplit)

#set seed for reproducibility
set.seed(626)

#create cross-validation folds
hbdtfolds <- vfold_cv(hbdttrain_data, v = 5)

#split data into training and test sets for other burden prevalence
obdtprevsplit <- initial_split(obmodelprev, prop = 3/4)
obdtprevtest_data <- testing(obdtprevsplit)
obdtprevtrain_data <- training(obdtprevsplit)

#set seed for reproducibility 
set.seed(626)

#create cross-validation folds
obdtprevfolds <- vfold_cv(obdtprevtrain_data, v = 5)

#split data into training and test sets for high burden prevalence
hbdtprevsplit <- initial_split(hbmodelprev, prop = 3/4)
hbdtprevtest_data <- testing(hbdtprevsplit)
hbdtprevtrain_data <- training(hbdtprevsplit)

#set seed for reproducibility 
set.seed(626)

#create cross-validation folds
hbdtprevfolds <- vfold_cv(hbdtprevtrain_data, v = 5)

## ---- obdtincidencemodel ---------
#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                 data = obdttrain_data)
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
    resamples = obdtfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
inctreeplot <- inctree_resamp %>% autoplot()
inctreeplot
treeplot1 = here("results", "statistical_analysis", "tune plots", "obdtinctreeplot.rds")
saveRDS(inctreeplot, file = treeplot1)

#show the best model
inctree_resamp %>% 
  show_best()

#select best model
obdtincbest_tree <- inctree_resamp %>% select_best("rmse")

#create final workflow
obdtincfinal_wf <- incwflow %>% finalize_workflow(obdtincbest_tree)

#fit the final workflow to the training data
obdtincfinal_fit <- obdtincfinal_wf %>% fit(obdttrain_data)

#find RMSE for model with training data
obdtincfitted <- augment(obdtincfinal_fit, obdttrain_data) %>% 
  select(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB incidence (new infections per 100 000 population)`, .pred)
obdtincfitted


#save for later comparison: RMSE based on predictions from best model
obdtinc_rmse <- inctree_resamp %>% 
  collect_predictions(parameters = obdtincbest_tree) %>% 
  rmse(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  mutate(model = "obdtinc")

## ---- hbdtincidencemodel ---------
#recipe predicting TB incidence
increcipe <- recipe(`TB incidence (new infections per 100 000 population)` ~.,
                    data = hbdttrain_data)
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
    resamples = hbdtfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
inctreeplot <- inctree_resamp %>% autoplot()
inctreeplot
treeplot2 = here("results", "statistical_analysis", "tune plots", "hbdtinctreeplot.rds")
saveRDS(inctreeplot, file = treeplot2)

#show the best model
inctree_resamp %>% 
  show_best()

#select best model
hbdtincbest_tree <- inctree_resamp %>% select_best("rmse")

#create final workflow
hbdtincfinal_wf <- incwflow %>% finalize_workflow(hbdtincbest_tree)

#fit the final workflow to the training data
hbdtincfinal_fit <- hbdtincfinal_wf %>% fit(hbdttrain_data)

#find RMSE for model with training data
hbdtincfitted <- augment(hbdtincfinal_fit, hbdttrain_data) %>% 
  select(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB incidence (new infections per 100 000 population)`, .pred)
hbdtincfitted

#plot decision tree
hbdtincfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

#save for later comparison
hbdtinc_rmse <- inctree_resamp %>% 
  collect_predictions(parameters = hbdtincbest_tree) %>% 
  rmse(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  mutate(model = "hbdtinc")

## ---- obdtmortalitymodel ---------
#recipe predicting TB mortality
mortrecipe <- recipe(`TB mortality (deaths per 100 000 population)` ~.,
                 data = obdttrain_data)
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
    resamples = obdtfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
morttreeplot <- morttree_resamp %>% autoplot()
morttreeplot
treeplot3 = here("results", "statistical_analysis", "tune plots", "obdtmorttreeplot.rds")
saveRDS(morttreeplot, file = treeplot3)

#show the best model
morttree_resamp %>% 
  show_best()

#select best model
obdtmortbest_tree <- morttree_resamp %>% select_best("rmse")

#create final workflow
obdtmortfinal_wf <- mortwflow %>% finalize_workflow(obdtmortbest_tree)

#fit the final workflow to the training data
obdtmortfinal_fit <- obdtmortfinal_wf %>% fit(obdttrain_data)

#find RMSE for model with training data
obdtmortfitted <- augment(obdtmortfinal_fit, obdttrain_data) %>% 
  select(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB mortality (deaths per 100 000 population)`, .pred)
obdtmortfitted

#save for later comparison: RMSE based on predictions from best model
obdtmort_rmse <- morttree_resamp %>% 
  collect_predictions(parameters = obdtmortbest_tree) %>% 
  rmse(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  mutate(model = "obdtmort")

## ---- hbdtmortalitymodel ---------
#recipe predicting TB mortality
mortrecipe <- recipe(`TB mortality (deaths per 100 000 population)` ~.,
                     data = hbdttrain_data)
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
    resamples = hbdtfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
morttreeplot <- morttree_resamp %>% autoplot()
morttreeplot
treeplot4 = here("results", "statistical_analysis", "tune plots", "hbdtmorttreeplot.rds")
saveRDS(morttreeplot, file = treeplot4)

#show the best model
morttree_resamp %>% 
  show_best()

#select best model
hbdtmortbest_tree <- morttree_resamp %>% select_best("rmse")

#create final workflow
hbdtmortfinal_wf <- mortwflow %>% finalize_workflow(hbdtmortbest_tree)

#fit the final workflow to the training data
hbdtmortfinal_fit <- hbdtmortfinal_wf %>% fit(hbdttrain_data)

#find RMSE for model with training data
hbdtmortfitted <- augment(hbdtmortfinal_fit, hbdttrain_data) %>% 
  select(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB mortality (deaths per 100 000 population)`, .pred)
hbdtmortfitted

#save for later comparison: RMSE based on predictions from best model
hbdtmort_rmse <- morttree_resamp %>% 
  collect_predictions(parameters = hbdtmortbest_tree) %>% 
  rmse(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  mutate(model = "hbdtmort")

#plot decision tree
hbdtmortfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- obdtprevalencemodel -----
#recipe predicting TB prevalence
prevrecipe <- recipe(`TB prevalence (cases per 100 000 population)` ~.,
                 data = obdtprevtrain_data)

#create workflow
prevwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(prevrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
prevtree_resamp <- prevwflow %>% 
  tune_grid(
    resamples = obdtprevfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
prevtreeplot <- prevtree_resamp %>% autoplot()
prevtreeplot

treeplot5 = here("results", "statistical_analysis", "tune plots", "obdtprevtreeplot.rds")
saveRDS(prevtreeplot, file = treeplot5)

#show the best model
prevtree_resamp %>% 
  show_best()

#select best model
obdtprevbest_tree <- prevtree_resamp %>% select_best("rmse")

#create final workflow
obdtprevfinal_wf <- prevwflow %>% finalize_workflow(obdtprevbest_tree)

#fit the final workflow to the training data
obdtprevfinal_fit <- obdtprevfinal_wf %>% fit(obdtprevtrain_data)

#find RMSE for model with training data
obdtprevfitted <- augment(obdtprevfinal_fit, obdtprevtrain_data) %>% 
  select(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB prevalence (cases per 100 000 population)`, .pred)
obdtprevfitted

#save for later comparison: RMSE based on predictions from best model
obdtprev_rmse <- prevtree_resamp %>% 
  collect_predictions(parameters = obdtprevbest_tree) %>% 
  rmse(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  mutate(model = "obdtprev")

## ---- hbdtprevalencemodel -----
#recipe predicting TB prevalence
prevrecipe <- recipe(`TB prevalence (cases per 100 000 population)` ~.,
                     data = hbdtprevtrain_data)

#create workflow
prevwflow <- workflow() %>% 
  add_model(treemodel) %>% 
  add_recipe(prevrecipe)

#set seed for reproducibility
set.seed(626)
#resample using cross-validation folds to tune parameters
prevtree_resamp <- prevwflow %>% 
  tune_grid(
    resamples = hbdtprevfolds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

#plot resampling by parameters
prevtreeplot <- prevtree_resamp %>% autoplot()
prevtreeplot

treeplot6 = here("results", "statistical_analysis", "tune plots", "hbdtprevtreeplot.rds")
saveRDS(prevtreeplot, file = treeplot6)

#show the best model
prevtree_resamp %>% 
  show_best()

#select best model
hbdtprevbest_tree <- prevtree_resamp %>% select_best("rmse")

#create final workflow
hbdtprevfinal_wf <- prevwflow %>% finalize_workflow(hbdtprevbest_tree)

#fit the final workflow to the training data
hbdtprevfinal_fit <- hbdtprevfinal_wf %>% fit(hbdtprevtrain_data)

#find RMSE for model with training data
hbdtprevfitted <- augment(hbdtprevfinal_fit, hbdtprevtrain_data) %>% 
  select(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  rmse(truth = `TB prevalence (cases per 100 000 population)`, .pred)
hbdtprevfitted

#save for later comparison: RMSE based on predictions from best model
hbdtprev_rmse <- prevtree_resamp %>% 
  collect_predictions(parameters = hbdtprevbest_tree) %>% 
  rmse(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  mutate(model = "hbdtprev")

## ---- summary ---------
#create summary table of RMSE of best models and save
comparedt <- bind_rows(obdtinc_rmse, hbdtinc_rmse,
                     obdtmort_rmse, hbdtmort_rmse,
                     obdtprev_rmse, hbdtprev_rmse,)
comparedt
table_file1 = here("results", "statistical_analysis", "dtsummaryrmse.rds")
saveRDS(comparedt, file = table_file1)