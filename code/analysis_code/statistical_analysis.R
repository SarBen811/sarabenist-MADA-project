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
      data = highburden) #fit linear model
incfittable <- tidy(inclm_fit) 
print(incfittable) #produce tidy table of fitted model

table_file1 = here("results", "statistical_analysis", "model fit tables", "incfittable.rds")
saveRDS(incfittable, file = table_file1) #save summary table

#fit model with full data
inclm_fitFD <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup,
      data = wide_data) #fit linear model
incfittable2 <- tidy(inclm_fitFD) 
print(incfittable2) #produce tidy table of fitted model

table_file2 = here("results", "statistical_analysis", "model fit tables","incfittableFD.rds")
saveRDS(incfittable2, file = table_file2) #save summary table

#prediction model using subgroup and setting
predinclm_fit <- lm_mod %>% 
  fit(`TB incidence (new infections per 100 000 population)` ~ subgroup + setting,
      data = highburden) #fit linear model
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
      data = highburden) #fit linear model
mortfittable <- tidy(mortlm_fit) 
print(mortfittable) #produce tidy table of fitted model

table_file3 = here("results", "statistical_analysis", "model fit tables", "mortfittable.rds")
saveRDS(mortfittable, file = table_file3) #save summary table

#fit model with full data
mortlm_fitFD <- lm_mod %>% 
  fit(`TB mortality (deaths per 100 000 population)` ~ subgroup,
      data = wide_data) #fit linear model
mortfittableFD <- tidy(mortlm_fitFD) 
print(mortfittableFD) #produce tidy table of fitted model

table_file4 = here("results", "statistical_analysis", "model fit tables", "mortfittableFD.rds")
saveRDS(mortfittableFD, file = table_file4)

# fit linear model TB prevalence as outcome, subgroups as predictors

#fit model with high burden countries
prevlm_fit <- lm_mod %>% 
  fit(`TB prevalence (cases per 100 000 population)` ~ subgroup,
      data = highburden) #fit linear model
prevfittable <- tidy(prevlm_fit) 
print(prevfittable) #produce tidy table of fitted model

table_file5 = here("results", "statistical_analysis", "model fit tables", "prevfittable.rds")
saveRDS(prevfittable, file = table_file5) #save summary table

#fit model with full data
prevlm_fitFD <- lm_mod %>% 
  fit(`TB prevalence (cases per 100 000 population)` ~ subgroup,
      data = wide_data) #fit linear model
prevfittableFD <- tidy(prevlm_fitFD) 
print(prevfittableFD) #produce tidy table of fitted model

table_file6 = here("results", "statistical_analysis", "model fit tables", "prevfittableFD.rds")
saveRDS(prevfittableFD, file = table_file6)


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
treeplot1 = here("results", "statistical_analysis", "tune plots", "wdinctreeplot.rds")
saveRDS(inctreeplot, file = treeplot1)

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

#save for later comparison: RMSE based on predictions from best model
wdinc_rmse <- inctree_resamp %>% 
  collect_predictions(parameters = wdincbest_tree) %>% 
  rmse(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  mutate(model = "wdinc")

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
treeplot2 = here("results", "statistical_analysis", "tune plots", "hbinctreeplot.rds")
saveRDS(inctreeplot, file = treeplot2)

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

#save for later comparison
hbinc_rmse <- inctree_resamp %>% 
  collect_predictions(parameters = hbincbest_tree) %>% 
  rmse(`TB incidence (new infections per 100 000 population)`, .pred) %>% 
  mutate(model = "hbinc")

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
treeplot3 = here("results", "statistical_analysis", "tune plots", "wdmorttreeplot.rds")
saveRDS(morttreeplot, file = treeplot3)

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

#save for later comparison: RMSE based on predictions from best model
wdmort_rmse <- morttree_resamp %>% 
  collect_predictions(parameters = wdmortbest_tree) %>% 
  rmse(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  mutate(model = "wdmort")

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
treeplot4 = here("results", "statistical_analysis", "tune plots", "hbmorttreeplot.rds")
saveRDS(morttreeplot, file = treeplot4)

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

#save for later comparison: RMSE based on predictions from best model
hbmort_rmse <- morttree_resamp %>% 
  collect_predictions(parameters = hbmortbest_tree) %>% 
  rmse(`TB mortality (deaths per 100 000 population)`, .pred) %>% 
  mutate(model = "hbmort")

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

treeplot5 = here("results", "statistical_analysis", "tune plots", "wdprevtreeplot.rds")
saveRDS(prevtreeplot, file = treeplot5)

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

#save for later comparison: RMSE based on predictions from best model
wdprev_rmse <- prevtree_resamp %>% 
  collect_predictions(parameters = wdprevbest_tree) %>% 
  rmse(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  mutate(model = "wdprev")

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

treeplot6 = here("results", "statistical_analysis", "tune plots", "hbprevtreeplot.rds")
saveRDS(prevtreeplot, file = treeplot6)

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

#save for later comparison: RMSE based on predictions from best model
hbprev_rmse <- prevtree_resamp %>% 
  collect_predictions(parameters = hbprevbest_tree) %>% 
  rmse(`TB prevalence (cases per 100 000 population)`, .pred) %>% 
  mutate(model = "hbprev")

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
treeplot7 = here("results", "statistical_analysis", "tune plots", "wdbcgtreeplot.rds")
saveRDS(bcgtreeplot, file = treeplot7)

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

#save for later comparison: RMSE based on predictions from best model
wdbcg_rmse <- bcgtree_resamp %>% 
  collect_predictions(parameters = wdbcgbest_tree) %>% 
  rmse(`BCG immunization coverage among one-year-olds (%)`, .pred) %>% 
  mutate(model = "wdbcg")

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
treeplot8 = here("results", "statistical_analysis", "tune plots", "hbbcgtreeplot.rds")
saveRDS(bcgtreeplot, file = treeplot8)

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

#save for later comparison: RMSE based on predictions from best model
hbbcg_rmse <- bcgtree_resamp %>% 
  collect_predictions(parameters = hbbcgbest_tree) %>% 
  rmse(`BCG immunization coverage among one-year-olds (%)`, .pred) %>% 
  mutate(model = "hbbcg")

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

treeplot9 = here("results", "statistical_analysis", "tune plots", "wdcatatreeplot.rds")
saveRDS(catatreeplot, file = treeplot9)

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

#save for later comparison: RMSE based on predictions from best model
wdcata_rmse <- catatree_resamp %>% 
  collect_predictions(parameters = wdcatabest_tree) %>% 
  rmse(`Families affected by TB facing catastrophic costs due to TB (%)`, .pred) %>% 
  mutate(model = "wdcata")

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
treeplot10 = here("results", "statistical_analysis", "tune plots", "hbcatatreeplot.rds")
saveRDS(catatreeplot, file = treeplot10)

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

#save for later comparison: RMSE based on predictions from best model
hbcata_rmse <- catatree_resamp %>% 
  collect_predictions(parameters = hbcatabest_tree) %>% 
  rmse(`Families affected by TB facing catastrophic costs due to TB (%)`, .pred) %>% 
  mutate(model = "hbcata")

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
treeplot11 = here("results", "statistical_analysis", "tune plots", "wdcasetreeplot.rds")
saveRDS(catatreeplot, file = treeplot11)

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

#save for later comparison: RMSE based on predictions from best model
wdcase_rmse <- casetree_resamp %>% 
  collect_predictions(parameters = wdcasebest_tree) %>% 
  rmse(`Case detection rate (%)`, .pred) %>% 
  mutate(model = "wdcase")

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
treeplot12 = here("results", "statistical_analysis", "tune plots", "hbcasetreeplot.rds")
saveRDS(catatreeplot, file = treeplot12)

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

#save for later comparison: RMSE based on predictions from best model
hbcase_rmse <- casetree_resamp %>% 
  collect_predictions(parameters = hbcasebest_tree) %>% 
  rmse(`Case detection rate (%)`, .pred) %>% 
  mutate(model = "hbcase")

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
treeplot13 = here("results", "statistical_analysis", "tune plots", "wdptntreeplot.rds")
saveRDS(ptntreeplot, file = treeplot13)

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

#save for later comparison: RMSE based on predictions from best model
wdptn_rmse <- ptntree_resamp %>% 
  collect_predictions(parameters = wdptnbest_tree) %>% 
  rmse(`Prevalence to notification ratio (years)`, .pred) %>% 
  mutate(model = "wdptn")

## ---- hbptnmodel --------
#recipe predicting TB prevalence to notification
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
treeplot14 = here("results", "statistical_analysis", "tune plots", "hbptntreeplot.rds")
saveRDS(ptntreeplot, file = treeplot14)

#show the best model
ptntree_resamp %>% 
  show_best()

#select best model
hbptnbest_tree <- ptntree_resamp %>% select_best("rmse")

#create final workflow
hbptnfinal_wf <- ptnwflow %>% finalize_workflow(hbptnbest_tree)

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

#save for later comparison: RMSE based on predictions from best model
hbptn_rmse <- ptntree_resamp %>% 
  collect_predictions(parameters = hbptnbest_tree) %>% 
  rmse(`Prevalence to notification ratio (years)`, .pred) %>% 
  mutate(model = "hbptn")

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
treeplot15 = here("results", "statistical_analysis", "tune plots", "wdatttreeplot.rds")
saveRDS(atttreeplot, file = treeplot15)

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

#save for later comparison: RMSE based on predictions from best model
wdatt_rmse <- atttree_resamp %>% 
  collect_predictions(parameters = wdattbest_tree) %>% 
  rmse(`People who would want a family member's TB kept secret - Male (%)`, .pred) %>% 
  mutate(model = "wdatt")

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
treeplot16 = here("results", "statistical_analysis", "tune plots", "hbatttreeplot.rds")
saveRDS(atttreeplot, file = treeplot16)

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

#save for later comparison: RMSE based on predictions from best model
hbatt_rmse <- atttree_resamp %>% 
  collect_predictions(parameters = hbattbest_tree) %>% 
  rmse(`People who would want a family member's TB kept secret - Male (%)`, .pred) %>% 
  mutate(model = "hbatt")

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
treeplot17 = here("results", "statistical_analysis", "tune plots", "wdknowtreeplot.rds")
saveRDS(knowtreeplot, file = treeplot17)

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

#save for later comparison: RMSE based on predictions from best model
wdknow_rmse <- knowtree_resamp %>% 
  collect_predictions(parameters = wdknowbest_tree) %>% 
  rmse(`People who report TB is spread through coughing - Male (%)`, .pred) %>% 
  mutate(model = "wdknow")

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
treeplot18 = here("results", "statistical_analysis", "tune plots", "hbknowtreeplot.rds")
saveRDS(knowtreeplot, file = treeplot18)

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

#save for later comparison: RMSE based on predictions from best model
hbknow_rmse <- knowtree_resamp %>% 
  collect_predictions(parameters = hbknowbest_tree) %>% 
  rmse(`People who report TB is spread through coughing - Male (%)`, .pred) %>% 
  mutate(model = "hbknow")

#plot decision tree
hbknowfinal_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

## ---- summary ---------
#create summary table of RMSE of best models and save
summary <- bind_rows(wdinc_rmse, hbinc_rmse,
                     wdmort_rmse, hbmort_rmse,
                     wdprev_rmse, hbprev_rmse,
                     wdbcg_rmse, hbbcg_rmse,
                     wdcata_rmse, hbcata_rmse,
                     wdcase_rmse, hbcase_rmse,
                     wdptn_rmse, hbptn_rmse,
                     wdatt_rmse, hbatt_rmse,
                     wdknow_rmse, hbknow_rmse)
summary
table_file1 = here("results", "statistical_analysis", "summaryrmse.rds")
saveRDS(summary, file = table_file1)