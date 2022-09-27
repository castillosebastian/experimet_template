# PART 1 -  FEATURE ENGINEERING
pacman::p_load(tidyverse,
               timetk,
               tsibble,
               tsibbledata,
               fastDummies,
               skimr, 
               tidymodels, 
               modeltime,
               future, 
               doFuture,
               plotly)

# inspiration
#https://blog.bguarisma.com/time-series-forecasting-lab-part-4-hyperparameter-tuning

wflw_artifacts <- read_rds(paste0(HOME_DIR, "/exp/001/workflows_artifacts_list.rds"))
 

# Cross-validation plan
# A k-fold cross-validation will randomly split the training data into k groups of roughly equal size (called "folds"). A resample of the analysis data consisted of k-1 of the folds while the assessment set contains the final fold. In basic k-fold cross-validation (i.e. no repeats), the number of resamples is equal to k.

# k = 10 folds
set.seed(123)
resamples_kfold <- training(splits) %>% 
  vfold_cv(v = 10)

# Registers the doFuture parallel processing
registerDoFuture()

# My laptop i5-8350U CPU has 8 threads (or vCores)
n_cores <- parallel::detectCores()

# Prophet Boost
# Prophet Boost - Identify tuning parameters

model_spec_prophet_boost_tune <- prophet_boost(
  mode = "regression",
  # growth = NULL,
  changepoint_num = tune(),
  #changepoint_range = NULL,
  seasonality_yearly = FALSE,
  seasonality_weekly = FALSE,
  seasonality_daily = FALSE,
  # season = NULL,
  # prior_scale_changepoints = NULL,
  # prior_scale_seasonality = NULL,
  # prior_scale_holidays = NULL,
  # logistic_cap = NULL,
  # logistic_floor = NULL,
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  # sample_size = NULL,
  # stop_iter = NULL
  ) %>%
  set_engine("prophet_xgboost")

wflw_spec_prophet_boost_tune <- workflow() %>%
  add_model(model_spec_prophet_boost_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

#Prophet Boost - Grid spec
# Check parameters' range
# The first thing to do is to display the parameters of the model with extract_parameter_set_dials(): you must check if there is any parameter with missing information about its values range.
# 
# As shown below, nparam[?] means that values range is missing for mtry parameter.

extract_parameter_set_dials(model_spec_prophet_boost_tune)


grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_boost_tune) %>% 
    update(mtry = mtry(range = c(1, 50))),
  size = 20
)

tune_results_prophet_boost_1 <- wflw_spec_prophet_boost_tune %>%
  tune_grid(
    resamples  = resamples_kfold,
    grid = grid_spec_1,
    control = control_grid(verbose = TRUE, 
                           allow_par = TRUE)
  )

plan(strategy = sequential)

tune_results_prophet_boost_1 %>% 
  show_best("rmse", n = Inf)




