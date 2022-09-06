# data
pacman::p_load(AmesHousing)
# data cleaning
pacman::p_load(janitor)
# data prep
pacman::p_load(dplyr)
# visualisation
pacman::p_load(ggplot2)
# tidymodels
pacman::p_load(rsample)
pacman::p_load(recipes)
pacman::p_load(parsnip)
pacman::p_load(tune)
pacman::p_load(dials)
pacman::p_load(workflows)
pacman::p_load(yardstick)
pacman::p_load(treesnip)
pacman::p_load(doParallel)

# speed up computation with parallel processing
# all_cores <- parallel::detectCores(logical = FALSE)
# registerDoParallel(cores = all_cores)
# set the random seed so we can reproduce any simulated results.
set.seed(1234)
# load the housing data and clean names
ames_data <- make_ames() %>%
  janitor::clean_names()
#Split data into train and test
ames_split <- rsample::initial_split(
  ames_data,
  prop = 0.8,
  strata = sale_price
)
# preprocesing
preprocessing_recipe <-
  recipes::recipe(sale_price ~ ., data = training(ames_split)) %>%
  # combine low frequency factor levels
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  # remove no variance predictors which provide no predictive information 
  recipes::step_nzv(all_nominal()) %>%
  # prep the recipe so it can be used on other data
  prep()

# Find the best hyperparameters
ames_cv_folds <-
  recipes::bake(
    preprocessing_recipe,
    new_data = training(ames_split)
  ) %>%
  rsample::vfold_cv(v = 5)


# Model especification
lightgbm_model<-
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("lightgbm", objective = "rmse",verbose=-1)

lightgbm_params <-
  dials::parameters(
    # The parameters have sane defaults, but if you have some knowledge 
    # of the process you can set upper and lower limits to these parameters.
    min_n(), # 2nd important
    tree_depth() # 3rd most important
  )

lgbm_grid <-
  dials::grid_max_entropy(
    lightgbm_params,
    size = 30 # set this to a higher number to get better results
    # I don't want to run this all night, so I set it to 30
  )
head(lgbm_grid)

lgbm_wf <-
  workflows::workflow() %>%
  add_model(lightgbm_model
  ) %>%
  add_formula(sale_price ~ .)

lgbm_tuned <- tune::tune_grid(
  object = lgbm_wf,
  resamples = ames_cv_folds,
  grid = lgbm_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = T) # set this to TRUE to see
  # in what step of the process you are. But that doesn't look that well in
  # a blog.
)

#Find the best model from tuning results
lgbm_tuned %>%
  tune::show_best(metric = "rmse",n = 5)

# plot the performance per parameter.
lgbm_tuned %>%
  tune::show_best(metric = "rmse",n = 10) %>%
  tidyr::pivot_longer(min_n:tree_depth, names_to="variable",values_to="value" ) %>% 
  ggplot(aes(value,mean)) +
  geom_line(alpha=1/2)+
  geom_point()+
  facet_wrap(~variable,scales = "free")+
  ggtitle("Best parameters for RMSE")

# Since we asked for multiple metrics we can see the best performance for different metrics too.

lgbm_tuned %>%
  tune::show_best(metric = "mae",n = 10) %>%
  tidyr::pivot_longer(min_n:tree_depth, names_to="variable",values_to="value" ) %>%
  ggplot(aes(value,mean)) +
  geom_line(alpha=1/2)+
  geom_point()+
  facet_wrap(~variable,scales = "free")+
  ggtitle("Best parameters for MAE")

#Than we can select the best parameter combination for a metric, or do it manually.

lgbm_best_params <-
  lgbm_tuned %>%
  tune::select_best("rmse")

# Finalize the lgbm model to use the best tuning parameters.
lgbm_model_final <-
  lightgbm_model%>%
  finalize_model(lgbm_best_params)

lgbm_model_final


# falta entrenamiento final con todo los datos
#################################################################


# And evaluate on test data (yardstick)
test_processed <- bake(preprocessing_recipe, new_data = testing(ames_split))
test_prediction <-
  trained_model_all_data %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(ames_split))

# measure the accuracy of our model on training set (overestimation)
train_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable()

# measure the accuracy of our model on data it hasn’t seen before (testset)
test_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable()

# look at residuals
house_prediction_residual <- test_prediction %>%
  arrange(.pred) %>%
  mutate(residual_pct = (sale_price - .pred) / .pred) %>%
  select(.pred, residual_pct)

ggplot(house_prediction_residual, aes(x = .pred, y = residual_pct)) +
  geom_point() +
  xlab("Predicted Sale Price") +
  ylab("Residual (%)") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent)



