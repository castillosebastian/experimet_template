# PART 1 -  FEATURE ENGINEERING
pacman::p_load(tidyverse,
               timetk,
               tsibble,
               tsibbledata,
               fastDummies,
               skimr, 
               tidymodels)

# inspiration
# https://blog.bguarisma.com/time-series-forecasting-lab-part-2-feature-engineering-with-recipes
# https://www.rstudio.com/blog/update-your-machine-learning-pipeline-with-vetiver-and-quarto/
# https://pins.rstudio.com/articles/pins.html#versioning

#---- Params

# poblacion

# Prepared and future datasets

data_prepared_tbl <- groups_fe_tbl %>%
  filter(!is.na(Turnover)) %>%
  drop_na()

data_prepared_tbl %>% glimpse()


future_tbl <- groups_fe_tbl %>%
  filter(is.na(Turnover))

# Train and test datasets split
splits <- data_prepared_tbl %>%
  time_series_split(Month, 
                    assess = "86 months", 
                    cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  filter(Industry == Industries[1]) %>%
  plot_time_series_cv_plan(.date_var = Month, 
                           .value = Turnover, 
                           .title = paste0("Split for ", Industries[1]))


# Create recipe

recipe_spec <- recipe(Turnover ~ ., data = training(splits)) %>%
  update_role(rowid, new_role = "indicator") %>%  
  step_other(Industry) %>%
  step_timeseries_signature(Month) %>%
  step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_normalize(Month_index.num, Month_year)

recipe_spec

# Recipe summary: 50 predictors (including all dummy variables) + 1 indicator + 1 outcome
recipe_spec %>% 
  prep() %>%
  juice() %>% 
  glimpse() 

# Month_index.num normalization Parameters
Month_index.num_limit_lower = 420595200
Month_index.num_limit_upper = 1317427200

# Month_year normalization Parameters
Month_year_limit_lower = 1983
Month_year_limit_upper = 2011

feature_engineering_artifacts_list <- list(
  # Data
  data = list(
    data_prepared_tbl = data_prepared_tbl,
    future_tbl      = future_tbl,
    industries = Industries
  ),
  
  # Recipes
  recipes = list(
    recipe_spec = recipe_spec
  ),
  
  # Splits
  splits = splits,
  
  # Inversion Parameters
  standardize = list(
    std_mean = std_mean,
    std_sd   = std_sd
  ),
  
  normalize = list(
    Month_index.num_limit_lower = Month_index.num_limit_lower, 
    Month_index.num_limit_upper = Month_index.num_limit_upper,
    Month_year_limit_lower = Month_year_limit_lower,
    Month_year_limit_upper = Month_year_limit_upper
  )  
)

dir.create(paste0(HOME_DIR, "/exp/001/"), showWarnings = FALSE )
archivo_salida  <-  paste0(HOME_DIR,"/exp/001/feature_engineering_artifacts_list.rds")

feature_engineering_artifacts_list %>% 
  write_rds(archivo_salida)



