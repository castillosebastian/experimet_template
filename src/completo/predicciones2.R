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


# Data
# Let us load the Australian retail trade turnover dataset tsibble::aus_retail . 
# The time series measure is Turnover which represents the retail turnover in $Million AUD.
# Our final goal is to forecast Turnover for the next year.
# Within the dataset each series is uniquely identified using two keys:
# -State: The Australian state (or territory)
# -Industry: The industry of retail trade
# We will focus on the “Australian Capital Territory” State value only and use all Industry values.

