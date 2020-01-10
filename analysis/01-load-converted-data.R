# read in all converted files and tidy them up

# load some packages
library(tidyverse)
library(lubridate)

# load survival data
surv_data <- readRDS("data/converted/survival-data.rds")
surv_data <- surv_data %>% map(
  mutate,
  planting_date_formatted = parse_date_time(`Planting Date`, orders = c("ymd_HMS", "ymd", "dmy")),
  date_formatted = parse_date_time(Date, orders = c("ymd_HMS", "ymd", "dmy"))
)
## Date column wrong in
# c(68, 63, 59, 49, 40, 36, 19, 16, 15, 14, 13, 12)

## use
# surv_data[-8] %>% map(function(x) x %>% select("Health") %>% unique)
## to work out which levels need tidying

# now we want to create new columns in our data that tell us when each plant was
#   surveyed, and whether it was alive or dead at each observation
surv_data <- surv_data %>% map(
  mutate,
  days = (planting_date_formatted %--% date_formatted) %>% magrittr::divide_by(ddays(1)),
  alive = if_else(Health == "Dead", 0, 1)
)

# now those are sorted, we can combine all five data sets into one big data set
#  (automatically matching columns or adding new columns if needed)
surv_data <- surv_data %>% bind_rows

# add a combined site-by-planting date column to the species data
surv_data <- surv_data %>% mutate(
  site_by_planting <- paste(site, planting_date_formatted, sep = "_")
)

# we need to load the rainfall data as well
rain_data <- read_csv("data/converted/rainfall-data.csv")
rain_data <- rain_data %>% mutate(
  date_formatted = parse_date_time(planting_date, orders = c("dmy_HM", "dmy"))
)

# calculate average rainfall and total rainfall deviations in each year following
#   planting (for each planting date and site)
rain_data <- rain_data %>%
  group_by(site, date_formatted) %>%
  summarise(rainfall_deviation_year1_mm = sum(monthly_deviation_from_mean_year1_mm),
            rainfall_deviation_year2_mm = sum(monthly_deviation_from_mean_year2_mm),
            rainfall_30days_prior_mm = median(rainfall_30days_prior_mm))

# add up the deviations from years 1 and 2
rainfall_deviations <- rainfall_deviations %>% mutate(
  rainfall_deviation_mm = rainfall_deviation_year1_mm + rainfall_deviation_year2_mm
)

# add a combined site/planting date to give "cohort" for the rainfall data
rain_data <- rain_data %>% mutate(
  site_by_planting = paste(site, planting_date_formatted, sep = "_")
)

# let's join the planting and rainfall data based on the `site_by_planting` column
data_set <- surv_data %>% left_join(rain_data, by = "site_by_planting")


# now we want to reduce to the final alive observation of each individual
#  maybe filter to only alive, then pull out max?
#  but we actually want to record first "dead" record not last "alive" record


## need to add a "censored" column if individual alive at final obs


## filter out NAs and anything observed only at first planting and never again



## create model-ready data set and save
# First, we make up a matrix of predictor variables in "design matrix" format
## WORK OUT APPROPRIATE PREDICTORS
## - planted vs natural recruits (Fixed?)
## - treatments (Fixed?) (list these from all data sets, will be easier after left_join)
## - source population (Random?)
design_mat <- model.matrix(~ spp, data = data_test)

# now we can combine all of the bits and pieces into our model
alive <- data_set$alive == 1  # (check only 0/1 possible)
mod_data <- list(Nobs = sum(!alive),              # number of individuals that died
                 Ncen = sum(alive),              # number that were alive at last visit
                 M_bg = ncol(design_mat),                          # number of covariates
                 yobs = data_test$days[!alive],   # when was an individual first recorded as dead?
                 ycen = data_test$days[alive],   # when was a still-alive individual last visited?
                 Xobs_bg = design_mat[!alive, ],  # predictors for the individuals that died
                 Xcen_bg = design_mat[alive, ])  # predictors for individuals that are still alive
saveRDS(mod_data, file = "data/compiled/model-data.rds")
