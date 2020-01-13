# read in all converted files and tidy them up

# load some packages
library(tidyverse)
library(lubridate)

# load helper functions
source("R/functions.R")

# load survival data
surv_data <- readRDS("data/converted/survival-data.rds")
surv_data <- surv_data %>% map(
  mutate,
  planting_date_formatted = parse_date_time(`Planting Date`, orders = c("ymd_HMS", "ymd", "dmy")),
  date_formatted = parse_date_time(Date, orders = c("ymd_HMS", "ymd", "dmy"))
)

# some of health levels are incorrectly entered, let's fix them
surv_data <- surv_data %>% map(recode_levels)
surv_data <- surv_data %>% map(na_is_logical)

# now we want to create new columns in our data that tell us when each plant was
#   surveyed, and whether it was alive or dead at each observation
surv_data <- surv_data %>% map(
  mutate,
  days = (planting_date_formatted %--% date_formatted) %>% magrittr::divide_by(ddays(1)),
  alive = if_else(Health == "Dead", 0, 1)
)

# cast columns to single class (character or numeric), adding NAs as needed
surv_data <- surv_data %>% map(
  mutate,
  planted = if_else(`Planted/ seedling recruit` == "Planted",
                    "planted",
                    "natural_recruit"),
  `Planting Date` = as.character(`Planting Date`),
  Date = as.character(Date),
  `Plant no.` = as.character(`Plant no.`),
  plant_no = as.character(`Plant no.`),
  `KPBG no.` = as.character(`KPBG no.`),
  kpbg_no = as.character(`KPBG no.`),
  `TFSC Accession no.` = as.numeric(`TFSC Accession no.`),
  tfsc_accession_no = as.numeric(`TFSC Accession no.`),
  `Source Population` = as.character(`Source Population`),
  source_population = as.character(`Source Population`),
  propagule_type = `Propagule type`,
  replicate = Replicate,
  Height = as.numeric(Height),
  height = as.numeric(Height),
  `Crown 1` = as.numeric(`Crown 1`),
  crown_one = as.numeric(`Crown 1`),
  `Crown 2` = as.numeric(`Crown 2`),
  crown_two = as.numeric(`Crown 2`),
  `Mean Crown` = as.numeric(`Mean Crown`),
  mean_crown = as.numeric(`Mean Crown`),
  `Pres/abs of buds` = as.character(`Pres/abs of buds`),
  buds_present = as.character(`Pres/abs of buds`),
  `No. of bud inflorescences` = as.numeric(`No. of bud inflorescences`),
  bud_inflorescences_no = as.numeric(`No. of bud inflorescences`),
  `No. flower inflorescences` = as.numeric(`No. flower inflorescences`),
  flower_inflorescences_no = as.numeric(`No. flower inflorescences`),
  `No. of fruit` = as.numeric(`No. of fruit`),
  fruit_no = as.numeric(`No. of fruit`),
  reproductive = Reproductive,
  health = Health,
  `Comments` = as.character(`Comments`),
  comments = as.character(`Comments`)
)

# now those are sorted, we can combine all five data sets into one big data set
#  (automatically matching columns or adding new columns if needed)
surv_data <- surv_data %>% bind_rows

# tidy data set by renaming variables and dropping unused variables
surv_data <- surv_data %>% transmute(
  planted = planted,
  species = species,
  site = site,
  planting_date = planting_date_formatted,
  survey_date = date_formatted,
  plant_no = plant_no,
  kpbg_no = kpbg_no,
  tfsc_accession_no = tfsc_accession_no,
  source_population = source_population,
  propagule_type = propagule_type,
  replicate = replicate,
  treatment_water = Treatment_water,
  treatment_mulch = Treatment_mulch,
  treatment_planting_time = `Treatment_planting time`,
  treatment_fence = Treatment_fence,
  treatment_seedling_age = `Treatment_seedling age`,
  treatment_shade = Treatment_Shade,
  treatment_terra_cottem = Treatment_TerraCottem,
  treatment_pre_planting_burn = `Treatment_Pre planting burn`,
  management_water = Management_water,
  management_fence = Management_fence,
  days = days,
  alive = alive,
  height = height,
  crown_one = crown_one,
  crown_two = crown_two,
  mean_crown = mean_crown,
  buds_present = buds_present,
  bud_inflorescences_no = bud_inflorescences_no,
  flower_inflorescences_no = flower_inflorescences_no,
  fruit_no = fruit_no,
  reproductive = reproductive,
  health = health,
  comments = comments
)

# there are 17 NAs in the planted column; these are natural recruits
surv_data <- surv_data %>% mutate(
  planted = if_else(is.na(planted), "natural_recruit", planted)
)

# add a combined site-by-planting date column to the species data
surv_data <- surv_data %>% mutate(
  site_by_planting = paste(site, planting_date_formatted, sep = "_")
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
rain_data <- rain_data %>% mutate(
  rainfall_deviation_mm = rainfall_deviation_year1_mm + rainfall_deviation_year2_mm
)

# add a combined site/planting date to give "cohort" for the rainfall data
rain_data <- rain_data %>% mutate(
  site_by_planting = paste(site, date_formatted, sep = "_")
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
