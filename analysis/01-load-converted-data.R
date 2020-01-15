# read in all converted files and tidy them up

# load some packages
library(tidyverse)
library(lubridate)

# load helper functions
source("R/functions.R")

# load survival data
translocation_data <- readRDS("data/converted/translocation-data.rds")
translocation_data <- translocation_data %>% map(
  mutate,
  planting_date_formatted = parse_date_time(`Planting Date`, orders = c("ymd_HMS", "ymd", "dmy")),
  date_formatted = parse_date_time(Date, orders = c("ymd_HMS", "ymd", "dmy"))
)

# some of health levels are incorrectly entered, let's fix them
translocation_data <- translocation_data %>% map(recode_levels)
translocation_data <- translocation_data %>% map(na_is_logical)

# now we want to create new columns in our data that tell us when each plant was
#   surveyed, and whether it was alive or dead at each observation
translocation_data <- translocation_data %>% map(
  mutate,
  days = (planting_date_formatted %--% date_formatted) %>% magrittr::divide_by(ddays(1)),
  alive = if_else(Health == "Dead", 0, 1)
)

# cast columns to single class (character or numeric), adding NAs as needed
translocation_data <- translocation_data %>% map(
  mutate,
  planted = if_else(`Planted/ seedling recruit` == "Planted",
                    "planted",
                    "natural_recruit"),
  `Planting Date` = as.character(`Planting Date`),
  Date = as.character(Date),
  `Plant no.` = as.character(`Plant no.`),
  plant_no = as.character(`Plant no.`),
  `KPBG no.` = as.character(`KPBG no.`),
  kpbg_no = as.character(`KPBG no.`),    ## less important than tfsc
  `TFSC Accession no.` = as.numeric(`TFSC Accession no.`),  ## seed collection batch
  tfsc_accession_no = as.numeric(`TFSC Accession no.`),
  `Source Population` = as.character(`Source Population`),  ## tfsc can catch different years within source pops
  source_population = as.character(`Source Population`),
  propagule_type = `Propagule type`,
  replicate = Replicate,  ## should be there with treatments
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
  reproductive = Reproductive, ## yes for buds
  health = Health,
  `Comments` = as.character(`Comments`),
  comments = as.character(`Comments`)
)

# now those are sorted, we can combine all five data sets into one big data set
#  (automatically matching columns or adding new columns if needed)
translocation_data <- translocation_data %>% bind_rows

# tidy data set by renaming variables and dropping unused variables
translocation_data <- translocation_data %>% transmute(
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
translocation_data <- translocation_data %>% mutate(
  planted = if_else(is.na(planted), "natural_recruit", planted)
)

# make a new data set that identifies the first "Dead" record for each individual
#   or marks individuals alive at latest survey as "Censored"
survival_data <- translocation_data %>% 
  filter(!is.na(plant_no), !is.na(days), !is.na(alive)) %>%
  group_by(species, site, plant_no, planting_date) %>%
  summarise(censored = is_censored(days = days, alive = alive),
            days = calculate_days_survived(days = days, alive = alive),
            treatment_water = unique(treatment_water),
            treatment_mulch = unique(treatment_mulch),
            treatment_planting_time = unique(treatment_planting_time),
            treatment_fence = unique(treatment_fence),
            treatment_seedling_age = unique(treatment_seedling_age),
            treatment_shade = unique_no_na(treatment_shade),
            treatment_terra_cottem = unique(treatment_terra_cottem),
            treatment_pre_planting_burn = unique(treatment_pre_planting_burn),
            management_water = unique(management_water),
            management_fence = unique(management_fence))

# some things were never observed a second time; remove them
survival_data <- survival_data %>% filter(days > 0)

# we need to load the rainfall data as well
rainfall_data <- read_csv("data/converted/rainfall-data.csv")
rainfall_data <- rainfall_data %>% mutate(
  planting_date_formatted = parse_date_time(planting_date, orders = c("ymd_HMS", "ymd"))
)

# calculate average rainfall and total rainfall deviations in each year following
#   planting (for each planting date and site)
rainfall_data <- rainfall_data %>%
  group_by(site, planting_date_formatted) %>%
  summarise(rainfall_deviation_year1_mm = sum(monthly_deviation_from_mean_year1_mm),
            rainfall_deviation_year2_mm = sum(monthly_deviation_from_mean_year2_mm),
            rainfall_30days_prior_mm = median(rainfall_30days_prior_mm))

# add up the deviations from years 1 and 2
rainfall_data <- rainfall_data %>% mutate(
  rainfall_deviation_mm = rainfall_deviation_year1_mm + rainfall_deviation_year2_mm
)

# let's join the survival and rainfall data based on the `site` and
#   `planting_date` columns
survival_data <- survival_data %>% left_join(
  rainfall_data, by = c("site", "planting_date" = "planting_date_formatted")
)

# now we can save a compiled version of the survival data for use in analyses
saveRDS(survival_data, file = "data/compiled/survival-data.rds")

# repeat for other data sets (growth, reproduction, recruitment)

## growth: average annual/daily growth per individual? Per observation?

## reproductive: tidy up categories

## recruitment: 1 for species with natural recruits, 0 otherwise
##    Need to account for time-since-planting somehow
