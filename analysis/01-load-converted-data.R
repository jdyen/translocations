# read in all converted files and tidy them up

# load some packages
library(tidyverse)
library(lubridate)
library(ggplot2)

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
translocation_data <- translocation_data %>% map(
  ~ mutate(.x, 
      Health = case_when(
        Health == "healthy" ~ "Healthy",
        Health == "Healthly" ~ "Healthy",
        Health == "H" ~ "Healthy",
        Health == "H eaten" ~ "Healthy",
        Health == "hralthy" ~ "Healthy",
        Health == "Health" ~ "Healthy",
        Health == "Ununhealthy" ~ "Unhealthy",
        Health == "unhealthy" ~ "Unhealthy",
        Health == "poor" ~ "Unhealthy",
        Health == "Poor" ~ "Unhealthy",
        Health == "Dea" ~ "Dead",
        Health == "D" ~ "Dead",
        Health == "DEAD" ~ "Dead",
        Health == "Yellow" ~ "NA",
        Health == "?" ~ "NA",
        TRUE ~ as.character(Health)
    )
  )
)
translocation_data <- translocation_data %>% map(
  ~ mutate(.x, Health = ifelse(Health == "NA", NA, Health))
)

# repeat for reproductive column
translocation_data <- translocation_data %>% map(
  ~ mutate(.x, 
      Reproductive = case_when(
        Reproductive == "Buds" ~ "Yes",
        Reproductive == "Noo" ~ "No",
        Reproductive == "N" ~ "No",
        TRUE ~ as.character(Reproductive)
    )
  )
)

# and for propagule type
translocation_data <- translocation_data %>% map(
  ~ mutate(.x, 
           `Propagule type` = case_when(
             `Propagule type` == "Cuttings" ~ "Cutting",
             `Propagule type` == "Seedlings" ~ "Seedling",
             `Propagule type` == "2, 6 and 1" ~ "NA",
             `Propagule type` == "Population 1A" ~ "NA",
             `Propagule type` == "Population 2" ~ "NA",
             `Propagule type` == "Population 2 and 3" ~ "NA",
             `Propagule type` == "Population 3" ~ "NA",
             `Propagule type` == "Population 5" ~ "NA",
             TRUE ~ as.character(`Propagule type`)
           )
  )
)
translocation_data <- translocation_data %>% map(
  ~ mutate(.x, `Propagule type` = ifelse(`Propagule type` == "NA", NA, `Propagule type`))
)

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

# pull out some summary stats from the full translocation data set
treatment_data <- translocation_data %>%
  group_by(species) %>%
  summarise(n_water = sum(!is.na(treatment_water)),
            n_mulch = sum(!is.na(treatment_mulch)),
            n_planting_time = sum(!is.na(treatment_planting_time)),
            n_fence = sum(!is.na(treatment_fence)),
            n_seedling_age = sum(!is.na(treatment_seedling_age)),
            n_shade = sum(!is.na(treatment_shade)),
            n_terra_cottem = sum(!is.na(treatment_terra_cottem)),
            n_pre_planting_burn = sum(!is.na(treatment_pre_planting_burn)),
            n_management_water = sum(!is.na(management_water)),
            n_management_fence = sum(!is.na(management_fence)),
            n_propagule_type = sum(!is.na(propagule_type)))

# save this to a file for review
treatment_data %>% write_csv(path = "data/compiled/treatment-list.csv")

# make a new data set that identifies the first "Dead" record for each individual
#   or marks individuals alive at latest survey as "Censored"
survival_data <- translocation_data %>% 
  filter(!is.na(plant_no), !is.na(days), !is.na(alive), planted == "planted") %>%
  group_by(species, site, plant_no, planting_date) %>%
  summarise(censored = is_censored(days = days, alive = alive),
            days = calculate_days_survived(days = days, alive = alive),
            source_population = unique(source_population),
            propagule_type = unique(propagule_type),
            tfsc_accession_no = unique(tfsc_accession_no),
            treatment_water = unique(treatment_water),
            treatment_mulch = unique(treatment_mulch),
            treatment_planting_time = unique(treatment_planting_time),
            treatment_fence = unique(treatment_fence),
            treatment_seedling_age = unique(treatment_seedling_age),
            treatment_shade = unique_no_na(treatment_shade),
            treatment_terra_cottem = unique(treatment_terra_cottem),
            treatment_pre_planting_burn = unique(treatment_pre_planting_burn),
            management_water = unique(management_water),
            management_fence = unique(management_fence)) %>%
  ungroup

# some things were never observed a second time; remove them
survival_data <- survival_data %>% filter(days > 0)

# we want to create a new source pop variable that accounts for
#   repeated source pop IDs among species. Ditto TFSC accession numbers
#   and plant numbers
survival_data <- survival_data %>% mutate(
  source_population = species %>% paste(source_population, sep = "_"),
  tfsc_accession_no = species %>% paste(tfsc_accession_no, sep = "_"),
  plant_no_unique = species %>% paste(plant_no, planting_date, sep = "_")
)

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
            rainfall_30days_prior_mm = median(rainfall_30days_prior_mm)) %>%
  ungroup

# add up the deviations from years 1 and 2
rainfall_data <- rainfall_data %>% mutate(
  rainfall_deviation_mm = rainfall_deviation_year1_mm + rainfall_deviation_year2_mm
)

# scale rainfall data
rainfall_data <- rainfall_data %>% mutate(
  rainfall_deviation_std = scale(rainfall_deviation_mm),
  rainfall_30days_prior_std = scale(rainfall_30days_prior_mm)
)

# let's join the survival and rainfall data based on the `site` and
#   `planting_date` columns
survival_data <- survival_data %>% left_join(
  rainfall_data, by = c("site", "planting_date" = "planting_date_formatted")
)

# now we can save a compiled version of the survival data for use in analyses
saveRDS(survival_data, file = "data/compiled/survival-data.rds")

# make a new data set that identifies the reproductive state of each individual
#   at each survey
reproduction_data <- translocation_data %>% 
  filter(!is.na(plant_no), !is.na(days), !is.na(alive), planted == "planted") %>%
  filter(alive == 1, !is.na(reproductive)) %>%
  group_by(species, site, plant_no, planting_date, survey_date) %>%
  summarise(reproductive = unique(reproductive),
            days = unique(days),
            source_population = unique(source_population),
            propagule_type = unique(propagule_type),
            tfsc_accession_no = unique(tfsc_accession_no),
            treatment_water = unique(treatment_water),
            treatment_mulch = unique(treatment_mulch),
            treatment_planting_time = unique(treatment_planting_time),
            treatment_fence = unique(treatment_fence),
            treatment_seedling_age = unique(treatment_seedling_age),
            treatment_shade = unique_no_na(treatment_shade),
            treatment_terra_cottem = unique(treatment_terra_cottem),
            treatment_pre_planting_burn = unique(treatment_pre_planting_burn),
            management_water = unique(management_water),
            management_fence = unique(management_fence)) %>%
  ungroup

# let's convert the reproductive column to a binary variable with
#   1 for reproductive and 0 otherwise
reproduction_data <- reproduction_data %>% mutate(
  reproductive = if_else(reproductive == "Yes", 1, 0)
)

# we want to create a new source pop variable that accounts for
#   repeated source pop IDs among species. Ditto TFSC accession numbers
#   and plant numbers
reproduction_data <- reproduction_data %>% mutate(
  source_population = species %>% paste(source_population, sep = "_"),
  tfsc_accession_no = species %>% paste(tfsc_accession_no, sep = "_"),
  plant_no_unique = species %>% paste(plant_no, planting_date, sep = "_")
)

# let's join the reproduction and rainfall data based on the `site` and
#   `planting_date` columns
reproduction_data <- reproduction_data %>% left_join(
  rainfall_data, by = c("site", "planting_date" = "planting_date_formatted")
)

# now we can save a compiled version of the reproduction data for use in analyses
saveRDS(reproduction_data, file = "data/compiled/reproduction-data.rds")

## growth: average annual/daily growth per individual? Per observation?
## main points:
##   - individual growth at age (need to incorporate age)
##   - conditional on survival (do we need to mark deaths?)
growth_data <- translocation_data %>%
  filter(!is.na(plant_no), !is.na(days), !is.na(alive), planted == "planted") %>%
  filter(alive == 1, !is.na(mean_crown)) %>%
  filter(mean_crown > 0) %>%
  group_by(species, site, plant_no, planting_date, survey_date) %>%
  summarise(mean_crown = mean(mean_crown),
            days = unique(days),
            source_population = unique(source_population),
            propagule_type = unique(propagule_type),
            tfsc_accession_no = unique(tfsc_accession_no),
            treatment_water = unique(treatment_water),
            treatment_mulch = unique(treatment_mulch),
            treatment_planting_time = unique(treatment_planting_time),
            treatment_fence = unique(treatment_fence),
            treatment_seedling_age = unique(treatment_seedling_age),
            treatment_shade = unique_no_na(treatment_shade),
            treatment_terra_cottem = unique(treatment_terra_cottem),
            treatment_pre_planting_burn = unique(treatment_pre_planting_burn),
            management_water = unique(management_water),
            management_fence = unique(management_fence)) %>%
  ungroup

# let's join the reproduction and rainfall data based on the `site` and
#   `planting_date` columns
growth_data <- growth_data %>% left_join(
  rainfall_data, by = c("site", "planting_date" = "planting_date_formatted")
)

# make a quick plot of the growth data to check outliers
growth_plot <- ggplot(data = growth_data, aes(days, mean_crown)) +
  geom_point(color = "steelblue") +
  labs(x = "Days since planting", y = "Mean crown") +
  facet_wrap( ~ species, scales = "free") +
  theme(
    strip.text.x = element_text(margin = margin(2, 0, 2, 0),
                                size = 8)
  )
pdf(file = "outputs/figs/growth-variation.pdf",
    width = 14, height = 10)
growth_plot
dev.off()

# now we can save a compiled version of the reproduction data for use in analyses
saveRDS(growth_data, file = "data/compiled/growth-data.rds")

## recruitment: 1 for species with natural recruits, 0 otherwise
##    Need to account for time-since-planting somehow
##    (could copy logistic regression idea for survival model)
## Needs more thinking -- want to capture time-since-planting,
##    but also numbers of recruits, and time of first recruitment (vs repeat obs)
##   Some of these are counted multiple times -- once for each survey
recruitment_data <- translocation_data %>% 
  filter(planted == "natural_recruit") %>%
  group_by(species, site, planting_date, survey_date) %>%
  summarise(date = unique(survey_date),
            initial_date = unique(planting_date),
            n_recruit = n()) %>%
  ungroup
