# load all raw data sets and convert to CSV or rds

# load all raw data files
get_file_list <- function(directory, ignore = NULL) {
  
  # we want a list of all the raw data files
  file_path <- dir(directory)
  
  # but we don't want to load the rainfall file here, so we remove it from
  #   the list
  if (!is.null(ignore)) {
    for (i in seq_along(ignore))
      file_path <- file_path[grep(ignore[i], file_path, invert = TRUE)]
  }

  # return
  file_path
  
}

# pull the site names out of file_list
create_site_list <- function(file_list, file) {

  # create a list of sites from the raw file names
  sites_list <- file_list %>%
    sapply(strsplit, split = "_") %>%
    sapply(function(x) x[2])
  sites <- sites_list %>%
    unique %>%
    sort %>%
    tibble
  
  # save unique sites to a file for checking
  sites %>% write_csv(file)
  
  # return
  sites_list
  
}

# pull out a list of sites and species from the file names
create_species_list <- function(file_list, file) {

  # create a list of species from the raw file names
  species_list <- file_list %>%
    sapply(strsplit, split = "_") %>%
    sapply(function(x) x[1]) 
  species <- species_list %>%
    unique %>%
    sort %>%
    tibble
  
  # save unique species to a file for checking
  species %>% write_csv(file)
  
  # return outputs
  species_list
  
}

# load the data sets and save them in a vector
load_translocation_data <- function(file_list, directory, sites, species) {
  
  translocation_data <- paste0(directory, file_list) %>%
    map(function(x) read_excel(path = x))
  
  # add in site and species IDs to the data sets
  translocation_data <- list(translocation_data, sites, species) %>% pmap(
    function(x, y, z) add_column(x, site = rep(y, nrow(x)), species = rep(z, nrow(x)))
  )
  
  # return
  translocation_data
  
}

# read in the xlsx version of the rainfall data
load_rainfall_data <- function(file) {

  rainfall_data <- read_excel(file)
  
  # four of the site names in the rainfall data do not match those in the plant data
  rainfall_data$site <- gsub("Gunyidi Townsite_West", "Gunyidi Townsite", rainfall_data$site)
  rainfall_data$site <- gsub("Wongan Hills NR", "Wongan", rainfall_data$site)
  rainfall_data$site <- gsub("Boundary Road", "Boundary Rd", rainfall_data$site)
  rainfall_data$site <- gsub("Mt ManyPeaks NR", "Mt Manypeaks NR", rainfall_data$site)
  
  # the column names of rainfall data are incorrect for monthly deviations
  rainfall_data <- rainfall_data %>% rename(
    monthly_deviation_from_mean_year1_mm = monthly_deviation_from_mean_year1_mm...10,
    monthly_deviation_from_mean_year2_mm = monthly_deviation_from_mean_year1_mm...11
  )
  
  # return
  rainfall_data
  
}

# read in all converted files and tidy them up

# how long did the individual survive?
calculate_days_survived <- function(days, alive) {
  
  # what is the final alive/dead state of an individual?
  final_state <- alive[which.max(days)]
  
  # is an individual alive at last sighting?
  if (final_state == 1) {
    out <- max(days)
  } else {  # if not, which is the first dead observation?
    out <- min(days[alive == 0])
  }
  
  # return
  out
  
}

# is an individual alive at last sighting?
is_censored <- function(days, alive) {
  
  # what is the final alive/dead state of an individual?
  final_state <- alive[which.max(days)]
  
  # is an individual alive at last sighting?
  if (final_state == 1) {
    censored <- 1
  } else {  # if not, it's not censored
    censored <- 0
  }
  
  # return
  censored
  
}

# sometimes treatments are entered with a mix of NA and true
#   treatment values within individuals. Calculate the unique
#   treatment and ditch the NA
unique_no_na <- function(x) {
  out <- unique(x)
  if (length(out) > 1)
    out <- out[!is.na(out)]
  out
}

# load survival data
tidy_translocation_data <- function(data) {
  
  translocation_data <- data %>% map(
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
  
  # return
  translocation_data
  
}

# correct mistakes based on a xlsx file of known errors
correct_errors <- function(data, corrections) {
  
  # load in data from a separate excel sheet of corrections
  corrections <- read_excel(corrections)

  # fix up some of the formatting in the corrections file so we can match
  #   it to the full data set
  corrections <- corrections %>% mutate(
    planting_date_formatted = parse_date_time(`Planting Date`, orders = c("ymd_HMS", "ymd", "dmy")),
    date_formatted = parse_date_time(Date, orders = c("ymd_HMS", "ymd", "dmy")),
    plant_no = as.character(`Plant no.`)
  )
  
  # join up the corrections to the full data set
  data_corrected <- corrections %>%
    select(Species, plant_no, planting_date_formatted, date_formatted,
           Height, `Crown 1`, `Crown 2`, `Mean Crown`) %>% 
    right_join(data, by = c("Species" = "species",
                            "plant_no" = "plant_no",
                            "planting_date_formatted" = "planting_date",
                            "date_formatted" = "survey_date"))

  # replace corrected values as needed  
  data_corrected <- data_corrected %>%
    mutate(
      height = ifelse(is.na(Height), height, Height),
      crown_one = ifelse(is.na(`Crown 1`), crown_one, `Crown 1`),
      crown_two = ifelse(is.na(`Crown 2`), crown_two, `Crown 2`),
      mean_crown = ifelse(is.na(`Mean Crown`), mean_crown, `Mean Crown`)
    )
  
  # remove excess columns
  data_corrected <- data_corrected %>%
    select(-Height, -`Crown 1`, -`Crown 2`, -`Mean Crown`)
  
  # and rename the replaced columns
  data_corrected <- data_corrected %>%
    rename(species = Species,
           planting_date = planting_date_formatted,
           survey_date = date_formatted)
  
  # return
  data_corrected
  
}

# pull out some summary stats from the full translocation data set
calculate_treatment_sizes <- function(data, file) {
  
  # calculate number of non-NA observations in each treatment
  treatment_data <- data %>%
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
  treatment_data %>% write_csv(path = file)
  
}

# function to tidy the rainfall data
tidy_rainfall_data <- function(rainfall_data) {
  
  # clean up the dates  
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
  
  # return
  rainfall_data
  
}

# make a new data set that identifies the first "Dead" record for each individual
#   or marks individuals alive at latest survey as "Censored"
calculate_survival <- function(data, rainfall, file) {
  
  # filter to valid observations and calculate days survived
  survival_data <- data %>% 
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
  
  # let's join the survival and rainfall data based on the `site` and
  #   `planting_date` columns
  survival_data <- survival_data %>% left_join(
    rainfall, by = c("site", "planting_date" = "planting_date_formatted")
  )
  
  # now we can save a compiled version of the survival data for use in analyses
  saveRDS(survival_data, file = file)
  
}

# make a new data set that identifies the reproductive state of each individual
#   at each survey
calculate_reproduction <- function(data, rainfall, file) {
  
  # filter to valid observations and calculate reproductive status
  reproduction_data <- data %>% 
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
    rainfall, by = c("site", "planting_date" = "planting_date_formatted")
  )
  
  # now we can save a compiled version of the reproduction data for use in analyses
  saveRDS(reproduction_data, file = file)
  
}

## growth: average annual/daily growth per individual? Per observation?
## main points:
##   - individual growth at age (need to incorporate age)
##   - conditional on survival (do we need to mark deaths?)
calculate_growth <- function(data, rainfall, file) {
  
  # filter to valid observations
  growth_data <- data %>%
    filter(!is.na(plant_no), !is.na(days), !is.na(alive), planted == "planted")
  
  # there are a few species with only one crown measurement,
  #   fill the mean measurements with the single value where relevant
  growth_data <- growth_data %>% mutate(
    mean_crown = ifelse(is.na(mean_crown), crown_one, mean_crown),
    mean_crown = ifelse(is.na(mean_crown), crown_two, mean_crown)
  )
  
  # filter a second time and calculate size-at-age
  growth_data <- growth_data %>%
    filter(alive == 1, !is.na(mean_crown), !is.na(height)) %>%
    filter(mean_crown > 0) %>%
    group_by(species, site, plant_no, planting_date, survey_date) %>%
    summarise(mean_crown = mean(mean_crown),
              height = mean(height),
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
    rainfall, by = c("site", "planting_date" = "planting_date_formatted")
  )
  
  # now we can save a compiled version of the reproduction data for use in analyses
  saveRDS(growth_data, file = file)
  
}

# make a quick plot of the growth data to check outliers
plot_crown_trajectories <- function(path, file) {
  
  on.exit(dev.off())
  pdf(file = file, width = 14, height = 10)
  
  # load growth data
  growth_data <- readRDS(path)
  
  # create plot of growth trajectories
  crown_plot <- ggplot(data = growth_data, aes(days, mean_crown)) +
    geom_point(color = "steelblue") +
    labs(x = "Days since planting", y = "Mean crown") +
    facet_wrap( ~ species, scales = "free") +
    theme(
      strip.text.x = element_text(margin = margin(2, 0, 2, 0),
                                  size = 8)
    )
  
  # plot to file
  print(crown_plot)

}

# make a quick plot of the growth data to check outliers
plot_height_trajectories <- function(path, file) {

  on.exit(dev.off())
  pdf(file = file, width = 14, height = 10)
  
  # load growth data
  growth_data <- readRDS(path)
  
  # create plot of growth trajectories
  height_plot <- ggplot(data = growth_data, aes(days, height)) +
    geom_point(color = "steelblue") +
    labs(x = "Days since planting", y = "Height") +
    facet_wrap( ~ species, scales = "free") +
    theme(
      strip.text.x = element_text(margin = margin(2, 0, 2, 0),
                                  size = 8)
    )
  
  # plot to file
  print(height_plot)

}

## recruitment: 1 for species with natural recruits, 0 otherwise
##    Need to account for time-since-planting somehow
##    (could copy logistic regression idea for survival model)
## Needs more thinking -- want to capture time-since-planting,
##    but also numbers of recruits, and time of first recruitment (vs repeat obs)
##   Some of these are counted multiple times -- once for each survey
calculate_recruitment <- function(data, rainfall, file) {
  
  # filter to valid observations and calculate number of recruits
  recruitment_data <- data %>% 
    filter(planted == "natural_recruit") %>%
    group_by(species, site, planting_date, survey_date) %>%
    summarise(date = unique(survey_date),
              initial_date = unique(planting_date),
              n_recruit = n()) %>%
    ungroup
  
  # let's join the recruitment and rainfall data based on the `site` and
  #   `date` columns
  recruitment_data <- recruitment_data %>% left_join(
    rainfall, by = c("site", "date" = "date_formatted")
  )
  
  # now we can save a compiled version of the reproduction data for use in analyses
  saveRDS(recruitment_data, file = file)
  
}
