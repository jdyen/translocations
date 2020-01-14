# load all raw data sets and convert to CSV or rds

# need a few R packages to get everything running
library(tidyverse)
library(readxl)

# we want a list of all the raw data files
file_path <- dir("data/raw")

# but we don't want to load the rainfall file here, so we remove it from
#   the list
file_path <- file_path[grep("rainfall", file_path, invert = TRUE)]

# pull out a list of sites and species from the file names
sites_list <- sites <- file_path %>%
  sapply(strsplit, split = "_") %>%
  sapply(function(x) x[2])
sites <- sites_list %>%
  unique %>%
  sort %>%
  tibble
species_list <- file_path %>%
  sapply(strsplit, split = "_") %>%
  sapply(function(x) x[1]) 
species <- species_list %>%
  unique %>%
  sort %>%
  tibble

# save these to their own file
sites %>% write_csv(path = "data/converted/site-list.csv", col_names = FALSE)
species %>% write_csv(path = "data/converted/species-list.csv", col_names = FALSE)

# load the data sets and save them in the vector `surv_data`
translocation_data <- paste0("data/raw/", file_path) %>%
  map(function(x) read_excel(path = x))

# add in site and species IDs to the data sets
translocation_data <- list(translocation_data, sites_list, species_list) %>% pmap(
  function(x, y, z) add_column(x, site = rep(y, nrow(x)), species = rep(z, nrow(x)))
)

# write to rds file
translocation_data %>% saveRDS(
  file = "data/converted/translocation-data.rds"
)

# read in the xlsx version of the rainfall data
rainfall_data <- read_excel("data/raw/Translocation site longterm rainfall data.xlsx")

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

# write to CSV
rainfall_data %>% write_csv(path = "data/converted/rainfall-data.csv")
