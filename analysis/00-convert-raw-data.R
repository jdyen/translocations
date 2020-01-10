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
sites <- file_path %>%
  sapply(strsplit, split = "_") %>%
  sapply(function(x) x[2]) %>%
  unique %>%
  sort %>%
  tibble
species <- file_path %>%
  sapply(strsplit, split = "_") %>%
  sapply(function(x) x[1]) %>%
  unique %>%
  sort %>%
  tibble

# save these to their own file
write_csv(sites, path = "data/converted/site-list.csv", col_names = FALSE)
write_csv(species, path = "data/converted/species-list.csv", col_names = FALSE)

# load the data sets and save them in the vector `surv_data`
surv_data <- paste0("data/raw/", file_path) %>%
  map(function(x) read_excel(path = x))

# write to rds file
surv_data %>% saveRDS(
  file = "data/converted/survival-data.rds"
)
