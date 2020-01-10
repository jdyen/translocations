# load all raw data sets and convert to CSV

# need a few R packages to get everything running
library(tidyverse)
library(readxl)
library(lubridate)

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

# write to CSVs
file_names <- file_path %>%
  strsplit(split = "\\.") %>%
  map(function(x) x[seq_len(length(x) - 1)]) %>%
  map(paste0) %>%
  map(gsub, pattern = "\\ ", replacement = "-") %>%
  map(paste0, ".csv")
surv_data %>% map2(
  file_names,
  function(x, y) write_csv(x, path = paste0("data/converted/", y))
)
