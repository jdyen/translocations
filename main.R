# main script to run the full analysis. Uses the drake R
# package to cache intermediate objects and save some runtime

# set a seed to keep the MCMC sampler reproducible
set.seed(2020-04-28)

# packages used for workflow management
library(drake)

# to load and prepare data objects
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

# to fit models
library(brms)

# to interrogate model outputs
library(magrittr)

# load the data ----------------------------------------------------------------

source("R/load-data.R")

load_and_tidy_data <- drake_plan(
  
  # load the raw excel files
  file_list = get_file_list(
    directory = file_in("data/raw"),
    ignore = c("rainfall", "corrected")
  ),
  sites = create_site_list(
    file_list,
    file_out("data/compiled/sites-list.csv")
  ),
  species = create_species_list(
    file_list, file_out("data/compiled/species-list.csv")
  ),
  translocation_data = load_translocation_data(
    file_list,
    file_in("data/raw/"),
    sites,
    species
  ),
  rainfall_data = load_rainfall_data(
    file_in("data/raw/Translocation site longterm rainfall data.xlsx")
  ),
  
  # tidy these loaded data sets
  translocation_tidy = tidy_translocation_data(translocation_data),
  
  # correct known errors in the data based on a table of corrections
  translocation_corrected = correct_errors(
    translocation_tidy,
    file_in("data/raw/corrected-growth-data.xlsx")
  ),
  
  # calculate some summary stats on the treatment sample sizes
  treatment_summaries = calculate_treatment_sizes(
    translocation_corrected,
    file_out("data/compiled/treatment-list.csv")
  ),
  
  # calculate the primary response variables
  rainfall_tidy = tidy_rainfall_data(rainfall_data),
  survival = calculate_survival(
    translocation_corrected, 
    rainfall_tidy, 
    file_out("data/compiled/survival-data.RDS")
  ),
  reproduction = calculate_reproduction(
    translocation_corrected,
    rainfall_tidy,
    file_out("data/compiled/reproduction-data.RDS")
  ),
  growth = calculate_growth(
    translocation_corrected,
    rainfall_tidy,
    file_out("data/compiled/growth-data.RDS")
  ),

  # make some summary plots to check data
  crown_plots = plot_crown_trajectories(
    file_in("data/compiled/growth-data.RDS"),
    file_out("outputs/figures/growth-variation-crown.pdf")
  ),
  height_plots = plot_height_trajectories(
    file_in("data/compiled/growth-data.RDS"),
    file_out("outputs/figures/growth-variation-height.pdf")
  )
  
)

make(load_and_tidy_data)

# fit models ----------------------------------------------------------------

source("R/modelling.R")

fit_models <- drake_plan(
  
  # load the data sets
  survival_data = readRDS(file_in("data/compiled/survival-data.RDS")),
  reproduction_data = readRDS(file_in("data/compiled/reproduction-data.RDS")),
  growth_tmp = readRDS(file_in("data/compiled/growth-data.RDS")),

  # define MCMC settings
  mcmc_settings = list(n_iter = 20000, n_thin = 10, n_chain = 4),
    
  # fit survival model
  survival_formula = days | cens(censored) ~  propagule_type +
    rainfall_deviation_std +
    rainfall_30days_prior_std +
    management_water +
    management_fence +
    (rainfall_deviation_std +
       rainfall_30days_prior_std +
       management_water +
       management_fence | species) +
    (1 | source_population) +
    (1 | site),
  survival_model = fit_survival_model(
    survival_formula,
    survival_data,
    mcmc_settings
  ),
  
  # fit reproduction model
  reproduction_formula = reproductive ~ 
    days +
    propagule_type +
    rainfall_deviation_std +
    rainfall_30days_prior_std +
    management_water +
    management_fence +
    (rainfall_deviation_std +
       rainfall_30days_prior_std +
       management_water +
       management_fence | species) +
    (1 | source_population) +
    (1 | site) +
    (1 | plant_no),
  reproduction_model = fit_reproduction_model(
    reproduction_formula,
    reproduction_data,
    mcmc_settings
  ),
  
  # fit crown growth model
  crown_growth_formula = mean_crown ~
    days +
    propagule_type +
    rainfall_deviation_std +
    rainfall_30days_prior_std +
    management_water +
    management_fence +
    (rainfall_deviation_std +
       rainfall_30days_prior_std +
       management_water +
       management_fence | species) +
    (1 | source_population) +
    (1 | site) +
    (1 | plant_no),
  crown_growth_model = fit_growth_model(
    crown_growth_formula,
    growth_tmp,
    mcmc_settings
  ),
  
  # fit height growth model
  height_growth_formula = height ~ 
    days +
    propagule_type +
    rainfall_deviation_std +
    rainfall_30days_prior_std +
    management_water +
    management_fence +
    (rainfall_deviation_std +
       rainfall_30days_prior_std +
       management_water +
       management_fence | species) +
    (1 | source_population) +
    (1 | site) +
    (1 | plant_no),
  height_growth_model = fit_growth_model(
    height_growth_formula,
    growth_tmp,
    mcmc_settings
  ),

)

# rstan models require lock_envir = FALSE to avoid issues
#   with drake
make(fit_models, lock_envir = FALSE)

# interrogate models -------------------------------------------------------------

source("R/plotting.R")

check_models <- drake_plan(

  # load the fitted models (could use qs if this doesn't work??)
  survival_model = readd(survival_model),
  reproduction_model = readd(reproduction_model),
  growth_model = readd(height_growth_model),
  
  # run some posterior predictive checks and save outputs to files
  survival_checks = posterior_checks(
    survival_model,
    type = c("dens_overlay", "error_hist"),
    file = file_out("outputs/figures/survival_pp_checks.pdf"),
    nsamples = 20
  ),
  reproduction_checks = posterior_checks(
    reproduction_model,
    type = c("bars", "bars_grouped"),
    file = file_out("outputs/figures/reproduction_pp_checks.pdf"),
    nsamples = 20,
    group = "species"
  ),
  height_growth_checks = posterior_checks(
    height_growth_model,
    type = c("dens_overlay", "error_hist"),
    file = file_out("outputs/figures/height_growth_pp_checks.pdf"),
    nsamples = 20
  ),
  crown_growth_checks = posterior_checks(
    crown_growth_model,
    type = c("dens_overlay", "error_hist"),
    file = file_out("outputs/figures/crown_growth_pp_checks.pdf"),
    nsamples = 20
  ),
  
  # calculate model r2 values
  survival_R2 = bayes_R2(survival_model),
  reproduction_R2 = bayes_R2(reproduction_model),
  growth_R2 = bayes_R2(growth_model),
  
)

make(check_models)

# summarise models -------------------------------------------------------------

# plot forest plots a la Danny & Pete et al. (by species, ordered by coefficient magnitude)
#   Neg at bottom, pos at top, average of all species at very bottom.
#   Can group by resprouting strategy.
# plot survival curves by species/treatment
