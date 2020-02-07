# main script to run the full analysis. Uses the drake R
# package to cache intermediate objects and save some runtime

# set a seed to keep the MCMC sampler reproducible
set.seed(2020-02-07)

# packages used for workflow management
library(drake)
library(future)

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
  file_list = get_file_list(directory = file_in("data/raw"), ignore = "rainfall"),
  sites = create_site_list(file_list),
  species = create_species_list(file_list),
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

  # and calculate some summary stats on the treatment sample sizes
  calculate_treatment_sizes(
    translocation_data,
    file_out("data/compiled/treatment-list.csv")
  ),
  
  # calculate the primary response variables
  rainfall_tidy = tidy_rainfall_data(rainfall_data),
  survival = calculate_survival(
    translocation_tidy, 
    rainfall_tidy, 
    file_out("data/compiled/survival-data.RDS")
  ),
  reproduction = calculate_reproduction(
    translocation_tidy,
    rainfall_tidy,
    file_out("data/compiled/reproduction-data.RDS")
  ),
  growth = calculate_growth(
    translocation_tidy,
    rainfall_tidy,
    file_out("data/compiled/growth-data.RDS")
  ),
  # recruitment = calculate_recruitment(
  ## NOT YET IMPLEMENTED
  #)
  
  # make some summary plots to check data
  growth_plots = plot_growth_trajectories(
    file_in("data/compiled/growth-data.RDS"),
    file_out("outputs/figs/growth-variation.pdf")
  ),
  
)

make(load_and_tidy_data)

# fit models ----------------------------------------------------------------

source("R/modelling.R")

fit_models <- drake_plan(
  
  # load the data sets
  survival_data = readRDS("data/compiled/survival-data.RDS"),
  reproduction_data = readRDS("data/compiled/reproduction-data.RDS"),
  growth_data = readRDS("data/compiled/growth-data.RDS"),
  
  # fit models
  ## FOR ALL:
  #    decide on model structure, shift some predictors to main effects,
  #    make sure there are overall effects in the model, not just random intercepts/coefs.
  
  # define MCMC settings
  mcmc_settings = list(n_iter = 10000, n_thin = 5, n_chain = 4),
    
  # fit survival model
  survival_formula = days | cens(censored) ~  propagule_type +
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
  
  # fit growth model
  growth_formula = mean_crown ~ 
    days +
    propagule_type +
    (rainfall_deviation_std +
       rainfall_30days_prior_std +
       management_water +
       management_fence | species) +
    (1 | source_population) +
    (1 | site) +
    (1 | plant_no),
  growth_model = fit_growth_model(
    growth_formula,
    growth_data,
    mcmc_settings
  ),
  
  ## NOT YET IMPLEMENTED
  # treatment_models = fit_treatment_model(
  #   formula, treatment_water, data, mcmc_settings
  # )
  
)

# set the computational future (and reset future once models have run)
old_future <- plan()
plan(multisession)
make(fit_models)
plan(old_future)

# interrogate models -------------------------------------------------------------

source("R/plotting.R")

check_models <- drake_plan(

  # load the fitted models (could use RDS if this doesn't work??)
  survival_draws <- loadd(survival_model),
  reproduction_draws <- loadd(reproduction_model),
  growth_draws <- loadd(growth_model),
  
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
  growth_checks = posterior_checks(
    growth_model,
    type = c("dens_overlay", "error_hist"),
    file = file_out("outputs/figures/growth_pp_checks.pdf"),
    nsamples = 20
  ),
  
  # calculate model r2 values
  survival_R2 = bayes_R2(survival_model),
  reproduction_R2 = bayes_R2(reproduction_model),
  growth_R2 = bayes_R2(growth_model),
  
  
)
