# main script to run the full analysis. Uses the drake R
# package to cache intermediate objects and save some runtime

# set a seed to keep the MCMC sampler reproducible
set.seed(2020-04-28)

# packages used for workflow management
library(drake)

# to load and prepare data objects
library(qs)
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)

# to fit models
library(brms)

# to interrogate model outputs
library(magrittr)
library(tidybayes)

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
  old_rainfall_data = load_rainfall_data(
    file_in("data/raw/Translocation site longterm rainfall data.xlsx")
  ),
  new_rainfall_data = load_rainfall_data(
    file_in("data/raw/Translocation site longterm rainfall data_2 missing datasets.xlsx")
  ),
  rainfall_data = bind_rows(old_rainfall_data, new_rainfall_data),
  
  # tidy these loaded data sets
  translocation_tidy = tidy_translocation_data(translocation_data),
  
  # correct known errors in the data based on a table of corrections
  translocation_no_errors = correct_errors(
    translocation_tidy,
    file_in("data/raw/corrected-growth-data.xlsx")
  ),
  
  # combine management and treatment water into a single variable
  translocation_corrected = translocation_no_errors %>%
    mutate(
      management_water = ifelse(is.na(management_water), "No", management_water),
      treatment_water = ifelse(is.na(treatment_water), "Control", treatment_water),
      management_water = ifelse(
        management_water == "Yes" | treatment_water == "Water",
        "Yes", 
        "No"),
      management_fence = ifelse(is.na(management_fence), "No", management_fence),
      treatment_fence = ifelse(is.na(treatment_fence), "Unfenced", treatment_fence),
      management_fence = ifelse(
        management_fence == "Yes" | treatment_fence == "Fenced",
        "Yes", 
        "No")
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
    file_out("data/compiled/survival-data.qs")
  ),
  reproduction = calculate_reproduction(
    translocation_corrected,
    rainfall_tidy,
    file_out("data/compiled/reproduction-data.qs")
  ),
  growth = calculate_growth(
    translocation_corrected,
    rainfall_tidy,
    file_out("data/compiled/growth-data.qs")
  ),

  # make some summary plots to check data
  crown_plots = plot_crown_trajectories(
    file_in("data/compiled/growth-data.qs"),
    file_out("outputs/figures/growth-variation-crown.pdf")
  ),
  height_plots = plot_height_trajectories(
    file_in("data/compiled/growth-data.qs"),
    file_out("outputs/figures/growth-variation-height.pdf")
  )
  
)

make(load_and_tidy_data)

# fit models ----------------------------------------------------------------

source("R/modelling.R")

fit_models <- drake_plan(
  
  # load the data sets
  survival_data = qread(file_in("data/compiled/survival-data.qs")),
  reproduction_data = qread(file_in("data/compiled/reproduction-data.qs")),
  growth_tmp = qread(file_in("data/compiled/growth-data.qs")),

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
  
  # save fitted models (not really necessary with drake::readd)
  qsave(survival_model, file = file_out("outputs/models/survival-model.qs")),
  qsave(reproduction_model, file = file_out("outputs/models/reproduction-model.qs")),
  qsave(height_growth_model, file = file_out("outputs/models/height-growth-model.qs")),
  qsave(crown_growth_model, file = file_out("outputs/models/crown-growth-model.qs")),
  
)

# rstan models require lock_envir = FALSE to avoid issues
#   with drake
make(fit_models, lock_envir = FALSE)

# interrogate and plot models -------------------------------------------------------------

source("R/plotting.R")

plot_models <- drake_plan(

  # load the fitted models
  survival_model = readd(survival_model),
  reproduction_model = readd(reproduction_model),
  height_growth_model = readd(height_growth_model),
  crown_growth_model = readd(crown_growth_model),
  
  # load rainfall zones
  species_rainfall_zones = load_rainfall_zones(
    file_in("data/raw/Species_rainfall zones.xlsx")
  ),

  # run some posterior predictive checks and save outputs to files
  survival_checks = posterior_checks(
    survival_model,
    type = c("dens_overlay", "error_hist"),
    file = c(file_out("outputs/figures/survival_pp_checks_dens.png"),
             file_out("outputs/figures/survival_pp_checks_hist.png")),
    nsamples = 20
  ),
  reproduction_checks = posterior_checks(
    reproduction_model,
    type = c("bars", "bars_grouped"),
    file = c(file_out("outputs/figures/reproduction_pp_checks_bars.png"),
             file_out("outputs/figures/reproduction_pp_checks_bars_grouped.png")),
    nsamples = 20,
    group = "species"
  ),
  height_growth_checks = posterior_checks(
    height_growth_model,
    type = c("dens_overlay", "error_hist"),
    file = c(file_out("outputs/figures/height_growth_pp_checks_dens.png"),
             file_out("outputs/figures/height_growth_pp_checks_hist.png")),
    nsamples = 20
  ),
  crown_growth_checks = posterior_checks(
    crown_growth_model,
    type = c("dens_overlay", "error_hist"),
    file = c(file_out("outputs/figures/crown_growth_pp_checks_dens.png"),
             file_out("outputs/figures/crown_growth_pp_checks_hist.png")),
    nsamples = 20
  ),
  
  # calculate model r2 valuesf
  survival_R2 = bayes_R2(survival_model),
  reproduction_R2 = bayes_R2(reproduction_model),
  height_growth_R2 = bayes_R2(height_growth_model),
  crown_growth_R2 = bayes_R2(crown_growth_model),
  
  # plot effects of variables
  var_list = c("Intercept",
               "rainfall_deviation_std",
               "rainfall_30days_prior_std",
               "management_waterYes",
               "management_fenceYes"),
  surv_files = sapply(paste0("outputs/figures/survival-", var_list, ".png"),
                      file_out),
  reprod_files = sapply(paste0("outputs/figures/reproduction-", var_list, ".png"),
                        file_out),
  hgrow_files = sapply(paste0("outputs/figures/height-growth-", var_list, ".png"),
                       file_out),
  cgrow_files = sapply(paste0("outputs/figures/crown-growth-", var_list, ".png"),
                       file_out),
  surv_coef_plots = plot_model(
    survival_model, 
    var_list, 
    surv_files, 
    rainfall_zone = species_rainfall_zones,
    order = TRUE, 
    xlog = FALSE,
    group = TRUE
  ),
  reprod_coef_plots = plot_model(
    reproduction_model,
    var_list, 
    reprod_files, 
    rainfall_zone = species_rainfall_zones,
    order = TRUE, 
    xlog = TRUE,
    group = TRUE
  ),
  hgrow_coef_plots = plot_model(
    height_growth_model, 
    var_list, 
    hgrow_files, 
    rainfall_zone = species_rainfall_zones,
    order = TRUE, 
    xlog = TRUE,
    group = TRUE
  ),
  cgrow_coef_plots = plot_model(
    crown_growth_model,
    var_list, 
    cgrow_files, 
    rainfall_zone = species_rainfall_zones,
    order = TRUE, 
    xlog = TRUE,
    group = TRUE
  ),

  # recreate plots but by variable rather than response
  model_list = list(
    survival_model,
    reproduction_model,
    height_growth_model,
    crown_growth_model
  ),
  rainfall_deviation_plot = plot_variable(
    model_list,
    var = "rainfall_deviation_std",
    file = "outputs/figures/rainfall_deviation-effects.png",
    rainfall_zone = species_rainfall_zones, 
    order = TRUE, 
    xlog = c(FALSE, rep(TRUE, 3))
  ),
  rainfall_prior_plot = plot_variable(
    model_list,
    var = "rainfall_30days_prior_std",
    file = "outputs/figures/rainfall_prior-effects.png",
    rainfall_zone = species_rainfall_zones, 
    order = TRUE, 
    xlog = c(FALSE, rep(TRUE, 3))
  ),
  management_water_plot = plot_variable(
    model_list,
    var = "management_waterYes",
    file = "outputs/figures/management_water-effects.png",
    rainfall_zone = species_rainfall_zones, 
    order = TRUE, 
    xlog = c(FALSE, rep(TRUE, 3))
  ),
  management_fence_plot = plot_variable(
    model_list,
    var = "management_fenceYes",
    file = "outputs/figures/management_fence-effects.png",
    rainfall_zone = species_rainfall_zones, 
    order = TRUE, 
    xlog = c(FALSE, rep(TRUE, 3))
  ),
  
  # plot histograms of rainfall deviation against watering treatment
  survival_data = qread(file_in("data/compiled/survival-data.qs")),
  check_watering_balance(survival_data),
  
)

make(plot_models)
