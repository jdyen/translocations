# need a few R packages to get everything running
library(brms)

# load some helper functions
source("R/functions.R")

# load model outputs
survival_model <- readRDS("outputs/models/survival_model.rds")
reproduction_model <- readRDS("outputs/models/reproduction_model.rds")

# some posterior predictive checks
#    Interpreted qualitatively: do the replicated values match the observations?
pp_check(survival_model, type = "dens_overlay", nsamples = 20)
pp_check(survival_model, type = "error_hist", group = "species")
pp_check(reproduction_model, type = "bars", nsamples = 20)
pp_check(reproduction_model, type = "bars_grouped", group = "species")

# get Bayesian R2 values for each model
survival_r2 <- bayes_R2(survival_model)
reproduction_r2 <- bayes_R2(reproduction_model)

# calculate fitted survival curves by species
## THIS MODEL will predict days, not survival; need enough replicates to make this work
## OR could plot differently as boxes/bars of age at death?
survival_newdata <- survival_model$data %>%
  as_tibble %>%
  select(-days, -censored) %>%
  group_by(species, source_population, site) %>%
  summarise(rainfall_deviation_mm = mean(rainfall_deviation_mm),
            rainfall_30days_prior_mm = mean(rainfall_30days_prior_mm),
            management_water = table(management_water) %>% which.max %>% names,
            management_fence = table(management_fence) %>% which.max %>% names)

survival_predictions <- posterior_predict(survival_model, newdata = survival_newdata)
survival_probabilities <- survival_predictions %>%
  apply(2, calculate_survival_probability)

## WORK ON HOW TO SHOW MULTIPLE SPECIES
## MAYBE BY GENUS?
col_palette <- viridis::viridis(198)
survival_probabilities[[1]] %$% 
  plot(probability_alive ~ breaks[-1], type = "l", lwd = 2)
for (i in seq_along(survival_probabilities)[-1])
  survival_probabilities[[i]] %$% lines(probability_alive ~ breaks[-1], lwd = 2, col = col_palette[i])

# bit of code to calculate parameter estimates summarised
# survival_model %>% spread_draws(r_species[species, var]) %>% median_qi(r_species)

