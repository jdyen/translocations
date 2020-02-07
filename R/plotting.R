# functions to summarise fitted brms models

# calculate probability of survival to time t or older from 
#   fitted brms survival model (simulated ages at death)
calculate_survival_probability <- function(ages, breaks = NULL) {
  
  # 
  if (is.null(breaks))
    breaks <- seq(from = min(ages), to = max(ages), length = 100)
  
  # 
  survival_categories <- hist(ages, breaks = breaks, plot = FALSE)
  
  # 
  bin_width <- diff(survival_categories$breaks)
  survival_probability <- bin_width * survival_categories$density
  
  #
  probability_alive <- 1 - cumsum(survival_probability)
  
  # return outputs
  list(survival_probability = survival_probability,
       probability_alive = probability_alive,
       breaks = breaks)
  
}

# some posterior predictive checks
#    Interpreted qualitatively: do the replicated values match the observations?
posterior_checks <- function(model, type, file, ...) {
  pdf(file = file)
  par(mfrow = c(length(type), 1))
  for (i in seq_along(type)) 
    pp_check(model, type = type[i], ...)
  dev.off()
}

# generate samples for the number of days survived for each species, assuming
#   average rainfall and modal management strategies
extract_survival_curves <- function(model) {
  
  # predict days survived at average conditions
  survival_predictions <- model$data %>%
    as_tibble %>%
    select(-days, -censored) %>%
    group_by(species, source_population, site) %>%
    summarise(rainfall_deviation_std = mean(rainfall_deviation_std),
              rainfall_30days_prior_std = mean(rainfall_30days_prior_std),
              management_water = table(management_water) %>% which.max %>% names,
              management_fence = table(management_fence) %>% which.max %>% names) %>%
    ungroup %>%
    posterior_predict(object = model)
  
  # turn these "days survived" values into survival probabilities
  survival_probabilities <- survival_predictions %>%
    apply(2, calculate_survival_probability)
  
  # return
  ## TO bE UPDATED TO PLOT AS NEEDED
  survival_probabilities
  
}

# plot survival curves by genus
#   - show average for genus, average for species, and species-by-source-and-site
# col_palette <- viridis::viridis(198)
# group_ids <- survival_model$data %>%
#   as_tibble %>%
#   select(-days, -censored) %>%
#   group_by(species, source_population, site) %>%
#   group_keys
# group_ids <- group_ids %>% mutate(
#   genus = species %>% strsplit(" ") %>% sapply(function(x) x[1])
# )
#   
## OPTIONS:  - flatten the survival probabilities to a long-form data set
##           - just work with the list (won't work with ggplot)

# survival_probabilities[[1]] %$% 
#   plot(probability_alive ~ breaks[-1], type = "l", lwd = 2)
# for (i in seq_along(survival_probabilities)[-1])
#   survival_probabilities[[i]] %$% lines(probability_alive ~ breaks[-1], lwd = 2, col = col_palette[i])

# bit of code to calculate parameter estimates summarised
# survival_model %>% spread_draws(r_species[species, var]) %>% median_qi(r_species)
