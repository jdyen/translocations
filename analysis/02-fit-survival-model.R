# need a few R packages to get everything running
library(rstan)

# we fit the model in Stan, which is a separate piece of software
#   The model is defined in a separate script: called "survival-mod.stan" in this case
mod_file <- "src/survival-model.stan"

## CURRENTLY STORES PREDICTED VALUES OF Y
##   Could drop this and save some memory?

# we now need to tell Stan to find that model script, compile it, and fit the model
#   This is a Bayesian model, so we need to tell it how many iterations, chains, and 
#   some other settings too
surv_mod <- stan(file = mod_file, data = mod_data,
                 iter = 20000, thin = 2,
                 chains = 4, cores = 4,
                 control = list(adapt_delta = 0.8))

# the Stan outputs aren't directly setup to make plots
#    We can reformat them and pull out the parameters we care about most
#    (alpha, mu, and the `beta_bg` parameters)
surv_draws <- as.matrix(surv_mod)
alpha_draws <- surv_draws[, grep("alpha$", colnames(surv_draws))]
mu_draws <- surv_draws[, grep("mu$", colnames(surv_draws))]
beta_draws <- surv_draws[, grep("beta_bg\\[", colnames(surv_draws))]