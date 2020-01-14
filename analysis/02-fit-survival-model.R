# need a few R packages to get everything running
library(brms)

# load the pre-compiled data set
survival_data <- readRDS("data/compiled/survival-data.rds")

# we can use the brms package to fit a survival model

### cens(censored) where censored is "left", "none", "right", "interval" or
###    -1, 0, 1, 2.
survival_model <- brm(days | cens(censored) ~ (1 | species) + (1 | site),
                      data = survival_data,
                      family = lognormal())

# the Stan outputs aren't directly setup to make plots
#    We can reformat them and pull out the parameters we care about most
#    (alpha, mu, and the `beta_bg` parameters)
surv_draws <- as.matrix(surv_mod)
alpha_draws <- surv_draws[, grep("alpha$", colnames(surv_draws))]
mu_draws <- surv_draws[, grep("mu$", colnames(surv_draws))]
beta_draws <- surv_draws[, grep("beta_bg\\[", colnames(surv_draws))]