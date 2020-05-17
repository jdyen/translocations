# functions to fit BRMS models to pre-loaded data

# BRMS notes:
# There are often errors in rstan compilation on OSX Catalina:
#    Solution is to update Makevars, instructions at:
#    <https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/>
# The `brm` function uses a standard R formula interface but with an additional
#    term to identify censored observations. This is handled on
#    the LHS of the formula, separated from days survived by a `|`.
#    The values of `censored` must take one of "left", "none", "right",
#    "interval" or, equivalently, -1 (left), 0 (none), 1 (right), 2 (interval).
#    If interval-censored, then an additional value is needed in `cens()`
#    to define the upper bound of the interval.
# Default behaviour is to remove NA rows. Alternatives are to impute
#    or to model these NA values. Imputation has issues, modelling is
#    computationally challenging.

# we can use the brms package to fit a survival model assuming time-to-death
#   follows a Weibull distribution.
fit_survival_model <- function(formula, data, mcmc_settings) {
  
  # fit brms model
  model <- brm(formula,
               data = data,
               family = weibull,
               iter = mcmc_settings$n_iter,
               thin = mcmc_settings$n_thin,
               chains = mcmc_settings$n_chain,
               future = TRUE)
  
  # return
  model
  
}

# model of reproductive status:
#   logistic regression with days as a predictor:
#      asks "probability of being reproductive at time t for species s under
#            management/treatment y?"
fit_reproduction_model <- function(formula, data, mcmc_settings) {
  
  # filter to things that survived to the first survey
  data_filtered <- data %>% filter(days > 0)
  
  # fit the brms model
  model <- brm(formula,
               data = data_filtered,
               family = bernoulli,
               iter = mcmc_settings$n_iter,
               thin = mcmc_settings$n_thin,
               chains = mcmc_settings$n_chain,
               future = TRUE)
  
  # return
  model
  
}

# model of individual growth:
#   log-linear regression with days as a predictor:
#      asks "how big is an individual of species s at time t under
#            management/treatment y?"
fit_growth_model <- function(formula, data, mcmc_settings) {
  
  # filter data to individuals that survived to the first survey
  data_filtered <- data %>% filter(days > 0)
  
  # fit brms model
  model <- brm(formula,
               data = data_filtered,
               family = lognormal,
               iter = mcmc_settings$n_iter,
               thin = mcmc_settings$n_thin,
               chains = mcmc_settings$n_chain,
               future = TRUE)
  
  # return
  model
  
}

