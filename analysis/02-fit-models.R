# need a few R packages to get everything running
library(brms)

# load the pre-compiled data sets
survival_data <- readRDS("data/compiled/survival-data.rds")
reproduction_data <- readRDS("data/compiled/reproduction-data.rds")

# open queries:
#  - should we add response to fire (e.g. serotiny) as a predictor in
#      analyses? Particularly relevant to reproduction and natural recruitment.
#  - how much interest do we have in treatments? Many NA values in these 
#      columns (85-100% of values are NA)
#  - how much interest do we have in TFSC, or even source population, from
#      an ecological perspective. Are these nuisance variables or are there
#      genuine questions here? Missing TFSC for 39% of observations, missing
#      source population for 11%. We have KPBG number for most (98%).

# we can use the brms package to fit a survival model assuming time-to-death follows
#    a Weibull distribution.
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
survival_model <- brm(days | cens(censored) ~ 
                        (rainfall_deviation_mm +
                           rainfall_30days_prior_mm +
                           management_water +
                           management_fence | species) +
                        (1 | source_population) +
                        (1 | site),
                      data = survival_data,
                      family = weibull,
                      iter = 20000,
                      thin = 10,
                      chains = 4,
                      cores = 4)

# save the fitted model
saveRDS(survival_model, file = "outputs/models/survival_model.rds")

# model of reproductive status:
#   logistic regression with days as a predictor:
#      asks "probability of being reproductive at time t for species s under
#            management/treatment y?"
reproduction_model <- brm(reproductive ~ 
                            days +
                            (rainfall_deviation_mm +
                               rainfall_30days_prior_mm +
                               management_water +
                               management_fence | species) +
                            (1 | source_population) +
                            (1 | site) +
                            (1 | plant_no),
                          data = reproduction_data,
                          family = bernoulli,
                          iter = 20000,
                          thin = 10,
                          chains = 4,
                          cores = 4)

# save the fitted model
saveRDS(reproduction_model, file = "outputs/models/reproduction_model.rds")
