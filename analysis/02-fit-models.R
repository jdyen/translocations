# need a few R packages to get everything running
library(brms)

# load the pre-compiled data set
survival_data <- readRDS("data/compiled/survival-data.rds")

# we can use the brms package to fit a survival model assuming time-to-death follows
#    a Weibull distribution.
# Uses a standard R formula interface but with an additional
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


## NEED some summaries to assess bias in NAs. Number removed by species and site?

## Errors in rstan compilation on OSX Catalina:
##    solve by updating Makevars as suggested at
##    https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

## use calculate_survival_probability() function (in R/functions.R) to calculate
##   species-level and/or covariate-conditioned survival curves



# model of reproductive status:
#   logistic regression with days as a predictor:
#      asks "probability of being reproductive at time t for species s under
#            management/treatment y?"

## could add mode of response to fire as a predictor for some analyses, particularly
##   reproduction or natural recruitment. Seed bank, serotiny, resprouters as simple
##   classifications.

