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
#    to define the interval.
survival_model <- brm(days | cens(censored) ~ (1 | species) + (1 | site),
                      data = survival_data,
                      family = weibull)
## Predictors
## need source pops
## rainfall might be missing second year for some recent plantings

## Errors in rstan compilation on OSX Catalina:
##    solve by updating Makevars as suggested at
##    https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

## use calculate_survival_probability() function (in R/functions.R) to calculate
##   species-level and/or covariate-conditioned survival curves
