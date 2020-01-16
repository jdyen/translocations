# define helper functions we need for the analysis

# how long did the individual survive?
calculate_days_survived <- function(days, alive) {

  # what is the final alive/dead state of an individual?
  final_state <- alive[which.max(days)]
  
  # is an individual alive at last sighting?
  if (final_state == 1) {
    out <- max(days)
  } else {  # if not, which is the first dead observation?
    out <- min(days[alive == 0])
  }

  # return
  out

}

# is an individual alive at last sighting?
is_censored <- function(days, alive) {
  
  # what is the final alive/dead state of an individual?
  final_state <- alive[which.max(days)]

  # is an individual alive at last sighting?
  if (final_state == 1) {
    censored <- 1
  } else {  # if not, it's not censored
    censored <- 0
  }
  
  # return
  censored
  
}

# sometimes treatments are entered with a mix of NA and true
#   treatment values within individuals. Calculate the unique
#   treatment and ditch the NA
unique_no_na <- function(x) {
  out <- unique(x)
  if (length(out) > 1)
    out <- out[!is.na(out)]
  out
}

# calculate survival at time t from fitted values Weibull parameters
#   (details in vignette(brmsfamilies) and at
#   discourse.mc-stan.org/t/estimating-survival-curves-with-weibulll-model/6475/3)
calculate_survival_probability <- function(t, mu, k) {
  lambda <- exp(mu) / gamma(1 + 1/k) 
  exp(-((t / lambda) ^ k))
}

