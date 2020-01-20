# define helper functions we need for the analysis

# scale function to handle case when sd(x) = 0
scale_tidy <- function(x) {
  if (length(unique(x)) > 1) {
    out <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  } else {
    out <- unique(x)
  }
  out
}

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

