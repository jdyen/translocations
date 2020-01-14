# define helper functions we need for the analysis

# recode levels to remove typos or confusing health statuses
recode_levels <- function(x) {
  x <- x %>% mutate(
    Health = case_when(
      Health == "healthy" ~ "Healthy",
      Health == "Healthly" ~ "Healthy",
      Health == "H" ~ "Healthy",
      Health == "H eaten" ~ "Healthy",
      Health == "hralthy" ~ "Healthy",
      Health == "Health" ~ "Healthy",
      Health == "Ununhealthy" ~ "Unhealthy",
      Health == "unhealthy" ~ "Unhealthy",
      Health == "poor" ~ "Unhealthy",
      Health == "Poor" ~ "Unhealthy",
      Health == "Dea" ~ "Dead",
      Health == "D" ~ "Dead",
      Health == "DEAD" ~ "Dead",
      Health == "Yellow" ~ "NA",
      Health == "?" ~ "NA",
      TRUE ~ as.character(Health)
    )
  )
}

# fix up NAs coded as characters
na_is_logical <- function(x) {
  x %>% mutate(
    Health = ifelse(Health == "NA", NA, Health)
  )
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
