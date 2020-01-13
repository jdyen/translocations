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

# function to calculate survival curve from fitted parameters
surv_fn <- function(t, alpha, mu, beta, x) {
  sigma_i <- c(exp(-1 * (mu + beta %*% x) / alpha))
  exp(- (t / sigma_i)^alpha)
}
