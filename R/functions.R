# define helper functions we need for the analysis

# function to calculate survival curve from fitted parameters
surv_fn <- function(t, alpha, mu, beta, x) {
  sigma_i <- c(exp(-1 * (mu + beta %*% x) / alpha))
  exp(- (t / sigma_i)^alpha)
}
