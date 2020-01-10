# need a few R packages to get everything running
# NEED THIS?
library(rstan)

# load some helper functions
source("R/functions.R")

# load model outputs


# setup a "days" variable so we can plot survival against the number of days survived
times <- seq(from = 0, to = 10000, length = 1000)

# set up a predictor matrix, in this case a design matrix with columns for spp
#   This tells R to plot one curve for each species
sp_set <- rbind(c(1, 0, 0, 0),
                c(1, 1, 0, 0),
                c(1, 0, 1, 0),
                c(1, 0, 0, 1))

# draw random draws from posterior and calculate survival curves
idx <- sample(seq_len(nrow(beta_draws)), size = 1000, replace = FALSE)
out <- array(NA, dim = c(length(idx), length(times), nrow(sp_set)))
for (i in seq_along(idx)) {
  for (j in seq_len(nrow(sp_set)))
    out[i, , j] <- surv_fn(times, alpha_draws[idx[i]], mu_draws[idx[i]], beta_draws[idx[i], ], sp_set[j, ])
}

# average parameters from posterior and calculate survival curves
alpha_mean <- quantile(alpha_draws, p = c(0.025, 0.1, 0.5, 0.9, 0.975))
mu_mean <- quantile(mu_draws, p = c(0.025, 0.1, 0.5, 0.9, 0.975))
beta_mean <- apply(beta_draws, 2, quantile, p = c(0.025, 0.1, 0.5, 0.9, 0.975))
out_ave <- array(NA, dim = c(length(alpha_mean), length(times), nrow(sp_set)))
for (i in seq_len(length(alpha_mean))) {
  for (j in seq_len(nrow(sp_set)))
    out_ave[i, , j] <- surv_fn(times, alpha_mean[i], mu_mean[i], beta_mean[i, ], sp_set[j, ])
}

# let's plot the average effects with their credible intervals
col_pal <- RColorBrewer::brewer.pal(4, "Dark2")
plot(out_ave[3, , 1] ~ times, type = "n",
     bty = "l", xlab = "Days", ylab = "Survival probability",
     las = 1,
     ylim = range(c(out_ave, 0, 1)))
for (i in seq_len(nrow(sp_set))) {
  polygon(c(times, rev(times)), c(out_ave[1, , i], rev(out_ave[1, , i])),
          col = scales::alpha(col_pal[i], 0.2), border = NA)
  polygon(c(times, rev(times)), c(out_ave[2, , i], rev(out_ave[4, , i])),
          col = scales::alpha(col_pal[i], 0.5), border = NA)
  lines(out_ave[3, , i] ~ times, lty = 1, lwd = 2,
        col = col_pal[i])
}

# make plots of random samples from the posterior
col_pal <- viridis::inferno(4)
plot(out[3, , 1] ~ times, type = "n",
     bty = "l", xlab = "Days", ylab = "Survival probability",
     las = 1,
     ylim = range(c(out, 0, 1)))
for (i in seq_len(nrow(sp_set))) {
  for (j in seq_len(length(idx)))
    lines(out[j, , i] ~ times, lty = 1, lwd = 0.1, col = scales::alpha(col_pal[i], 0.3))
}

sp_labels <- c("A. cochlocarpa",
               "B. ionthocarpa",
               "G. calliantha",
               "C. humile")
legend(x = 7000, y = 1.0, legend = sp_labels,
       lty = 1, col = col_pal,
       bty = "n", lwd = 2)
