# functions to summarise fitted brms models

# some posterior predictive checks
#    Interpreted qualitatively: do the replicated values match the observations?
posterior_checks <- function(model, type, file, ...) {

  for (i in seq_along(type)) {
    jpeg(file = file[i], res = 300, height = 4000, width = 4000, pointsize = 4)
    print(pp_check(model, type = type[i], ...))
    dev.off()
  }
  
}

# alternative way to plot survival: calculate Weibull curve for parameters
weibull_curve <- function(model, age, newdata, re_formula = NULL, probs = c(0.025, 0.1, 0.5, 0.9, 0.975)) {
  
  # set a default random effects formula
  if (is.null(re_formula)) {
    re_formula <-  ~ (rainfall_deviation_std + 
                        rainfall_30days_prior_std + 
                        management_water + 
                        management_fence | species)
  }
  
  # generate estimates of the transformed linear predictor
  linpred <- pp_expect(
    model,
    newdata = newdata,
    re_formula = re_formula
  )
  
  # pull out the shape parameter
  shape <- as.matrix(model)[, "shape"]
  
  # reconstruct scale parameter from linear predictor
  sigma_i <- sweep(linpred, 1, gamma(1 + 1 / shape), "/")
  age_sigma <- outer(age, sigma_i, FUN = "/")
  
  # component 1
  exponent <- exp( -1 * sweep(age_sigma, c(2, 3), shape, "^"))
  
  # component 2
  multiplier <- sweep(age_sigma, c(2, 3), shape - 1, "^")
  
  # and component 3
  main_factor <- sweep(1 / sigma_i, 1, shape, "*")
  
  # combine it all together
  out <- sweep(multiplier * exponent, c(2, 3), main_factor, "*")
  
  # rescale to account for discrete density estimates
  bin_width <- diff(age)
  bin_width <- c(bin_width[1], bin_width)
  out <- sweep(out, 1, bin_width, "*")
  
  # calculate cumulative probability of being alive
  out <- apply(out, c(2, 3), cumsum)
  
  # and return quantiles
  apply(out, c(1, 3), quantile, probs = probs)
  
}

# plot weibull curves with uncertainty
plot_weibull <- function(model,
                         species,
                         age = NULL,
                         rainfall_dev = 0,
                         rainfall_30 = 0,
                         manage_water = "Yes",
                         manage_fence = "Yes",
                         propagule = "Seedling",
                         main = NULL,
                         legend = NULL,
                         col = NULL,
                         cumulative = TRUE) {
  
  # change margins, saving old margins
  old_mar <- par()$mar
  par(mar = c(4.6, 4.5, 2.1, 1.1))
  
  # create new data to plot
  newdata <- expand.grid(
    rainfall_deviation_std = rainfall_dev,
    rainfall_30days_prior_std = rainfall_30,
    management_water = manage_water,
    management_fence = manage_fence,
    species = species,
    propagule_type = propagule
  )
  
  # create a sequence of age values if not provided
  if (is.null(age))
    age <- seq(1, 11000, length = 2000)
  
  # calculate weibull density for each age and row of newdata
  dens <- 1 - weibull_curve(model, age, newdata)

  # work out plot limits
  nplot <- dim(dens)[3]
  ylim <- range(dens)
  
  # define a colour palette if required
  if (is.null(col))
    col <- viridis::inferno(5 * nplot)[round(seq(nplot, 4 * nplot, length = nplot))]
  
  # create an empty plot
  plot(dens[3, , 1] ~ age,
       type = "n",
       bty = "l",
       las = 1,
       ylim = ylim,
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n")
  
  # add curves and shaded regions for intervals
  for (i in seq_len(nplot)) {
    polygon(c(age, rev(age)), c(dens[5, , i], rev(dens[1, , i])),
            col = scales::alpha(col[i], 0.25), border = NA)
  }
  for (i in seq_len(nplot))
    lines(dens[3, , i] ~ age, col = col[i], lwd = 2)
  
  # add axes
  axis(1)
  axis(2, las = 1)
  
  # add axis labels
  mtext("Age (days)", side = 1, line = 2.6, adj = 0.5)
  mtext("Probability alive", side = 2, line = 3, adj = 0.5)
  
  # add main label if required
  if (!is.null(main))
    mtext(main, side = 3, line = 0.1, adj = 1)
  
  # add legend if required
  if (!is.null(legend))
    legend(x = 17000, y = 0.8, lwd = 2, legend = legend, col = col)
  
  # reset margins
  par(mar = old_mar)
  
}

# function to compare survival curves among treatments
plot_survival_treatments <- function(model, filepath) {
  
  # list all species
  sp_list <- unique(model$data$species)
  
  # create a file to plot to
  jpeg(file = paste0(filepath, "-watering.jpg"),
       height = 6000, width = 4000, units = "px",
       res = 300, pointsize = 10)
  
  # reset plot parameters, saving old pars
  par(mfrow = c(10, 5))
  
  # plot all species
  for (i in seq_along(sp_list)) {
    plot_weibull(model,
                 species = sp_list[i],
                 manage_water = c("Yes", "No"),
                 main = sp_list[i])
  }
  
  # close plot device
  dev.off()
  
  # repeating for fencing
  jpeg(file = paste0(filepath, "-fencing.jpg"),
       height = 6000, width = 4000, units = "px",
       res = 300, pointsize = 10)
  
  # reset plot parameters, saving old pars
  par(mfrow = c(10, 5))
  
  # plot all species
  for (i in seq_along(sp_list)) {
    plot_weibull(model,
                 species = sp_list[i],
                 manage_fence = c("Yes", "No"),
                 main = sp_list[i])
  }
  
  # close plot device
  dev.off()
  
}

# extract coefficients from fitted brms model
extract_coefficients <- function(model, variable, .width = c(0.8, 0.95)) {
  
  # create variable name to pull out main effect
  main_effect <- paste0("b_", variable)
  
  # pull out draws in wide format
  vals <- model %>%
    spread_draws(!!sym(main_effect), r_species[species, term]) %>%
    filter(term == variable) %>%
    mutate(coef = r_species + !!sym(main_effect)) %>%
    median_qi(.width = .width)
  
  vals <- vals %>% 
    select(species,
           term,
           coef,
           coef.lower,
           coef.upper,
           .width,
           .point,
           .interval)
  
  vals <- vals %>%
    pivot_wider(
      names_from = .width,
      values_from = c(coef.lower, coef.upper)
    )
  
  # return
  vals
  
}

# plot coefficients from a fitted brms model
plot_model <- function(model, var_list, file_list, order = FALSE, xlog = FALSE) {
  
  # loop through each variable of interest and create a forest plot of coefficients
  for (i in seq_along(var_list)) {
    
    # open a plotting device
    jpeg(file = file_list[i], width = 1600, height = 1600, pointsize = 8, res = 300)
    
    # get coefficients
    vals <- extract_coefficients(model, var_list[i])
    
    # get the species and variable names correct
    var_name <- format_name(var_list[i])
    sp_names <- vals$species
    sp_names <- gsub("\\.", " ", sp_names)
    
    # do we want to exponentiate the x-axis? (yes if intercepts in some models)
    log_x <- FALSE
    if (i == 1 & xlog)
      log_x <- TRUE
    
    # calculate mean line (i.e. shared mean for all species)
    mean_line <- fixef(model)[var_list[i], "Estimate"]
    
    # plot it
    plot_coefficients(vals$coef, vals$coef.lower_0.8, vals$coef.upper_0.8, vals$coef.lower_0.95, vals$coef.upper_0.95,
                      labels = sp_names, main = var_name, order = order, xlog = log_x, mean_line = mean_line)
    
    # close the plotting device
    dev.off()
    
  }
  
}

# internal function used in plot_model
plot_coefficients <- function(midpoint,
                              narrow_lower,
                              narrow_upper,
                              wide_lower = NULL,
                              wide_upper = NULL,
                              labels = NULL, 
                              xlab = "Parameter estimate",
                              main = NULL,
                              order = FALSE,
                              xlog = FALSE,
                              mean_line = NULL) {
  
  # reset margins, saving old margins
  old_mar <- par()$mar
  par(mar = c(5.2, 11.2, 2.1, 1.1))
  
  # what if labels aren't provided?
  if (is.null(labels))
    labels <- paste0("Group ", seq_len(length(midpoint)))
  
  # reorder if needed
  if (order) {
    idx <- order(midpoint)
    midpoint <- midpoint[idx]
    narrow_lower <- narrow_lower[idx]
    narrow_upper <- narrow_upper[idx]
    if (!is.null(wide_lower) & !is.null(wide_upper)) {
      wide_lower <- wide_lower[idx]
      wide_upper <- wide_upper[idx]
    }
    labels <- labels[idx]
  }
  
  # do we need to exponentiate everything?
  if (xlog) {
    midpoint <- exp(midpoint)
    narrow_lower <- exp(narrow_lower)
    narrow_upper <- exp(narrow_upper)
    if (!is.null(wide_lower) & !is.null(wide_upper)) {
      wide_lower <- exp(wide_lower)
      wide_upper <- exp(wide_upper)
    }
  }
  
  # generate plot dimensions
  nvar <- length(midpoint)
  ylim <- range(c(0.5, nvar + 0.5))
  xlim <- range(c(midpoint, narrow_lower, narrow_upper, wide_lower, wide_upper))
  xsub <- seq_len(nvar)
  
  # plot the midpoints
  plot(midpoint, xsub,
       xlim = xlim, ylim = ylim,
       pch = 16,
       cex = 1.25,
       bty = "l",
       las = 1,
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n")
  
  # and add the credible intervals
  for (i in xsub) {
    lines(c(narrow_lower[i], narrow_upper[i]), c(i, i), lty = 1, lwd = 3.5)
    if (!is.null(wide_lower) & !is.null(wide_upper))
      lines(c(wide_lower[i], wide_upper[i]), c(i, i), lty = 1, lwd = 1.5)
  }
  
  # plus a zero line
  lines(c(0, 0), c(0, nvar + 1), lty = 2, lwd = 1.5)
  
  # and a mean line
  if (!is.null(mean_line))
    lines(c(mean_line, mean_line), c(0, nvar + 1), lty = 2, lwd = 2, col = "gray50")
  
  # and some axes  
  axis(1)
  axis(2, at = xsub, labels = labels, las = 1, cex.axis = 0.75)
  
  # and x-axis label
  mtext(xlab, side = 1, line = 2.8, adj = 0.5, cex = 1)
  
  # add a main label if required
  if (!is.null(main))
    mtext(main, side = 3, line = 0, adj = 1, cex = 1.1)
  
  # reset margins
  par(mar = old_mar)
  
}

# internal function used in plot_model
format_name <- function(x) {
  
  x <- gsub("_", " ", x)
  x <- gsub("std", "", x)
  x <- gsub("Yes", "", x)
  
  x <- paste0(toupper(substr(x, 1, 1)),
              tolower(substr(x, 2, nchar(x))))
  
}


# plot variance components
#  - use something like:
#      VarCorr(model)$species$sd
#      VarCorr(model)$source_population$sd
#      VarCorr(model)$site$sd
