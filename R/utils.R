#' Plot variables within quintiles of another variable
#'
#' @param data Dataset to use
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @export
plot_var_and_quintile <- function(data, exposure, outcome) {
  exp_var <- data[[exposure]]
  out_var <- data[[outcome]]
  exp_var_quintile <- as.factor(cut(
    exp_var,
    quantile(exp_var, c(0, 0.2, 0.4, 0.6, 0.8, 1.0), na.rm =
               TRUE),
    include.lowest = TRUE,
    labels = FALSE
  ))
  par(mfrow = c(1, 2))
  plot(
    out_var ~ exp_var,
    main = exposure,
    xlab = exposure,
    ylab = outcome,
    xlim = c(0, quantile(exp_var, c(0.99), na.rm = TRUE))
  )
  
  boxplot(
    out_var ~ exp_var_quintile,
    main = paste(exposure, '-quintile', sep = ""),
    xlab = exposure,
    ylab = outcome
  )
}

#' Plot behaviour profiles over an average day
#'
#' @param data Dataset to use
#' @param exposure_prefix Name of behaviour variable to plot
#' @param exposure_suffix Suffix of behaviour variable columns in dataset
#' @param y_label Label for y-axis
#' @return Plot of behaviour variable over hours of day
#' @export
plotAverageDay <-
  function(data,
           exposure_prefix,
           exposure_suffix,
           y_label = exposure_prefix) {
    for (name in c("hr_PAcols",
                   "mean_PAcols",
                   "se_PAcols",
                   "low_PAcols",
                   "high_PAcols")) {
      assign(name, rep(NA, times = 24))
    }
    hrs <- rep(NA, times = 24)
    for (hr in 0:23) {
      # Admin
      hrs[hr + 1] <- hr
      hr_PACols[hr + 1] <-
        paste0(exposurePrefix, hr, exposureSuffix)
      data_col_full <- data[, hrPACols[hr + 1]]
      data_col <- data_col_full[!is.na(data_col_full)]
      n <- length(data_col)
      
      # Means and SEs
      mean_PACols[hr + 1] <- mean(data_col)
      se_PACols[hr + 1] <- sqrt(var(data_col))/sqrt(n)
      low_PACols <- mean_PACols[hr + 1] - 1.96 * se_PACols[hr + 1]
      high_PACols <- mean_PACols[hr + 1] + 1.96 * se_PACols[hr + 1]
    
      }
    
    # Draw plot
    plot_data <- data.frame(cbind(hrs, mean_PACols, low_PACols, high_PACols))
    plot <-
      ggplot2::ggplot(data = plot_data,
                      aes(x = hrs, y = mean_PACols)) +
      geom_line() +
      geom_ribbon(aes(x = hrs, ymin = low_PACols,
                      ymax = high_PACols), colour = "grey") +
      
      labs(title = "Time-of-day behaviour profile",
           y = yAxisLabel,
           x = "Hour of Day")
    
    return(plot)
    
  }
