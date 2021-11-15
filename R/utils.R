#' Plot variables within quintiles of another variable
#'
#' @param dataInput Dataset to use
#' @param exposure Name of exposure variable
#' @param outcome Name of outcome variable
#' @param writePDF Logical parameter. If TRUE, writes pdf file with output.
#' @export
plotVarAndQuintile <- function(dataInput, exposure, outcome, writePDF = FALSE){
  #cat('\n\n\n =========== \n', outcome, ' AND ', exposure, '(confounders = ', confounders, ')\n =========== \n')
  expVar <- dataInput[[exposure]]
  outVar <- dataInput[[outcome]]
  filename <- paste(outcome, '-', exposure, '.pdf', sep="")
  expVarQuintile <- as.factor(cut(expVar,
                                  quantile(expVar, c(0,0.2,0.4,0.6,0.8,1.0), na.rm=TRUE),
                                  include.lowest=TRUE,
                                  labels=FALSE)
  )
  if (writePDF) {
    pdf(filename, height=8, width=16)
  }
  par(mfrow=c(1,2))
  plot(outVar ~ expVar,
       main=exposure, xlab=exposure, ylab=outcome, xlim=c(0,quantile(expVar, c(0.99), na.rm=TRUE)))

  boxplot(outVar ~ expVarQuintile,# data=tmp,
          main=paste(exposure,'-quintile',sep=""), xlab=exposure, ylab=outcome)
  if (writePDF) {
    dev.off()
    cat('Plots written to: ', filename)
  }
}

#' Plot behaviour profiles over an average day
#'
#' @param data Dataset to use
#' @param exposurePrefix Name of behaviour variable to plot
#' @param exposureSuffix Suffix of behaviour variable columns in dataset
#' @param yAxisLabel Label for y-axis
#' @param outPng Optional filename to save plot
#' @return Plot of behaviour variable over hours of day
#' @export
plotAverageDay <- function(data, exposurePrefix, exposureSuffix, yAxisLabel = exposurePrefix, outPng = NULL){

  hrPACols <- c()
  mean_PACols <- c()
  se_PACols <- c()
  low_PACols <- c()
  high_PACols <- c()
  hrs <- c()
  for (hr in 0:23){
    hrs <- c(hrs, as.numeric(hr))
    hrPACols <- c(hrPACols , paste(exposurePrefix, hr, exposureSuffix, sep = ""))
    mean_PACols <- c(mean_PACols, as.numeric(mean(data[,hrPACols[hr+1]], na.rm = TRUE)))
    se_PACols <- c(se_PACols, as.numeric(sqrt(var(data[, hrPACols[hr+1]], na.rm = TRUE))/sqrt(nrow(data))))
    low_PACols <- c(low_PACols, mean_PACols[hr+1] - 1.96*se_PACols[hr+1])
    high_PACols <- c(high_PACols, mean_PACols[hr+1] + 1.96*se_PACols[hr+1])
  }
  plot <- ggplot2::ggplot(data = data.frame(cbind(hrs, mean_PACols, low_PACols, high_PACols)), aes(x = hrs, y = mean_PACols))+
    geom_ribbon(aes(x = hrs, ymin = low_PACols,
                    ymax = high_PACols), colour = "grey")+
    geom_line()+
    labs(title = "Time-of-day behaviour profile",
         y = yAxisLabel,
         x = "Hour of Day")

  if (!(is.null(outPng))){
    ggplot2::ggsave(outPng, plot = plot, device = png())
  }

  return(plot)

}
