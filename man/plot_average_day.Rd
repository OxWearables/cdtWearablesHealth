% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plot_average_day}
\alias{plot_average_day}
\title{Plot behaviour profiles over an average day}
\usage{
plot_average_day(
  data,
  exposure_prefix,
  exposure_suffix,
  y_label = exposure_prefix
)
}
\arguments{
\item{data}{Dataset to use}

\item{exposure_prefix}{Name of behaviour variable to plot}

\item{exposure_suffix}{Suffix of behaviour variable columns in dataset}

\item{y_label}{Label for y-axis}
}
\value{
Plot of behaviour variable over hours of day
}
\description{
Plot behaviour profiles over an average day
}
