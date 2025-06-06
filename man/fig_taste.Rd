% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fig_taste.R
\name{fig_taste}
\alias{fig_taste}
\title{Generate a Radar Plot for Taste Properties}
\usage{
fig_taste(
  df,
  line_col = "red",
  bac_col = "skyblue",
  label.size = 5,
  line.width = 1,
  added = NULL
)
}
\arguments{
\item{df}{A data frame containing the taste data with at least two columns: 'taste' and 'weighted_frequency'.}

\item{line_col}{Character. Color for the lines connecting the data points on the radar plot. Default is 'red'.}

\item{bac_col}{Character. Background color for the radar plot. Default is 'skyblue'.}

\item{label.size}{Numeric. Size of the labels on the radar plot. Default is 5.}

\item{line.width}{Numeric. Width of the lines connecting the data points on the radar plot. Default is 1.}

\item{added}{A data frame containing the additional data to be added to the original data frame. Default is NULL.}
}
\value{
A radar plot visualizing the taste properties.
}
\description{
This function creates a radar plot to visualize the weighted frequency of various tastes in a given data frame.
}
