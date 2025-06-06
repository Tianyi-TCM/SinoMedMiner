% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findSimRxs.R
\name{findSimRxs}
\alias{findSimRxs}
\title{Find Similar Prescriptions}
\usage{
findSimRxs(rxs, threshold = 0.3)
}
\arguments{
\item{rxs}{A vector of strings, such as: fangji <- c("Mahuang, Guizhi, Shaoyao, Gancao"), where each string represents a prescription and, following convention, Chinese herbs are separated by Chinese commas.}

\item{threshold}{A numeric value to set the similarity threshold, default is 0.3.}
}
\value{
Returns a string that represents the IDs and compositions of similar prescriptions, for example: "1：Mahuang，Guizhi，Shaoyao，Gancao".
}
\description{
By comparing with classic famous formulas, find similar prescriptions in classic formulas that resemble the input prescription.
}
\examples{
#
}
