% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{autocorrelation}
\alias{autocorrelation}
\title{Estimate the Temporal Autocorrelation of a Numeric Vector}
\usage{
autocorrelation(x, biasCorrection = TRUE)
}
\arguments{
\item{x}{A numeric vector.}

\item{biasCorrection}{Autocorrelation estimates are biased for short time series. The function can
correct for this bias in the manner proposed by Quenouille (1949). Set to TRUE by default.}
}
\value{
A single numeric value: the estimate of the temporal autocorrelation with a lag of 1.
}
\description{
A wrapper for the \code{\link[stats]{acf}} function from the stats package that
extracts only the temporal autocorrelation at a lag of one timestep (which is
the type of temporal autocorrelation that this package simulates). The function omits
NA values in the time series.
}
\examples{
rednoise <- colored_noise(timesteps = 50, mean = 0.5, sd = 0.2, phi = 0.3)
autocorrelation(rednoise)
}
