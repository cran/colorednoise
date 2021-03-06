% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{colored_noise}
\alias{colored_noise}
\title{Generate Autocorrelated Noise}
\usage{
colored_noise(timesteps, mean, sd, phi)
}
\arguments{
\item{timesteps}{The number of temporally autocorrelated random numbers (one
per timestep) you want.}

\item{mean}{The mean of the temporally autocorrelated random numbers.}

\item{sd}{The standard deviation of the temporally autocorrelated random
numbers.}

\item{phi}{The temporal autocorrelation. 0 is white noise (uncorrelated),
positive values are red noise (directly correlated) and negative values are blue
noise (inversely correlated).}
}
\value{
A vector of temporally autocorrelated random numbers.
}
\description{
Generates temporally autocorrelated random numbers with a mean,
standard deviation, and autocorrelation you specify.
}
\examples{
rednoise <- colored_noise(timesteps = 30, mean = 0.5, sd = 0.2, phi = 0.3)
rednoise
}
