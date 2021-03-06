% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_populations.R
\name{autocorr_sim}
\alias{autocorr_sim}
\title{Simulate Temporally Autocorrelated Populations for Every Combination of
Parameters}
\usage{
autocorr_sim(
  timesteps,
  start,
  survPhi,
  fecundPhi,
  survMean,
  survSd,
  fecundMean,
  fecundSd,
  replicates
)
}
\arguments{
\item{timesteps}{The number of timesteps you want to simulate. Individuals
are added and killed off every timestep according to the survival and
fertility rates. Can be a scalar or a vector of values to loop over.}

\item{start}{The starting population size. Can be a scalar or vector.}

\item{survPhi}{The temporal autocorrelation of survival. 0 is white noise (uncorrelated),
positive values are red noise (directly correlated) and negative values are
blue noise (inversely correlated). Can be a scalar or a vector.}

\item{fecundPhi}{The temporal autocorrelation of fecundity. As above.}

\item{survMean}{The mean survival from timestep to timestep. Must be a value
between 0 (all individuals die) and 1 (all individuals live). Can be a scalar
or a vector.}

\item{survSd}{The standard deviation of the survival from timestep to
timestep. Must be a value between 0 and 1. Can be a scalar or a vector.}

\item{fecundMean}{The mean fertility: mean offspring produced by each individual per timestep. Can be a scalar or a vector.}

\item{fecundSd}{The standard deviation of the fertility. Can be a scalar or a vector of values.}

\item{replicates}{How many replicates you would like of each possible
combination of parameters.}
}
\value{
A list of data frames, each with fourteen variables: timestep,
  newborns (new individuals added this timestep), survivors (individuals
  alive last year who survived this timestep), population (total individuals
  alive), growth (the increase or decrease in population size from last
  year), estimated survival in the timestep, estimated fecundity in the
  timestep, and the seven parameters used to generate the simulation.
}
\description{
Essentially a loop of \code{\link{unstructured_pop}}, this function simulates a
population with temporally autocorrelated vital rates for every combination
of parameters you specify, with as many replicates as desired. It also
estimates the sample mean survival and fertility for each simulated
population. Please be advised that this function can be very computationally
intensive if you provide many possible parameter values and/or ask for many
replicates.
}
\examples{
survival_range <- autocorr_sim(timesteps = 30, start = 200, survPhi = 0.3, fecundPhi = 0.1,
                               survMean = c(0.2, 0.3, 0.4, 0.5, 0.6), survSd = 0.5,
                               fecundMean = 1.1, fecundSd = 0.5, replicates = 50)
head(survival_range[[1]])
}
