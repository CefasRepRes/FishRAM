% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plotStock}
\alias{plotStock}
\title{Plot the biological outcomes for different allocation scenarios for a simulation}
\usage{
plotStock(sim, stock = "B")
}
\arguments{
\item{sim}{A \code{BioEconomicSim} object.}

\item{stock}{A \code{character} giving the stock desired. This should be \code{"A"} for the adult population, \code{"J"} for the juvenile population, \code{"T"} for the total population  or \code{"B"} for both stocks plotted on the same axis.}
}
\description{
Plot the biological outcomes for different allocation scenarios for a simulation
}
\examples{
#load in the model parameters
data("seabass")
R <- c(rep(23151200, 20), rep(757600, 20))
sim <- project(params, R = R)
plotStock(sim)
plotStock(sim, stock = "T")
plotStock(sim, stock = "A")
}
