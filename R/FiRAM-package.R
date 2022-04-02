#' Fisheries Resource Allocation Model
#'
#' @description The `FiRAM` package implements the model from Tidbury et. al. (2021) to simulate biological and economic impacts of recreational and commercial fleets.
#'
#' @details
#' Running simulations in \pkg{FiRAM} is done in 3 main stages:
#' 1. Set up model parameters using the `BioeconomicParams` function.
#' 2. Run simulations using the `project` function. This creates a `BioeconomicSim` object.
#' 3. Explore results using the plotting functions (`plot, plotStock, plotActivity, plotEconomicImpact`)
#'
#' @docType package
#' @name FiRAM-package
#' @aliases FiRAM
#' @references Hannah J Tidbury, Angela Muench, Philip D Lamb, Kieran Hyder, \"Balancing biological and economic goals in commercial and recreational fisheries: systems modelling of sea bass fisheries\", ICES Journal of Marine Science, Volume 78, Issue 5, August 2021, Pages 1793-1803
NULL
