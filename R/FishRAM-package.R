#' Fisheries Resource Allocation Model
#'
#' @description The `FishRAM` package implements the model from Tidbury et. al. (2021) to simulate biological and economic impacts of recreational and commercial fleets.
#'
#' @details
#' Running simulations in `FishRAM` is done in 3 main stages:
#' 1. Set up model parameters using the `BioeconomicParams` constructor function.
#' 2. Run simulations using the `project` function and a recruitment scenario. This creates a `BioeconomicSim` object.
#' 3. Explore results by calling plotting functions (`plot, plotStock, plotActivity, plotEconomicImpact`) on the `BioeconomicSim` object.
#'
#' @docType package
#' @name FishRAM-package
#' @aliases FishRAM
#' @references Hannah J Tidbury, Angela Muench, Philip D Lamb, Kieran Hyder, "Balancing biological and economic goals in commercial and recreational fisheries: systems modelling of sea bass fisheries", *ICES Journal of Marine Science*, Volume 78, Issue 5, August 2021, Pages 1793-1803
NULL
