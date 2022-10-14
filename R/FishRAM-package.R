#' Fisheries Resource Allocation Model
#'
#' @description The `FishRAM` package implements the dynamic system model from Tidbury et. al. (2021) which simulates the biological and economic impacts for recreational and commercial fishing under different stock allocation scenarios. Default values are set to represent the biology of Sea bass \emph{Dicentrarchus labrax} in division  27-4.b–c (North Atlantic stock) and English inshore commercial fishing fleet and recreational anglers.
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
