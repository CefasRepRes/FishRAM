#' Constructor for the `BioeconomicParams` class
#'
#' A constructor for the `BioeconomicParams` class.
#'@param file A csv file contatining the input parameters and their values
#'@return An object of type \code{\linkS4class{BioeconomicParams}}
#'@details
#'The `file` params csv file should contain all parameter values required to run the model (see the \code{\linkS4class{BioeconomicParams}} class definition for a full list of required parameters), the first column containing the parameter names and the second column containing the corresponding values.
#'@export
#'@examples
#'\dontrun{
#'params <- BioeconomicParams(file = "params.csv")
#'}
BioeconomicParams <- function(file) {

  params_raw <- read.csv(file)[, 1:2]
  params_raw <- split(params_raw[, 2], params_raw[, 1])

  bioeceonomic_params <- do.call(function(...) new('BioeconomicParams', ...), params_raw)

  return(bioeceonomic_params)
}

#### Class definition ####
#' A class for bioeconomic parameters.
#' @slot g The magnitude of commercial trip change per year.
#' @slot p The magnitude of recreational trip change per year.
#' @slot b Recreational catch per effort threshold.
#' @slot qC Commercial catchability per effort.
#' @slot qR Recreational catchability per effort.
#' @slot muA The natural mortality of the adult stock.
#' @slot muJ The natural mortality of the juvenile stock.
#' @slot chiR Effort per recreational trip.
#' @slot chiC Days per commercial trip.
#' @slot theta Max number of commercial trips per year.
#' @slot gamma Max number of recreational trips per year.
#' @slot delta Proportion of recreational catch released.
#' @slot varphi Proportion of released recreational catch that survives.
#' @slot eta Commercial discard rate.
#' @slot Gamma Proportion of commercial discards that survive.
#' @slot WR Average weight of recreationally landed fish (tonnes).
#' @slot WC Average weight of commercial landed fish (tonnes).
#' @slot nu Commercial fish price per tonnes.
#' @slot baromega Variable cost per commercial day at sea.
#' @slot Lambda Proportion fixed cost attributable to sea bass.
#' @slot phi Fixed cost per fleet per year.
#' @slot sigma GVA multiplier for commercial Revenue.
#' @slot lambda Average per trip expenditure of recreational anglers.
#' @slot zeta GVA multiplier for recreational trip expenditure.
#' @slot initialSJ The starting value of the size of the juvenile stock (in numbers).
#' @slot initialSA The starting value of the size of the adult stock (in numbers).
#' @slot initialTC The starting value for the number of commercial trips.
#' @slot initialTR The starting value for the number of recreational trips.
#' @export
setClass(
  "BioeconomicParams",
  slots = c(theta = "numeric",
             gamma = "numeric",
             g = "numeric",
             p = "numeric",
             b = "numeric",
             WR = "numeric",
             WC = "numeric",
             phiC = "numeric",
             phiR = "numeric",
             muA = "numeric",
             muJ = "numeric",
             sigma = "numeric",
             zeta = "numeric",
             nu = "numeric",
             baromega = "numeric",
             phi = "numeric",
             lambda = "numeric",
             chiR = "numeric",
             chiC = "numeric",
             delta = "numeric",
             eta = "numeric",
             Lambda = "numeric",
             initialSJ = "numeric",
             initialSA = "numeric",
             initialTC = "numeric",
             initialTR = "numeric",
             varphi = "numeric",
             Gamma = "numeric",
            qC = "numeric",
            qR = "numeric")
)
