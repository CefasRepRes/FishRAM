#' Constructor for the `BioeconomicSim` class
#'
#' A constructor for the `BioeconomicSim` class. This need not be called directly, as this class is generated automatically via the `project()` function.
#'@param params An object of class `BioeconomicParams`.
#'@param R The recruitment across the simulation. This is either a `numeric` giving the recruitment at each time step, or a function of the previous years adult stock abundance.
#'@param t_start An `integer` specifying the start time for the simulation. The default is `1`
#'@param t_end An `integer` specifying the end time for the simulation. The default is `1 + length(R)`
#'@param R_init A `numeric` giving the initial recruitment value. Only required if `R` is a function of stock size.
#'@return An object of type \linkS4class{BioeconomicSim}
#'@examples
#'data("seabass")
#'recruitment_fun <- function(stock){
#'  return(
#'        1.492*stock/(7.2e-4 + 1.87e-07*stock)*exp(rnorm(1,mean=0,sd=0.9))
#'        )
#'}
#'sim <- BioeconomicSim(params, recruitment_fun, t_start = 1980, t_end = 2020, R_init = 1e5)
#'@export
BioeconomicSim <- function(params, R, t_start, t_end, R_init = NULL) {

  t_max = t_end - t_start + 1
  states <- init_states(params, R, R_init, t_max)

  rec_func <- NULL
  if(is.function(R))
    rec_func <- R

  sim <-new('BioeconomicSim',
            params = params,
            rec_func = rec_func,
            t_start = t_start,
            t_end = t_end,
            states = states)

  return(sim)
}

setClassUnion("funcorNULL", members = c("function", "NULL"))

#### Class definition ####
#' A class to hold the results of a simulation.
#'
#' @slot params A `BioeconomicParams` object encoding the parameters for the model.
#' @slot rec_func The recruitment function
#' @slot t_start The start time of the simulation.
#' @slot t_end The end time of the simulation.
#' @slot states A `data.frame` containing the states of the system for each time step of the simulation.
#' @seealso \link{BioeconomicSim}
#' @details BioeconomicSim
#' @export
setClass(
  "BioeconomicSim",
  slots = c(params = "BioeconomicParams",
            rec_func = "funcorNULL",
            t_start = "numeric",
            t_end = "numeric",
            states = "data.frame")
)

#' Summary plot for `BioeconomicSim` objects.
#'
#' After running a simulation, produces 6 plots: stock size, activity (commercial fleet), activity (recreational fleet), economic impact (total across both fleets), economic impact (commercial fleet), economic impact (recreational fleet).
#' @examples
#' #loads in the relevant parameters
#' data("seabass")
#' # Runs the simulation with 20 years of high recruitment followed by 20 years of low recruitment
#' R <- c(rep(23151200, 20), rep(757600, 20))
#' sim <- project(params, R = R)
#' plot(sim)
#'@export
plot.BioeconomicSim <- function(sim){
  cowplot::plot_grid(plotStock(sim, stock = "B"),
                     plotActivity(sim, fleet = "C"),
                     plotActivity(sim, fleet = "R"),
                     plotEconomicImpact(sim, fleet = "T"),
                     plotEconomicImpact(sim, fleet = "C"),
                     plotEconomicImpact(sim, fleet = "R"),
                     nrow = 6, ncol = 1)
}




init_states <- function(params, R, R_init, t_max){

  state_names <- c("R", "SJ", "SA", "TC", "TR", "ER", "EC", "FC", "FR", "tau", "K", "LR", "LC", "D", "VR", "VC", "ZA")
  states <- matrix(NA, t_max, length(state_names))
  colnames(states) <- state_names
  states <- as.data.frame(states)

  if(!is.function(R))
    states$R <- R
  else
    states$R[1] <- R_init

  # Initialisation
  states$SJ[1] = params@initialSJ
  states$SA[1] = params@initialSA
  states$TC[1] = params@initialTC
  states$TR[1] = params@initialTR

  states$EC[1] = params@chiC * states$TC[1]
  states$ER[1] = params@chiR * states$TR[1]
  states$FC[1] = params@qC * states$EC[1]
  states$FR[1] = params@qR * states$ER[1]

  states$ZA[1] = params@muA + (1-params@varphi * params@delta)*states$FR[1] +  (1 - params@Gamma * params@eta)*states$FC[1]

  #TODO: Need help with the D values here....
  states$D[1] = 1/6

  states$LC[1] <- params@WC*(1 - params@eta)  *states$FC[1]*(1 - exp(-states$ZA[1]))*states$SA[1] / states$ZA[1]
  states$LR[1] <- params@WR*(1 - params@delta)*states$FR[1]*(1 - exp(-states$ZA[1]))*states$SA[1] / states$ZA[1]


  states$VC[1] = params@sigma*params@nu*states$LC[1]
  states$VR[1] = params@zeta*params@lambda*states$TR[1]
  states$tau[1] = params@nu*states$LC[1] - (params@baromega*states$EC[1] + params@Lambda*params@phi)
  states$K[1] = states$FR[1]*states$SA[1]/states$ER[1] #params@b+1
  return(states)

}





