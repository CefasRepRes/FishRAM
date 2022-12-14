#' Constructor for the `BioeconomicSim` class
#'
#' A constructor for the `BioeconomicSim` class. This need not be called directly, as this class is generated automatically via the `project()` function.
#'@param params An object of class `BioeconomicParams`.
#'@param R The recruitment across the simulation. This is either a `numeric` giving the recruitment at each time step, or a function of the previous years adult stock abundance.
#'@param t_start An `integer` specifying the start time for the simulation. The default is `1`
#'@param t_end An `integer` specifying the end time for the simulation. The default is `1 + length(R)`
#'@param R_init A `numeric` giving the initial recruitment value. Only required if `R` is a function of stock size.
#' @param CLim_func The catch limit function. This should be a `function` of the current stock size that returns the total catch limit across both fleets. The default value is `NULL` which corresponds to no management of the stock.
#' @param CLim_alloc The catch allocation across both fleets. This should be a `numeric` of length 2 that sums to 1. The first element should be the proportion of total catch assigned to the commercial fleet, while the second element should be the proportion of total catch assigned to the recreational fleet. The default is `NULL` - no management.
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
BioeconomicSim <- function(params, R, t_start, t_end, R_init = NULL, CLim_func = NULL, CLim_alloc = NULL){

  states <- init_states(params, R, R_init, t_start, t_end, CLim_func, CLim_alloc)

  rec_func <- NULL
  if(is.function(R))
    rec_func <- R

  sim <-methods::new('BioeconomicSim',
            params = params,
            rec_func = rec_func,
            CLim_func = CLim_func,
            t_start = t_start,
            t_end = t_end,
            states = states)

  return(sim)
}

#A hack to allow the recruitment functions and catch limit functions to be either a function or a NULL object. This is needed to define the class.
setClassUnion("funcorNULL", members = c("function", "NULL"))


#### Class definition ####
#' A class to hold the results of a simulation.
#'
#' @slot params A `BioeconomicParams` object encoding the parameters for the model.
#' @slot rec_func The recruitment function
#' @slot CLim_func The function to calculate the total catch limit (across both fleets) from the adult stock size (in numbers).
#' @slot t_start The start time of the simulation.
#' @slot t_end The end time of the simulation.
#' @slot states A `data.frame` containing the states of the system for each time step of the simulation.
#' @seealso \link{BioeconomicSim}
#' @details
#' The states of the simulation at each time step are stored in the `states` slot of this object. The `states` slot contains a \eqn{T \times 20} matrix where \eqn{T} is the duration of the simulation. Each column represents a different model output and each row gives the value of those outputs at a time step. The columns in this matrix are:
#' 1. R - The recruitment (numbers)
#' 2. SJ - The size of the juvenile stock (numbers)
#' 3. SA - The size of the adult stock (numbers)
#' 4. TC - The number of commercial trips
#' 5. TR - The number of recreationl trips.
#' 6. EC - The commercial effort
#' 7. ER - The recreational effort
#' 8. FC - The commercial fishing mortality
#' 9. FR - The recreational fishing mortality
#' 10. tau - The profit of the commercial fleet
#' 11. K - The catch per unit effort of the recreational fleet
#' 12. LR - The landed catch of the recreational fleet (tonnes)
#' 13. LC - The landed catch of the commercial fleet (tonnes)
#' 14. D - The proportion of the juvenile stock that mature into the adult stock
#' 15. VR - The GVA of the recreational fleet
#' 16. VC - The GVA of the commercial fleet
#' 17. ZA - The total mortality on the adult stock
#' 18. CLim - The total catch limit (in numbers) for a given time step.
#' 19. CAllocCom - The proportion of CLim that is allocated to the commercial fleet
#' 20. CAllocRec - The proportion of CLim that is allocated to the recreational fleet
#' @export
setClass(
  "BioeconomicSim",
  slots = c(params = "BioeconomicParams",
            rec_func = "funcorNULL",
            CLim_func = "funcorNULL",
            t_start = "numeric",
            t_end = "numeric",
            states = "data.frame")
)

#' Summary plot for `BioeconomicSim` objects.
#'
#' After running a simulation, produces 6 plots: stock size, activity (commercial fleet), activity (recreational fleet), economic impact (total across both fleets), economic impact (commercial fleet), economic impact (recreational fleet).
#' @param x A `BioEconomicSim` object
#' @param ... Additional plotting parameters. Currently unused.
#' @examples
#' #loads in the relevant parameters
#' data("seabass")
#' # Runs the simulation with 20 years of high recruitment followed by 20 years of low recruitment
#' R <- c(rep(23151200, 20), rep(757600, 20))
#' sim <- project(params, R = R)
#' plot(sim)
#'@export
plot.BioeconomicSim <- function(x, ...){
  cowplot::plot_grid(plotStock(x, stock = "B"),
                     plotActivity(x, fleet = "C"),
                     plotActivity(x, fleet = "R"),
                     plotEconomicImpact(x, fleet = "T"),
                     plotEconomicImpact(x, fleet = "C"),
                     plotEconomicImpact(x, fleet = "R"),
                     nrow = 6, ncol = 1)
}

init_states <- function(params, R, R_init, t_start, t_end, CLim_func, CLim_alloc){
  state_names <- c("R", "SJ", "SA", "TC", "TR", "EC", "ER", "FC", "FR", "tau", "K", "LR", "LC", "D", "VR", "VC", "ZA", "t", "CLim", "CAllocCom", "CAllocRec")
  states <- matrix(NA, t_end - t_start + 1, length(state_names))
  colnames(states) <- state_names
  states <- as.data.frame(states)

  states$t <- t_start:t_end

  if(!is.function(R))
    states$R <- R
  else
    states$R[1] <- R_init

 states$CLim <- -1; states$CAllocCom <- -1;states$CAllocRec <- -1 # Default values

  if(is.function(CLim_func)){
    states$CAllocCom <- CLim_alloc[1]
    states$CAllocRec <- CLim_alloc[2]
    states$CLim[1] <- CLim_func(params@initialSA)
  }

  # Initialisation
  states$SJ[1] <- params@initialSJ
  states$SA[1] <- params@initialSA
  states$TC[1] <- params@initialTC
  states$TR[1] <- params@initialTR

  states$EC[1] <- params@chiC * states$TC[1]
  states$ER[1] <- params@chiR * states$TR[1]
  states$FC[1] <- params@qC * states$EC[1]
  states$FR[1] <- params@qR * states$ER[1]

  states$ZA[1] <- params@muA + (1-params@varphi * params@delta)*states$FR[1] +  (1 - params@Gamma * params@eta)*states$FC[1]

  states$D[1] <- exp(-6*params@muJ)

  states$LC[1] <- params@WC*(1 - params@eta)  *states$FC[1]*(1 - exp(-states$ZA[1]))*states$SA[1] / states$ZA[1]
  states$LR[1] <- params@WR*(1 - params@delta)*states$FR[1]*(1 - exp(-states$ZA[1]))*states$SA[1] / states$ZA[1]


  states$VC[1] <- params@sigma*params@nu*states$LC[1]
  states$VR[1] <- params@zeta*params@lambda*states$TR[1]
  states$tau[1]<- params@nu*states$LC[1] - (params@baromega*states$EC[1] + params@Lambda*params@phi)
  states$K[1]  <- states$FR[1]*states$SA[1]/states$ER[1] #params@b+1
  return(states)

}





