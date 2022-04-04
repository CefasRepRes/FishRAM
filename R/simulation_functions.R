step = function(sim, t){

  params <- sim@params

  states <- sim@states[t, ]
  previous_states <- sim@states[t - 1, ]


  if(sum(is.na(previous_states)) != 0){
    stop("Invalid states detected.")
  }

  #Step 1. Work out the number of trips that should occur
  #Commercial trips
  previous_profit <- previous_states$tau
  if(t > 2){
    previous_profit <- sim@states[t - 2, ]$tau
  }

  if (previous_profit >= 0 && previous_states$TC < params@theta){
    states$TC <- min((1 + params@g)*previous_states$TC, params@theta)
  }else if(previous_profit < 0){
    states$TC <- (1 - params@g)*previous_states$TC
  }else{
    states$TC <- params@theta
  }

  #Recreational trips
  if(previous_states$K >= params@b && previous_states$TR < params@gamma){
    states$TR <- min((1 + params@p)*previous_states$TR, params@gamma)
  }else if(previous_states$K < params@b){
    states$TR <- (1 - params@p)*previous_states$TR
  }else{
    states$TR <- params@gamma
  }

  #Step 2: Translate this into effort and fishing pressure
  states$EC <- params@chiC * states$TC
  states$ER <- params@chiR * states$TR
  states$FC <- params@qC * states$EC
  states$FR <- params@qR * states$ER

  #Step 3: Stock effects: Recruitment
  if(is.function(sim@rec_func)){
    states$R <- sim@rec_func(previous_states$SA)
  }

  #Step 4: Stock effects: Stock sizes
  states$SJ = previous_states$SJ + states$R - (1 - exp(-params@muJ))*previous_states$SJ - previous_states$D*previous_states$SJ
  states$ZA <- params@muA + (1-params@varphi * params@delta)*states$FR +  (1 - params@Gamma * params@eta)*states$FC
  states$SA = exp(-states$ZA)*previous_states$SA + previous_states$D*previous_states$SJ

  states$D <- exp(-6*params@muJ)
  if(t > 5){
    states$D <- exp(-6*params@muJ) * sim@states[t - 5, "R"]/ states$SJ
  }

  #Step 5: Economic impacts: Commercial fleet
  states$LC  <- params@WC * (1-params@eta) * states$FC * (1 - exp(-states$ZA)) * states$SA / states$ZA

  states$tau <- params@nu * states$LC - (params@baromega * states$EC + params@Lambda * params@phi)
  states$VC  <- params@sigma * params@nu * states$LC

  #Step 6: Economic impacts: Recreational fleet
  states$LR <- params@WR * (1 - params@delta) * states$FR * (1 - exp(-states$ZA)) * states$SA / states$ZA
  states$VR <- params@zeta * params@lambda * states$TR


  #Step 7: CPUE for recreational fleet.
  states$K <- (1-exp(-states$FR))*states$SA/states$ER

  return(states)
}

#' Project a FiRAM simulation.
#'
#' Runs the FiRAM model for a specified recruitment scenario. Full details of the model specification can be found in the supporting information of Tidbury et al. (2021), However some details have been extended for this package (see details below)
#' @param params An object of type `BioeconomicParams` encoding the model parameters.
#' @param R A `numeric` of `function` specifying the recruitment of the stock across the simulation. See details
#' @param t_start The year which forms the start of the simulation. The default is 1.
#' @param t_end The year which forms the end of the simulation. The default is `length(R) + t_start - 1`
#' @param R_init The initial recruitment value. Only required if `R` is a function.
#' @return An object of class [BioeconomicSim]
#' @details
#' Recruitment can be specified in 3 ways using different forms of the `R` parameter:
#' * A `numeric` of length 1, giving the constant recruitment across the duration of the simulation.
#' * A `numeric` of length `t_end - t_start + 1`, specifying the recruitment at each time step of the simulation.
#' * A `function` which calculates recruitment from the adult stock size at each time step. This function should have exactly one argument (the adult stock size) and return a `numeric` of length 1. If using a function, the `R_init` argument should also be passed through to specify the initial recruitment at the start of the simulation.
#'
#'
#' @section Model extensions:
#' *Mortality reparametrisation.* To allow for a larger range of mortalities, the mortality in `FiRAM` is parametrised differently to the mortality in Tidbury et al. (2021). The stock dynamics for the adult population are instead calculated through
#' \deqn{S_{A, t} = \exp(M_A + (1 - \varphi \delta)F_{R, t} + (1-\Gamma \eta)F_{C, t})S_{A, t-1} + D_{t}S_{J, t}}.
#' With the landings of each fleet now calculated as
#' \deqn{L_{R, t} = W_C \frac{(1-\delta)F_{R, t}}{M_A + (1 - \varphi \delta)F_{R, t} + (1-\Gamma \eta)F_{C, t}}S_{A, t}}
#' \deqn{L_{C, t} = W_C \frac{(1-\eta)F_{C, t}}{M_A + (1 - \varphi \delta)F_{R, t} + (1-\Gamma \eta)F_{C, t}}S_{A, t}}
#' This is equivalent to assuming that all mortalities (commercial, recreational, and natural) are applied simultaneously to the adult stock.
#'
#' *Initial values*
#' For the first 5 years of the simulation, some quantities relying on the history of simulation are calculated differently, by assuming some values were static prior to the start of the simulation.
#' * Number of trips. The number of trips of the commercial fleet \eqn{T_C} depends on the profit of the fleet from 2 years ago. The commercial fleet profit is therefore assumed to be constant before the simulation starts (i.e. for time \eqn{t <= 0}) to ensure that \eqn{T_C} can be calculated for the first 2 years.
#' * Maturation. The fraction of juveniles maturing into adults in a given year \eqn{D_t} is estimated from the recruitment values from 5 years previously. Recruitment is therefore assumed to be constant before the start of the simulation so that $D_t$ can be calculated for the first 5 years.
#'
#' @references Hannah J Tidbury, Angela Muench, Philip D Lamb, Kieran Hyder, Balancing biological and economic goals in commercial and recreational fisheries: systems modelling of sea bass fisheries, *ICES Journal of Marine Science*, Volume 78, Issue 5, August 2021, Pages 1793-1803
#' @export
#' @examples
#' #loads in the relevant parameters
#' data("seabass")
#' sim <- project(params, R = 8606000, t_start = 1990, t_end = 2030)
#'
#' # Runs the simulation with 20 years of high recruitment followed by 20 years of low recruitment
#' R <- c(rep(23151200, 20), rep(757600, 20))
#' sim <- project(params, R = R)
#'
#' #Generates recruitment from a type II functional response recruitment function with log Gaussian noise.
#' rec_func <- function(stock){
#'   return(
#'     1.5@*stock/(7e-4 + 1.3e-4 @* 1.5e-3 @* stock)@*exp(rnorm(1,mean=0,sd=0.9))
#'   )
#' }
#' sim <- project(params, R = rec_func, t_start = 2010, t_end = 2050, R_init = 1e4)
project = function(params, R, t_start = 1, t_end = (length(R) + t_start - 1), R_init = NULL){
  t_max <- t_end - t_start + 1

  if(!is(params, "BioeconomicParams")){
    stop("params should be a BioeconomicParams object")
  }
  if(!(is.numeric(R) || is.function(R))){
    stop("R should be a numeric vector or function specifying recruitment")
  }
 if(!(length(R) == 1 || length(R) == t_max)){
  stop("R should have length 1 or length equal to the duration of the simulation")
 }

  if(is.function(R) && is.null(R_init)){
    stop("R is a function, but no initial values have been specified. The initial value should be specified using the R_init parameter.")
  }

  #TODO: Think about chaining sim together, so setting your own initial values outside of the params object?
  sim <- BioeconomicSim(params, R, t_start, t_end, R_init)

  for (t in 2:t_max){
    sim@states[t, ] = step(sim, t)
  }

  return(sim)
}
