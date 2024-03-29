% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation_functions.R
\name{project}
\alias{project}
\title{Project a FishRAM simulation.}
\usage{
project(
  params,
  R,
  t_start = 1,
  t_end = (length(R) + t_start - 1),
  R_init = NULL,
  CLim_func = NULL,
  CLim_alloc = NULL
)
}
\arguments{
\item{params}{An object of type \code{BioeconomicParams} encoding the model parameters.}

\item{R}{A \code{numeric} of \code{function} specifying the recruitment of the stock across the simulation. See details}

\item{t_start}{The year which forms the start of the simulation. The default is 1.}

\item{t_end}{The year which forms the end of the simulation. The default is \code{length(R) + t_start - 1}}

\item{R_init}{The initial recruitment value. Only required if \code{R} is a function.}

\item{CLim_func}{The catch limit function. This should be a \code{function} of the current stock size that returns the total catch limit across both fleets. The default value is \code{NULL} which corresponds to no management of the stock.}

\item{CLim_alloc}{The catch allocation across both fleets. This should be a \code{numeric} of length 2 that sums to 1. The first element should be the proportion of total catch assigned to the commercial fleet, while the second element should be the proportion of total catch assigned to the recreational fleet. The default is \code{NULL} - no management.}
}
\value{
An object of class \linkS4class{BioeconomicSim}. See the \code{BioeconomicSim-class} help file for a full list of model outputs included in this object.
}
\description{
Runs the FishRAM model for a specified recruitment scenario. Full details of the model specification can be found in the supporting information of Tidbury et al. (2021), however some details have been extended for this package (see details below).
}
\details{
Recruitment can be specified in 3 ways using different forms of the \code{R} parameter:
\itemize{
\item A \code{numeric} of length 1, giving the constant recruitment across the duration of the simulation.
\item A \code{numeric} of length \code{t_end - t_start + 1}, specifying the recruitment at each time step of the simulation.
\item A \code{function} which calculates recruitment from the adult stock size at each time step. This function should have exactly one argument (the adult stock size) and return a \code{numeric} of length 1. If using a function, the \code{R_init} argument should also be passed through to specify the initial recruitment at the start of the simulation.
}
}
\section{Model extensions}{

\emph{Mortality reparametrisation.} To allow for a larger range of mortalities, the mortality in \code{FishRAM} is parametrised differently to the mortality in Tidbury et al. (2021). The stock dynamics for the adult population are instead calculated through
\deqn{S_{A, t} = \exp(M_A + (1 - \varphi \delta)F_{R, t} + (1-\Gamma \eta)F_{C, t})S_{A, t-1} + D_{t}S_{J, t}}.
With the landings of each fleet now calculated as
\deqn{L_{R, t} = W_C \frac{(1-\delta)F_{R, t}}{M_A + (1 - \varphi \delta)F_{R, t} + (1-\Gamma \eta)F_{C, t}}S_{A, t}}
\deqn{L_{C, t} = W_C \frac{(1-\eta)F_{C, t}}{M_A + (1 - \varphi \delta)F_{R, t} + (1-\Gamma \eta)F_{C, t}}S_{A, t}}
This is equivalent to assuming that all mortalities (commercial, recreational, and natural) are applied simultaneously to the adult stock.

\emph{Initial values}
For the first 5 years of the simulation, some quantities relying on the history of simulation are calculated differently, by assuming some values were static prior to the start of the simulation.
\itemize{
\item Number of trips. The number of trips of the commercial fleet \eqn{T_C} depends on the profit of the fleet from 2 years ago. The commercial fleet profit is therefore assumed to be constant before the simulation starts (i.e. for time \eqn{t <= 0}) to ensure that \eqn{T_C} can be calculated for the first 2 years.
\item Maturation. The fraction of juveniles maturing into adults in a given year \eqn{D_t} is estimated from the recruitment values from 5 years previously. Recruitment is therefore assumed to be constant before the start of the simulation so that $D_t$ can be calculated for the first 5 years.
}
}

\examples{
#loads in the relevant parameters
data("seabass")
sim <- project(params, R = 8606000, t_start = 1990, t_end = 2030)

#Runs the simulation with 20 years of high recruitment followed
#by 20 years of low recruitment
R <- c(rep(23151200, 20), rep(757600, 20))
sim <- project(params, R = R)

#Generates recruitment from a type II functional response
#recruitment function with log Gaussian noise.
rec_func <- function(stock){
  return(
    1.5*stock/(7e-4 + 1.3e-4 * 1.5e-3 * stock)*exp(rnorm(1,mean=0,sd=0.9))
  )
}
sim <- project(params, R = rec_func, t_start = 2010, t_end = 2050, R_init = 1e4)

# Including management strategies
# MSY as calculated by the original Tidbury paper
CLim_func <- function(stock){
return(0.203/(0.203 + 0.24) * stock * (1 - exp(-(0.203 + 0.24))))
}
# All allocated to the commercial fleet
sim <- project(params, R = rec_func, t_start = 2010, t_end = 2050, R_init = 1e4,
              CLim_func = CLim_func, CLim_alloc = c(1,0))
# All allocated to the recreational fleet
sim <- project(params, R = rec_func, t_start = 2010, t_end = 2050, R_init = 1e4,
              CLim_func = CLim_func, CLim_alloc = c(0, 1))
# Allocated equally across both fleets
sim <- project(params, R = rec_func, t_start = 2010, t_end = 2050, R_init = 1e4,
              CLim_func = CLim_func, CLim_alloc = c(0.5,0.5))
}
\references{
Hannah J Tidbury, Angela Muench, Philip D Lamb, Kieran Hyder, Balancing biological and economic goals in commercial and recreational fisheries: systems modelling of sea bass fisheries, \emph{ICES Journal of Marine Science}, Volume 78, Issue 5, August 2021, Pages 1793-1803
}
