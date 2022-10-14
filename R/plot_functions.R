#' Plot the biological outcomes for different allocation scenarios for a simulation
#' @param sim A `BioEconomicSim` object.
#' @param stock A `character` giving the stock desired. This should be `"A"` for the adult population, `"J"` for the juvenile population, `"T"` for the total population  or `"B"` for both stocks plotted on the same axis.
#' @examples
#' #load in the model parameters
#' data("seabass")
#' R <- c(rep(23151200, 20), rep(757600, 20))
#' sim <- project(params, R = R)
#' plotStock(sim)
#' plotStock(sim, stock = "T")
#' plotStock(sim, stock = "A")
#' @export
plotStock <- function(sim, stock = "B"){
  if(!is(sim, "BioeconomicSim")){
    stop("sim should be ")
  }

  if(!(stock %in% c("B", "T", "J", "A"))){
    warning("Stock should be a character specifying the stock type that should be plotted. Plotting both stocks.")
  }

  years <- sim@t_start:sim@t_end
  if (stock == "A"){
    df <- cbind(years, sim@states[, "SA"])
    colnames(df) <- c("Year", "Adult popn (000)s")
    df[, "Adult popn (000)s"] <- df[, "Adult popn (000)s"] / 1000
    df <- as.data.frame(df)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Adult popn (000)s`)) + ggplot2::geom_line()
    )
  }else if(stock == "J"){
    df <- cbind(years, sim@states[, "SJ"])
    colnames(df) <- c("Year", "Juvenile popn (000)s")
    df[, "Juvenile popn (000)s"] <- df[, "Juvenile popn (000)s"] / 1000
    df <- as.data.frame(df)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Juvenile popn (000)s`)) + ggplot2::geom_line()
    )
  }else if(stock == "T"){
    df <- data.frame(years, rowSums(sim@states[, c("SA", "SJ")]))
    colnames(df) <- c("Year", "Population (000s)")
    df[, "Population (000s)"] <- df[, "Population (000s)"] / 1000
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Population (000s)`)) + ggplot2::geom_line()
    )

  }
  df <- cbind(years, sim@states[, c("SJ", "SA")])
  colnames(df) <- c("Year", "Juvenile", "Adult")
  df[, "Juvenile"] <- df[, "Juvenile"] / 1000
  df[, "Adult"] <- df[, "Adult"] / 1000

  df <- reshape2::melt(df, id.vars="Year")
  colnames(df) <- c("Year", "Stock", "Population (000s)")
   return(
    ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Population (000s)`, group = `Stock`, colour = `Stock`)) + ggplot2::geom_line()
  )
}

#' Plot the economic impact for a simulation for commercial and recreational fishing.
#' @param sim A `BioEconomicSim` object.
#' @param fleet A `character` specifying which fleet to plot. This should be one of `"C"` to plot the commercial fleet, `"R"` to plot the recreational fleet, "T" to plot the total GVA, or `"B"` to plot both fleets on the same axis.
#' @examples
#' #load in the model parameters
#' data("seabass")
#' R <- c(rep(23151200, 20), rep(757600, 20))
#' sim <- project(params, R = R)
#' plotEconomicImpact(sim)
#' plotEconomicImpact(sim, fleet = "T")
#' plotEconomicImpact(sim, fleet = "C")
#'
#' @export
plotEconomicImpact <- function(sim, fleet = "B"){
  if(!(fleet %in% c("B", "R", "T", "C")))
    warning("Fleet should be one of R, C, T, or B to specify the fleet. Plotting both fleets.")


  years <- sim@t_start:sim@t_end
  if(fleet == "C"){
    df <- data.frame(years, sim@states[, "VC"])
    colnames(df) <- c("Year", "Commercial GVA")
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Commercial GVA`)) + ggplot2::geom_line()
    )
  }else if(fleet == "R"){
    df <- cbind(years, sim@states[, "VR"])
    colnames(df) <- c("Year", "Recreational GVA")
    df <- as.data.frame(df)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Recreational GVA`)) + ggplot2::geom_line()
    )
  }
  df <- data.frame(years, sim@states[, c("VR", "VC")])
  colnames(df) <- c("Year", "Recreational GVA", "Commercial GVA")
  df <- reshape2::melt(df, id.vars = "Year")
  colnames(df) <- c("Year", "Fleet", "GVA")
  if(fleet == "T"){
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `GVA`, fill = `Fleet`)) + ggplot2::geom_bar(stat = "identity")
    )
  }
  return(
    ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `GVA`, group = `Fleet`, colour = `Fleet`)) + ggplot2::geom_line()
  )
}


#' Plot the activity (number of trips) for a simulation.
#' Under the assumption that recreational and commercial fishers usually conduct daytrips, this indicator can be used as an effort measure.
#' @param sim A `BioEconomicSim` object.
#' @param fleet A `character` specifying which fleet to plot. This should be one of `"C"` to plot the commercial fleet, `"R"` to plot the recreational fleet, or `"B"` to plot both fleets on the same axis.
#' @examples
#' #load in the model parameters
#' data("seabass")
#' R <- c(rep(23151200, 20), rep(757600, 20))
#' sim <- project(params, R = R)
#' plotActivity(sim)
#' plotActivity(sim, fleet = "R")
#' plotActivity(sim, fleet = "C")
#' @export
plotActivity <- function(sim, fleet = "B"){
  if(!(fleet %in% c("B", "R", "C")))
    warning("Fleet should be one of R, C, or B to specify the fleet. Plotting both fleets.")


  years <- sim@t_start:sim@t_end
  if(fleet == "C"){
    df <- data.frame(years, sim@states[, "TC"])
    df[, 2] <- df[, 2] / 1000
    colnames(df) <- c("Year", "Commercial Trips (000s)")
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Commercial Trips (000s)`)) + ggplot2::geom_line()
    )
  }else if(fleet == "R"){
    df <- cbind(years, sim@states[, "TR"])
    df[, 2] <- df[, 2] / 1000
    colnames(df) <- c("Year", "Recreational Trips (000s)")
    df <- as.data.frame(df)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Recreational Trips (000s)`)) + ggplot2::geom_line()
    )
  }
  df <- data.frame(years, sim@states[, c("TR", "TC")])
  df[, c("TR", "TC")] <- df[, c("TR", "TC")] / 1000

  colnames(df) <- c("Year", "Recreational Trips (000s)","Commercial Trips (000s)")
  df <- reshape2::melt(df, id.vars = "Year")
  colnames(df) <- c("Year", "Fleet", "Trips (000s)")
  return(ggplot2::ggplot(df, ggplot2::aes(x=`Year`, y = `Trips (000s)`, group = `Fleet`, colour = `Fleet`)) + ggplot2::geom_line())
}

