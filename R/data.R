#' Sea Bass
#'
#' A dataset containing the fitted parameters for the Sea Bass fisheries
#'
#' @format An object of class \code{"BioeconomicParams"}.
#' @docType data
#' @usage data("seabass")
#' @references Hannah J Tidbury, Angela Muench, Philip D Lamb, Kieran Hyder, Balancing biological and economic goals in commercial and recreational fisheries: systems modelling of sea bass fisheries, ICES Journal of Marine Science, Volume 78, Issue 5, August 2021, Pages 1793–1803,
#' @examples
#' data("seabass")
#' sim <- project(params, R = 20, t_end = 40)
#' plot(sim)
"params"
