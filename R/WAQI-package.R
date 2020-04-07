#' Assessment of Air Cleanliness using Open Data
#'
#' Access real-time data through the World Air Quality Index (WAQI) project's
#' API.
#'
#' @name claiR-package
#' @docType package
#' @title Assessment of Air Cleanliness using Open Data
#' @author Florian Detsch
#' \cr
#' \cr
#' \emph{Maintainer:} Florian Detsch \email{fdetsch@@web.de}
#'
#' @import parallel rgeos sf
#' @importFrom devtools install_github
#' @importFrom fst read_fst write_fst
#' @importFrom jsonlite fromJSON
#' @importFrom Orcs list2df
#' @importFrom plyr rbind.fill
#' @importFrom R.utils withTimeout
#' @importFrom RCurl getURLContent
#' @importFrom stats na.omit
#'
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name WAQIstations
#' @title WAQI Stations in Germany
#' @description World Air Qualilty Index (WAQI) project monitoring stations
#' located in Germany.
#' @details Data available through the Air Quality Programmatic API (see Source).
#' @format \code{sf data.frame} with 210 features and 2 fields covering a
#' spatial extent from 6.094444, 47.515447, 14.638056, 53.638190 (xmin, ymin,
#' xmax, ymax; EPSG:4326).
#' @source
#' \url{http://aqicn.org/api/de/}
#'
NULL
