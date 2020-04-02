#' Query WAQI Stations Within Spatial Extent
#'
#' @description
#' Access the WAQI API to query all available monitoring stations within a given
#' spatial extent.

#' @param x A \code{sf} or \code{Spatial} object from which to extract spatial extent.
#' @param token \href{http://aqicn.org/faq/2015-09-18/map-web-service-real-time-air-quality-tile-api/}{API token ID}
#' for Air Quality Open Data Platform as \code{character}.
#' @param ... Currently not used.
#'
#' @return
#' Monitoring stations within spatial extent as \code{sf} object.
#'
#' @note
#' The \strong{rgeos} package is required for finding out which hole belongs to
#' which exterior ring in \code{\link[sf]{st_as_sfc}} for \code{SpatialPolygons}.
#'
#' @examples
#' \dontrun{
#' deu = raster::getData(country = "DEU", level = 0)
#' stn = inventory(deu, token = keyring::key_get("waqi_api"))
#'
#' library(mapview)
#' mapview(deu, alpha.regions = .25) + stn
#' }
#'
#' @export inventory
#' @name inventory
inventory = function(x, token, ...) {

  ## if required, transform 'Spatial' object into 'sf'
  if (inherits(x, "Spatial")) {
    x = sf::st_as_sf(x)
  }

  ## extract spatial extent in order required by api
  ext = as.numeric(sf::st_bbox(x))[c(2, 1, 4, 3)]

  ## construct query based on token id and spatial extent
  api = paste0("http://api.waqi.info/map/bounds/?token="
               , token
               , "&latlng="
               , paste(ext, collapse = ","))

  ## download and import station inventory
  txt = RCurl::getURLContent(api)
  stn = jsonlite::fromJSON(txt)$data

  ## remove stations outside spatial boundary
  sfd = sf::st_as_sf(stn, coords = c("lon", "lat"), crs = 4326)
  sfd = suppressMessages(sfd[x, ])

  return(sfd)
}
