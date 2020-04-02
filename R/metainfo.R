#' Query WAQI Stations Metainformation
#'
#' @description
#' Access the WAQI API to query metainformation for a given set of monitoring
#' stations.
#'
#' @param x WAQI stations as \code{sf} or \code{Spatial} object, usually derived
#' from \code{\link{inventory}}.
#' @param token \href{http://aqicn.org/faq/2015-09-18/map-web-service-real-time-air-quality-tile-api/}{API token ID}
#' for Air Quality Open Data Platform as \code{character}.
#' @param uid Column in 'x' with unique monitoring station ID as
#' \code{character}; defaults to "uid" which is the \href{http://aqicn.org/json-api/doc/}{API standard}.
#' @param stock A \code{data.frame} of already available metainformation created
#' during a previous function run, see 'Details'.
#' @param sort A \code{logical} determining whether the rows are to be re-arranged
#' by unique IDs.
#' @param ... Additional arguments passed to \code{\link{makePSOCKcluster}}.
#'
#' @return
#' Retrieved metainformation as \code{data.frame}.
#'
#' @details
#' Providing already available metainformation is a convenient way to process
#' either newly added monitoring stations or stations that have temporarily been
#' out of service during a previous function run only, and hence reduce
#' computation time significantly.
#'
#' @seealso
#' \code{\link{inventory}}
#'
#' @examples
#' \dontrun{
#' tkn = keyring::key_get("waqi_api")
#'
#' deu = raster::getData(country = "DEU", level = 0)
#' stn = inventory(deu, token = tkn)
#'
#' nfo = metainfo(stn, token = tkn)
#' head(nfo, 10)
#' }
#'
#' @export metainfo
#' @name metainfo
metainfo = function(x, token, uid = "uid", stock = NULL, sort = TRUE, ...) {

  ## if required, transform 'Spatial' object into 'sf' before converting to
  ## usual 'data.frame'
  if (inherits(x, "Spatial")) {
    x = sf::st_as_sf(x)
  }

  x = data.frame(x)

  ## parallelization
  dots = list(...)

  if (!"names" %in% names(dots)) {
    dots$names = 1L
  }

  cl = do.call(parallel::makePSOCKcluster, args = dots)
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(cl, c("x", "uid", "token"), envir = environment())

  ## loop over station list
  lst = parallel::parLapply(cl, 1:nrow(x), function(i) {

    # construct query based on token id and unique station id
    api = paste0("http://api.waqi.info/feed/@"
                 , x[i, uid]
                 , "/?token="
                 , token)

    # download and import current station measurements
    stn = NULL

    while (all(is.null(stn))) {
      Sys.sleep(.5)
      txt = try(RCurl::getURLContent(api), silent = TRUE)
      if (inherits(txt, "try-error")) {
        next
      } else {
        stn = jsonlite::fromJSON(txt)$data

        if (length(stn) > 0) {
          if (stn[1] %in% c("concurrent", "Can not connect")) {
            stn = NULL
          }
        }
      }
    }

    # extract epa attribution for current station
    atb = stn$attributions

    if (nrow(atb) > 1) {
      atb = Orcs::list2df(lapply(1:nrow(atb), function(j) atb[j, ]), "cols")
    }

    # (prevent duplicate column names)
    ids_url = grep("url", names(atb))
    names(atb)[ids_url] = paste0("atb_url_", sqc <- 1:(ncol(atb) / 2))
    names(atb)[-ids_url] = paste0("atb_name_", sqc)

    # extract information about monitoring station
    cty = stn$city
    ids = grep("geo", names(cty))
    cty = Orcs::list2df(cty[-ids], bind = "cols")

    # (prevent duplicate column names)
    ids_url = grep("url", names(cty))
    names(cty)[ids_url] = "stn_url"
    names(cty)[-ids_url] = "stn_name"

    # create output
    suppressWarnings(
      data.frame(IDx = stn$idx
                 , atb
                 , cty)
    )
  })

  nfo = do.call(plyr::rbind.fill, lst)

  ## reorder columns (id, station name and url, attribution name and url)
  nms = names(nfo)
  ids = sapply(c("stn_name", "stn_url"), function(i) {
    grep(i, nms)
  })

  tmp1 = grep("atb_name", nms); tmp2 = grep("atb_url", nms)
  tmp = if (length(tmp1) > 1 & length(tmp1) == length(tmp2)) {
    unlist(lapply(1:length(tmp1), function(i) c(tmp1[i], tmp2[i])))
  } else c(tmp1, tmp2)

  nfo = nfo[, c(1, ids, tmp)]

  ## reorder rows according to uid (optional)
  if (sort) {
    nfo = nfo[order(nfo[, 1]), ]
  }

  return(nfo)
}
