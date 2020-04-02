#' Query WAQI Stations Readings
#'
#' @description
#' Access the WAQI API to query readings for a given set of monitoring stations.

#' @param x,token,uid See \code{\link{metainfo}}.
#' @param dsn Optional target folder for automated \code{.fst} file output,
#' see also \code{\link[fst]{write_fst}}.
#' @param ... Additional arguments passed to \code{\link{makePSOCKcluster}}.
#'
#' @return
#' Retrieved readings as \code{data.frame}. Note that stations that could not be
#' found or were temporarily out of service are not included.
#'
#' @seealso
#' \code{\link{inventory}}
#'
#' @examples
#' \dontrun{
#' ## spatial extent: bavaria
#' deu = raster::getData(country = "DEU", level = 1)
#' bay = subset(deu, NAME_1 == "Bayern")
#'
#' ## api token
#' tkn = keyring::key_get("waqi_api")
#'
#' ## query monitoring stations within spatial extent
#' stn = inventory(bay, token = tkn)
#'
#' ## query station readings
#' vls = readings(stn, token = tkn, names = parallel::detectCores() - 1)
#' vls
#' }
#'
#' @export readings
#' @name readings
readings = function(x, token, uid = "uid", dsn = NULL, ...) {

  ## disable strings as factors
  saf = default.stringsAsFactors()
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = saf))

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

    # if station is (temporarily) out of service, move on to next station
    if (!is.null(stn$status)) {
      if (stn$status == "error") {
        return(NULL)
      }
    }

    # extract air quality information
    tmp = do.call(cbind, stn$iaqi)
    iaqi = matrix(unlist(tmp), ncol = length(tmp))
    colnames(iaqi) = colnames(tmp)

    # create output
    suppressWarnings(
      data.frame(IDx = stn$idx
                 , do.call(cbind, stn$time)
                 , AQI = stn$aqi
                 , DPL = ifelse(is.null(tmp <- stn$dominentpol), NA, tmp)
                 , iaqi
                 , stringsAsFactors = FALSE)
    )
  })

  ## rbind readings
  out = do.call(plyr::rbind.fill, lst)
  out = preparation(out)
  lst = split(out, out$IDx)

  ## optional file output
  if (!is.null(dsn)) {

    # target files
    ofl = paste0(dsn, "/", names(lst), ".fst")

    # write to disk
    for (i in 1:length(lst)) {

      dat = if (file.exists(ofl[i])) {
        dat = fst::read_fst(ofl[i])

        # synchronize real-time and local columns
        tmp = lst[[i]]
        ids = match(nms <- names(dat), names(tmp))
        tmp = tmp[, stats::na.omit(ids)]

        # if identical record already exists and no new columns were introduced in
        # current reading, move on to next station
        dpl = if (nrow(dat) == 0) {
          FALSE
        } else {
          sapply(1:nrow(dat), function(j) {
            all(dat[j, ] == tmp, na.rm = TRUE)
          })
        }

        if (any(dpl) & all(names(lst[[i]]) %in% nms)) {
          next
        } else {
          plyr::rbind.fill(dat, lst[[i]])
        }
      } else {
        lst[[i]]
      }

      fst::write_fst(dat, ofl[i], compress = 75)
    }
  }

  return(out)
}
