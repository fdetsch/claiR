#' Query WAQI Stations Metainformation and Readings
#'
#' @description
#' Access the WAQI API to query metainformation and readings for a given set of
#' monitoring stations.
#'
#' @param x,token,uid,stock,sort,dsn,... See \code{\link{metainfo}} and
#' \code{\link{readings}}.
#'
#' @return
#' A \code{list} with retrieved (i) metainformation and (ii) readings, each as a
#' separate \code{data.frame}.
#'
#' @examples
#' ## see ?metainfo and ?readings
#'
#' @export retrieval
#' @name retrieval
retrieval = function(x, token, uid = "uid"
                     , stock = NULL, sort = TRUE # metainfo
                     , dsn = NULL # readings
                     , ...) {

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
  jnk = parallel::clusterEvalQ(cl, options(stringsAsFactors = FALSE))

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

    # geographic coordinates (if available)
    crd = stn$city[[ids]]
    crd = if (!is.null(crd)) {
      data.frame(lon = crd[2], lat = crd[1])
    } else {
      data.frame(lon = NA, lat = NA)
    }

    # (prevent duplicate column names)
    ids_url = grep("url", names(cty))
    names(cty)[ids_url] = "stn_url"
    names(cty)[-ids_url] = "stn_name"

    # create output (readings)
    suppressWarnings(
      list(data.frame(IDx = stn$idx
                      , atb
                      , cty)
           , data.frame(IDx = stn$idx
                        , crd
                        , do.call(cbind, stn$time)
                        , AQI = stn$aqi
                        , DPL = ifelse(is.null(tmp <- stn$dominentpol), NA, tmp)
                        , iaqi)
      )
    )
  })


  ### metainfo ----

  nfo = do.call(plyr::rbind.fill, lapply(lst, "[[", 1))

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


  ### readings ----

  ## rbind readings
  out = do.call(plyr::rbind.fill, lapply(lst, "[[", 2))
  out = preparation(out)
  lst2 = split(out, out$IDx)

  ## optional file output
  if (!is.null(dsn)) {

    # target files
    ofl = paste0(dsn, "/", names(lst2), ".fst")

    # write to disk
    for (i in 1:length(lst2)) {

      dat = if (file.exists(ofl[i])) {
        dat = fst::read_fst(ofl[i])

        # synchronize real-time and local columns
        dat = plyr::rbind.fill(dat, lst2[[i]])
        tmp = dat[nrow(dat), ]; dat = dat[-nrow(dat), ]

        # if identical record already exists, move on to next station
        dpl = if (nrow(dat) == 0) {
          FALSE
        } else {
          # create subset with current timestamp
          sbs = subset(dat, dat$v == tmp$v)
          if (nrow(sbs) > 0) {
            sapply(1:nrow(sbs), function(j) {
              all(sbs[j, ] == tmp, na.rm = TRUE)
            })
          } else {
            FALSE
          }
        }

        if (any(dpl)) {
          next
        } else {
          dat = plyr::rbind.fill(lst2[[i]], dat)
          dat = dat[c(2:nrow(dat), 1), ]
        }
      } else {
        lst2[[i]]
      }

      fst::write_fst(dat, ofl[i], compress = 75)
    }
  }

  return(list(nfo, out))
}
