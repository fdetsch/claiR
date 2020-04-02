#' Preprocess WAQI Data
#'
#' @description
#' Preprocess WAQI data, ie. declare missing values \code{NA}, reformat hourly
#' time offsets (sometimes ':' is not included, which is required by connected
#' functions, e.g. from \strong{lubridate}), and remove duplicate rows.
#'
#' @param x WAQI data as \code{data.frame}.
#'
#' @return
#' An adjusted \code{data.frame}.
#'
#' @export preparation
#' @name preparation
preparation = function(x) {

  ## introduce 'NA' in aqi and dpl
  columns = c("AQI", "DPL"); na_strings = c("-", "")
  for (i in 1:length(columns)) {
    ids = x[, columns[i]] == na_strings[i] | is.na(x[, columns[i]])
    if (any(ids)) {
      x[ids, columns[i]] = NA
    }
  }

  ## reformat time offsets (sometimes ':' is missing, lubridate cannot handle that)
  ids = !grepl(":", x$tz)
  if (any(ids)) {
    tmp = sapply(x$tz[ids], function(i) {
      if (!is.na(i)) {
        nc = nchar(i)
        paste(substr(i, 1, nc - 2), substr(i, nc - 1, nc), sep = ":")
      } else NA
    })
    x$tz[ids] = tmp
  }

  ## correct incorrect epoch entries
  datetime = as.POSIXct(x$s, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  ids = (tmp <- as.integer(datetime)) != x$v | is.na(x$v)
  if (any(ids)) {
    x$v[ids] = tmp[ids]
  }

  ## remove duplicated rows
  ids_tz = grep("tz", names(x)) # 'tz' is sometimes broken, so drop it when searching for duplicates
  dpl = duplicated(x[, -ids_tz])
  x = x[!dpl, ]

  return(x)
}
