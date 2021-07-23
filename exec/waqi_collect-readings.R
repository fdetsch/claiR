#' ---
#' title: "Collect Real-Time WAQI Readings"
#' author: "Florian Detsch, <fdetsch@web.de>"
#' output:
#'   html_notebook:
#'     code_folding: show
#'     toc: true
#'     toc_depth: 2
#'     toc_float: true
#'     collapsed: false
#' ---
#+ setup, include=FALSE
knitr::opts_chunk[['set']](collapse=FALSE, message=FALSE, warning=FALSE, prompt=FALSE)
#+ libs, echo=FALSE

## status message
cat("Run started at", format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"), "\n\n")


### ENVIRONMENT ====

## packages
library(claiR)

## cores available for parallelization
nds = parallel::detectCores() - 1L

## target folder
odr = ... # TODO: DuckDB solution?
Orcs::ifMissing(odr, invisible, dir.create, arg1 = "path", recursive = TRUE)


### DATA RETRIEVAL ====

### . readings ----

stn = readRDS("inst/extdata/deu-stations.rds")
rtv = try(
  R.utils::withTimeout(
    retrieval(
      stn
      , dsn = odr
      , names = nds
      , token = keyring::key_get("waqi_api")
    )
    , onTimeout = "error"
    , timeout = 25 * 60
  )
  , silent = TRUE
)


### . metainfo ----

nfo = rtv[[1]]

## if new metainformation is introduced, add to local inventory
if (!inherits(rtv, "try-error")) {
  ofl_nfo = "inst/extdata/deu-stations-metainfo.rds"
  nfo_bu = readRDS(ofl_nfo)
  if (any(navl <- !nfo$IDx %in% nfo_bu$IDx)) {
    nfo = plyr::rbind.fill(nfo_bu, nfo[navl, ])
    saveRDS(nfo, ofl_nfo)
  }
}

## status message
cat("Run finished at", format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"), "\n")

#'
#' ### ZZ. Final things last
#'
#' <details><summary>Session info (click to view)</summary>
devtools::session_info()
#' </details>
