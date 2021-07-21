#' ---
#' title: "Update WAQI Station Inventory"
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
library(raster)
library(claiR)


### STATION RETRIEVAL ====

## target spatial extent
deu = getData(
  country = "DEU"
  , level = 2
  , path = "inst/extdata" # tmpDir()
)

## retrieve stations per state
stn = lapply(
  1:length(deu)
  , function(i) {
    tmp = try(
      inventory(
        deu[i, ]
        , token = keyring::key_get("waqi_api")
      )
      , silent = TRUE
    )
    
    if (!inherits(tmp, "try-error") && nrow(tmp) > 0) {
      return(tmp)
    }
  }
)

## remove NULL entries
stn = mapedit:::combine_list_of_sf(
  Filter(
    Negate(
      is.null
    )
    , stn
  )
)

stn = stn[, -2]

## if new stations are introduced, add to local inventory
ofl_stn = "inst/extdata/deu-stations.rds"
stn_bu = readRDS(ofl_stn)
if (any(navl <- !stn$uid %in% stn_bu$uid)) {
  cat("New WAQI stations found, adding them to package built-in dataset.\n\n")
  
  stn = rbind(stn_bu, stn[navl, ])
  saveRDS(stn, ofl_stn)
  print(stn)
  
  WAQIstations = stn
  save(
    WAQIstations
    , file = "data/WAQIstations.rda"
    , compress = "bzip2"
  )
} else {
  cat("No new WAQI stations found, going to sleep...\n")
}

## status message
cat("Run finished at", format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z"), "\n")

#'
#' ### ZZ. Final things last
#'
#' <details><summary>Session info (click to view)</summary>
devtools::session_info()
#' </details>
