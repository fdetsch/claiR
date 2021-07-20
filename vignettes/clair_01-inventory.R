#' ---
#' title: '**claiR**: WAQI Station Retrieval for Marburg, Germany'
#' author: "`r Sys.getenv('USERNAME')`"
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
#+ libs, message=FALSE
library(claiR)
library(mapview)


#' ### **1 Coarse geolocation query: Hesse**
### . hesse ----

de1 = raster::getData(country = "DEU", level = 1, path = raster::tmpDir())
hsn = subset(de1, NAME_1 == "Hessen")

stn1 = inventory(hsn, token = keyring::key_get("waqi_api"))
cat("# stations in Marburg:", sum(grepl("Marburg", stn1$station$name)))
m1 = mapview(stn1)
m1


#' ### **2 Fine geolocation query: Marburg-Biedenkopf**
### . marburg-biedenkopf ----

de2 = raster::getData(country = "DEU", level = 2, path = raster::tmpDir())
mrb = subset(de2, grepl("Marburg", NAME_2))

stn2 = inventory(mrb, token = keyring::key_get("waqi_api"))
cat("# stations in Marburg:", nrow(stn2))
m2 = mapview(stn2)
m2


#'
#' ### ZZ. Final things last
#'
#' <details><summary>Session info (click to view)</summary>
devtools::session_info()
#' </details>
