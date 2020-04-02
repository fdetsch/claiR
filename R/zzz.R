.onLoad <- function(lib, pkg) {

  ## Orcs (adapted from https://github.com/MatMatt/MODIS/blob/master/R/zzz.R)
  avl <- length(find.package("Orcs", quiet = TRUE)) > 0

  if (!avl) {
    devtools::install_github("fdetsch/Orcs")
  }
}
