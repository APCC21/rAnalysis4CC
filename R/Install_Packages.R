#' @export
Install_Required_Packages  <- function( ) {

  required.packages = c("yaml", "ggplot2", "PCICt", "climdex.pcic", "reshape", "matrixStats", "gstat", "hydroGOF", "gsubfn")

  new.packge = required.packages[!(required.packages %in% installed.packages()[,"Package"])]

  if(length(new.packge) > 0) install.packages(new.packge)

  # Load packages
  lapply(required.packages, require, character.only = TRUE)

}
