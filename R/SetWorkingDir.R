#' @export
SetWorkingDir <- function(wdir) {

  # Creat working dir if not exists
  dir.create(wdir, showWarnings=F,recursive=T)
  setwd(wdir)

}
