#Table includes year and data
#' @export
Cal.signal <- function (ref_start, ref_end, future_start,future_end, Table) {
  names(Table) <- c("Year","Data")
  Mean_ref<-mean(Table[Table[,1]>=ref_start & Table[,1]<=ref_end,2],na.rm=TRUE)
  Mean_future<-mean(Table[Table[,1]>=future_start & Table[,1]<=future_end,2],na.rm=TRUE)
  Signal=Mean_future-Mean_ref
  return(Signal)
}
