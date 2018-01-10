#' @export

Calculate.Future.Changes <- function(sel_dsnms, sel_idxnms, sel_gcmnms, rcpnms, WeightDir, CindexDir, stndir, stnfile, syear_his, eyear_his, syear_scn, eyear_scn,...){

  # sel_dsnms <- EnvList$sel_dsnms
  # sel_idxnms <- EnvList$sel_idxnms
  # sel_gcmnms <- EnvList$sel_gcmnms
  # rcpnms <- EnvList$rcpnms
  # stndir <- EnvList$stndir
  # stnfile <- EnvList$stnfile
  # syear_his <- EnvList$syear_his
  # eyear_his <- EnvList$eyear_his
  # syear_scn <- EnvList$syear_scn
  # eyear_scn <- EnvList$eyear_scn
  # RawGcmDir <- EnvList$RawGcmDir
  # CindexDir <- EnvList$CindexDir
  # SpReproDir <- EnvList$SpReproDir
  # WeightDir <- EnvList$WeightDir
  # DsDir <- EnvList$DsDir
  # WeightDir <- EnvList$WeightDir

  options(stringsAsFactors = FALSE)

  dsnms <- sel_dsnms
  gcmnms <- sel_gcmnms
  stninfo <- read.csv(file.path(stndir, stnfile), header=T)
  stnnms <- stninfo$ID
  stnnms <- c(stnnms, "Stn_Avg")
  fucnms <- future.csn(syear_scn,eyear_scn)
  fucnmsp <- paste(fucnms,"(%)",sep="")

  for(i_ds in 1:length(dsnms)){
    dsnm <- dsnms[i_ds]
  wfDF <- read.csv(file.path(WeightDir, sprintf("%s_Distance_Summary.csv", dsnm)), header=T)
  # Sorting based on GCM order
  if(length(gcmnms) == nrow(wfDF)){
    wfactor <- wfDF[match(wfDF$GCM, gcmnms), c("Weight")]
  } else {
    message("Please check number of your selected GCMs!")
  }

  idxdir <- file.path(CindexDir, "Raw", dsnm)
  idxnms <- unique(c(sel_idxnms, "prcptot", "rx1day"))


  out <- matrix(NA,length(stnnms),(length(syear_scn)*2+1))
  for(i in 1:length(rcpnms)){
    rcpnm <- rcpnms[i]
    #cat("\n");cat("index name");cat("\n")
    #pb_idx <- txtProgressBar(min = 0, max = length(idxnms), style = 3)
    for(j in 1:length(idxnms)){
      idxnm <- idxnms[j]
      #setTxtProgressBar(pb_idx,j)
      cat(paste("process now running : ",dsnm," ",i_ds,"/",length(dsnms),"  ",rcpnm," ",i,"/",length(rcpnms),
                  "  ",idxnm," ",j,"/",length(idxnms),sep=""));cat("\n")
      #cat("\n");cat("station");cat("\n")
      pb_stn <- txtProgressBar(min = 0, max = (nrow(stninfo)+1), style = 3)
      for(k in 1:(nrow(stninfo)+1)){
        setTxtProgressBar(pb_stn,k)

        stnnm <- stnnms[k]

        ## Read historical data as reference
        for(m in 1:length(gcmnms)){
          gcmnm <- gcmnms[m]

          IdxDFile <- file.path(idxdir, gcmnm, sprintf("%s_%s_%s_historical.csv", stnnm, dsnm, gcmnm))
          idx <- read.csv(IdxDFile, header=T)[idxnm]
          idx_avg <- mean(idx[, c(idxnm)],na.rm=TRUE)
          if(m == 1){
            avg_vec <- idx_avg
          } else {
            avg_vec <- c(avg_vec, idx_avg)
          }
        } #gcm loop

        idx_mme_his <- wfactor %*% avg_vec
        out[k,1] <- idx_mme_his
        ### RCP periods
        for(n in 1:length(syear_scn)){

          syear <- syear_scn[n]; eyear <- eyear_scn[n]

          for(m in 1:length(gcmnms)){
            gcmnm <- gcmnms[m]

            IdxDFile <- file.path(idxdir, gcmnm, sprintf("%s_%s_%s_%s.csv", stnnm, dsnm, gcmnm, rcpnm))
            idx <- read.csv(IdxDFile, header=T)[ c("year", idxnm)]
            idx_avg <- mean(idx[which(idx$year >= syear & idx$year <= eyear), c(idxnm)],na.rm=TRUE)
            if(m == 1){
              avg_vec <- idx_avg
            } else {
              avg_vec <- c(avg_vec, idx_avg)
            }
          }
          n1 <- n+1
          idx_mme <- wfactor %*% avg_vec
          out[k,n1] <- idx_mme
        } # future loop
      } #stn loop
    close(pb_stn)
    for(i_per in 1:length(syear_scn)){
      colnum <- i_per + length(syear_scn) + 1
      out[,colnum] <- round(((out[,(i_per+1)] / out[,1]) - 1)*100,1)
    }
    colnames(out) <- c("historical",fucnms,fucnmsp)
    rownames(out) <- stnnms
    filename <- paste(WeightDir,"/",dsnm,"_",idxnms[j],"_",rcpnms[i],"_WeightedFutureChange.csv",sep="")
    write.csv(out,filename)
    } #idx loop

  } #rcp loop

}

}
