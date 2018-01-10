############################################################################
#' @export
cal.distance <- function(sdval) {

  dist = sqrt(sum(sdval^2))

}

# install.packages("matrixStats")
# install.packages("reshape")
# install.packages("hydroGOF")
# library(reshape)
# library(matrixStats)
# library(hydroGOF)

# dsnms = EnvList$dsnms
# prcp_idxnms = EnvList$prcp_idxnms
# temp_idxnms = EnvList$temp_idxnms
# user_idxnms = EnvList$user_idxnms
# sel_idxnms = EnvList$sel_idxnms
# sel_gcmnms = EnvList$sel_gcmnms
# prjdir = EnvList$prjdir
# stndir = EnvList$stndir
# stnfile = EnvList$stnfile
# rcpnms = EnvList$rcpnms
# syear_obs = EnvList$syear_obs
# eyear_obs = EnvList$eyear_obs
# CindexDir <- EnvList$CindexDir
# sel_dsnms <- EnvList$sel_dsnms
# DsDir <- EnvList$DsDir
# WeightDir <- EnvList$WeightDir
################################################################
#' @export
Calculate_Weight_Factor <- function( sel_dsnms, prcp_idxnms, temp_idxnms, user_idxnms, sel_idxnms, sel_gcmnms, DsDir, CindexDir, WeightDir, stndir, stnfile, rcpnms, syear_obs, eyear_obs,...){

  idxnms = c(prcp_idxnms, temp_idxnms, user_idxnms)

  idxdir = file.path(CindexDir, "Raw")
  dsnms <- sel_dsnms
  if(any(sel_idxnms %in% prcp_idxnms) & any(sel_idxnms %in% temp_idxnms)) varnms = c("prcp", "tavg")
  if(any(sel_idxnms %in% prcp_idxnms) & !any(sel_idxnms %in% temp_idxnms)) varnms = c("prcp")
  if(!any(!sel_idxnms %in% prcp_idxnms) & any(sel_idxnms %in% temp_idxnms)) varnms = c("tavg")

  ###### Get Station ID, lat, and Lon information
  setwd(stndir)
  stninfo = read.csv(stnfile, header=T)
  stninfo = stninfo[,c("ID", "Lon", "Lat")]
  stnnms = matrix(stninfo$ID)
  stncnt = length(stnnms)

  ###### OBS folder
  obsdir =file.path(DsDir, "OBS")
  obsidxdir = file.path(CindexDir, "Raw","OBS")

  cnt = 1; cnt2 = 1


  for(ii in 1:length(dsnms)){
    #for(ii in 1:1){
    dsnm = dsnms[ii]

    sddir = file.path(DsDir, dsnm)
    sdidxdir = file.path(idxdir, dsnm)
    cat(paste("process now running : ",dsnm," ",ii,"/",length(dsnms),sep=""));cat("\n")
    pb_gcm <- txtProgressBar(min = 0, max = length(sel_gcmnms), style = 3)
    for(i in 1:length(sel_gcmnms)){
      setTxtProgressBar(pb_gcm,i)
      gcmnm = sel_gcmnms[i]
      gcmdir = file.path(sddir, gcmnm)
      gcmidxdir = file.path(sdidxdir, gcmnm)

      for(k in 1:length(varnms)){
        varnm = varnms[k]

        for(j in 1:stncnt){
        #for(j in 1:2){

          stnnm = stnnms[j]

          ObsDFile = GetFilenames(obsdir, toMatch = c(stnnm))
          GcmDFile = GetFilenames(gcmdir, toMatch = c(stnnm, dsnm, gcmnm, "historical.csv"))

          if(length(ObsDFile) == 1 & length(GcmDFile) == 1){

            obs = read.csv(ObsDFile, header=T, na.strings = c("-99", -99,"NA"))
            obs = obs[which(obs$year >= syear_obs & obs$year <= eyear_obs), ]
            obs$ten = ifelse(obs$day<=10, 1, ifelse(obs$day<=20, 2, 3))
            obs$MT = sprintf("%02d-%02d", obs$mon, obs$ten)
            obs$tavg = (as.numeric(obs$tmax) + as.numeric(obs$tmin))/2

            gcm = read.csv(GcmDFile, header=T, na.strings = c("-99.00", "-99.0", "-99", -99))
            gcm$ten = ifelse(gcm$day<=10, 1, ifelse(gcm$day<=20, 2, 3))
            gcm$MT = sprintf("%02d-%02d", gcm$mon, gcm$ten)
            gcm$tavg = (gcm$tmax + gcm$tmin)/2

            #### Calculate Average and CV
            obs_tday = cast(obs, MT~year, sum, value = varnm)
            obs_avg = data.frame(rowMeans(as.matrix(obs_tday), na.rm=T))
            colnames(obs_avg) = c("Avg")
            obs_sds = data.frame(rowSds(as.matrix(obs_tday), na.rm=T))
            colnames(obs_sds) = c("Sds")
            obs_cv = obs_sds/obs_avg
            colnames(obs_cv) = c("CV")

            gcm_tday = cast(gcm, MT~year, sum, value = varnm)
            gcm_avg = data.frame(rowMeans(as.matrix(gcm_tday), na.rm=T))
            colnames(gcm_avg) = c("Avg")
            gcm_sds = data.frame(rowSds(as.matrix(gcm_tday), na.rm=T))
            colnames(gcm_sds) = c("Sds")
            gcm_cv = gcm_sds/gcm_avg
            colnames(gcm_cv) = c("CV")

            MRmse = rmse(gcm_avg, obs_avg)
            CRmse = rmse(gcm_cv, obs_cv)

            if(cnt == 1){
              out0 = cbind(dsnm, gcmnm, sprintf("%s-mean", varnm), stnnm, MRmse)
              out1 = cbind(dsnm, gcmnm, sprintf("%s-cv", varnm), stnnm, CRmse)
            } else {
              tmp0 = cbind(dsnm, gcmnm, sprintf("%s-mean", varnm), stnnm, MRmse)
              tmp1 = cbind(dsnm, gcmnm, sprintf("%s-cv", varnm), stnnm, CRmse)
              out0 = rbind(out0, tmp0)
              out1 = rbind(out1, tmp1)
            }
            cnt = cnt + 1

          } # If GCM is available

        } # Station Loop

      } # Variable Loop
      colnames(out0) = c("dsnm", "gcmnm", "varnm", "stnnm", "MRmse")
      colnames(out1) = c("dsnm", "gcmnm", "varnm", "stnnm", "CRmse")

      for(kk in 1:length(sel_idxnms)){
      #for(kk in 1:2){
        idxnm = sel_idxnms[kk]

        for(jj in 1: stncnt){
        #for(jj in 1:2){

          stnnm = stnnms[jj]

          ObsDFile = GetFilenames(obsidxdir, toMatch = c(stnnm))
          GcmDFile = GetFilenames(gcmidxdir, toMatch = c(stnnm, dsnm, gcmnm, "historical.csv"))

          if(length(ObsDFile) == 1 & length(GcmDFile) == 1){

            obs = read.csv(ObsDFile, header=T)
            obs = obs[which(obs$year >= syear_obs & obs$year <= eyear_obs), idxnm]

            gcm = read.csv(GcmDFile, header=T)
            gcm = as.numeric(gcm[which(gcm$year >= syear_obs & gcm$year <= eyear_obs), idxnm])

            #### Kolmogorov-Smirnov test p-value
            ks = ks.test(obs, gcm, exact = F)
            Simil = 1 - ks$p.value

            if(cnt2 == 1) {
              out2 = cbind(dsnm, gcmnm, idxnm, stnnm, Simil)
            } else {
              tmp = cbind(dsnm, gcmnm, idxnm, stnnm, Simil)
              out2 = rbind(out2, tmp)
            }
            cnt2 = cnt2 + 1

          }


        } # Station Loop
      } # Variable Loop


      #print(paste(i,"/",length(sel_gcmnms),"  ",dsnms[ii],sep=""))
    } # GCM Loop
    close(pb_gcm)
  } # Downscaling Loop

  # Standarize Errors
  rownames(out0) <- NULL; rownames(out1) <- NULL
  out0 = as.data.frame(out0); out1 = as.data.frame(out1); out2 = as.data.frame(out2)
  out0$MRmse = as.numeric(out0$MRmse); out1$CRmse = as.numeric(out1$CRmse); out2$Simil = as.numeric(out2$Simil)
  MRmseMn = min(out0$MRmse); MRmseMx = max(out0$MRmse)
  CRmseMn = min(out1$CRmse); CRmseMx = max(out1$CRmse)

  out0$MRmse = (out0$MRmse - MRmseMn) / (MRmseMx - MRmseMn)
  out1$CRmse = (out1$CRmse - CRmseMn) / (CRmseMx - CRmseMn)

  out0$key = paste(out0$dsnm, "_", out0$gcmnm, sep="")
  out1$key = paste(out1$dsnm, "_", out1$gcmnm, sep="")

  SimilMn = min(out2$Simil); SimilMx = max(out2$Simil)
  out2$Simil = (out2$Simil - SimilMn) / (SimilMx - SimilMn)
  out2$key = paste(out2$dsnm, "_", out2$gcmnm, sep="")

  mrdata = out0[ ,c("key", "varnm", "MRmse")]
  mrsmry = aggregate(MRmse~key+varnm, data=mrdata, FUN = mean)
  colnames(mrsmry) = c("key", "item", "sdval")

  crdata = out1[ ,c("key", "varnm", "CRmse")]
  crsmry = aggregate(CRmse~key+varnm, data=crdata, FUN = mean)
  colnames(crsmry) = c("key", "item", "sdval")

  smildata = out2[, c("key", "idxnm", "Simil")]
  simismry = aggregate(Simil~key+idxnm, data=smildata, FUN = mean)
  colnames(simismry) = c("key", "item", "sdval")

  smry = rbind(mrsmry, crsmry, simismry)

  outdir = WeightDir; SetWorkingDir(WeightDir)
  OutDFile = file.path(WeightDir, "Individual_distance_output.csv")
  write.csv(smry, OutDFile, row.names = F)

  ## Rank table for each downscaling method
  ranktable = aggregate(sdval~key, data=smry, FUN=function(z) cal.distance(z))

  ranktable$dsnm = matrix(unlist(strsplit(ranktable[,"key"], "_")), nrow=nrow(ranktable), byrow=T)[,1]
  ranktable$gcmnm = matrix(unlist(strsplit(ranktable[,"key"], "_")), nrow=nrow(ranktable), byrow=T)[,2]

  dsnms = unique(ranktable$dsnm); gcmnms = unique(ranktable$gcmnm)
  for(i_ds in 1:length(dsnms)){
    dsnm = dsnms[i_ds]
    tbl <- ranktable[which(ranktable[,"dsnm"]==dsnms[i_ds]),]
    tbl$weight <- 1/(tbl$sdval)^2
    tbl$weight <- tbl$weight / sum(tbl$weight)
    tbl$rank <- rank(tbl$sdval)
    ranktbl <- tbl[c("dsnm","gcmnm","sdval","weight","rank")]
    ranktbl <- ranktbl[order(ranktbl$rank),]
    colnames(ranktbl) = c("Downscaling","GCM","Distance","Weight","Rank")

    OutDFile = file.path(outdir, sprintf("%s_Distance_Summary.csv",dsnm))
    write.csv(ranktbl, OutDFile, row.names = F)

  }

}

