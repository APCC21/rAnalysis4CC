#' @export
GCM.Rank.spatial <- function(gcmnms, dsnm,varnms,prjdir,DsDir,stndir,stnfile,syear_obs,eyear_obs,...){

  ###### Get Station ID, lat, and Lon information
  # gcmnms <- EnvList$gcmnms
  # dsnm <- EnvList$dsnms[1]
  # varnms <- EnvList$VarNames
  # prjdir <- EnvList$prjdir
  # stndir <- EnvList$stndir
  # syear_obs <- EnvList$syear_obs
  # eyear_obs <- EnvList$eyear_obs
  # stnfile <- EnvList$stnfile
  # dsnms <- EnvList$dsnms

  stndir1 <- paste(stndir,"/",stnfile,sep="")
  stninfo = read.csv(stndir1, header=T)
  stninfo = stninfo[,c("ID", "Lon", "Lat")]
  stnnms = matrix(stninfo$ID)
  stncnt = length(stnnms)
  sddir = file.path(DsDir, dsnm)

  ###### Get model names
  mdlnms = gcmnms
  mdlcnt = length(mdlnms)
  varcnt <- length(varnms)

  Out <- Rate <- CRtotal <- array(NA,c(mdlcnt,36,varcnt))
  Output <- array(NA,c(mdlcnt,36,(3*varcnt)))
  s <- NA
  for(i in 1:12){
    for(j in 1:3){
      sss <- ifelse(i<=9,paste("0",i,sep=""),i)
      ss <- paste(sss,"-0",j,sep="")
      s <- c(s,ss)
    }
  }


  # If station number is less than 3, skip and put NAs
  if(stncnt >= 3){
    for(i in 1:mdlcnt){

      mdlnm = mdlnms[i]
      mdldir = file.path(sddir, mdlnm)

      for(k in 1:varcnt){

        varnm = varnms[k]

        for(j in 1: stncnt){

          stnnm = stnnms[j]

          ObsDFile = GetFilenames(paste(DsDir,"/OBS",sep=""), toMatch = c(stnnm))
          GcmDFile = paste(mdldir,"/",stnnm,"_" ,dsnm,"_", mdlnm, "_historical_original.csv",sep="")

          obs = read.csv(ObsDFile, header=T, na.strings = c(-99, "-99","NA") )
          #colnames(obs) <- c("year","mon","day","prcp","tmax","tmin","wspd","rhum","rsds")
          obs = obs[which(obs$year >= syear_obs & obs$year <= eyear_obs), ]
          obs$tavg = (as.numeric(obs$tmax) + as.numeric(obs$tmin))/2
          obs$ten = ifelse(obs$day<=10, 1, ifelse(obs$day<=20, 2, 3))
          obs$MT = sprintf("%02d-%02d", obs$mon, obs$ten)

          gcm = read.csv(GcmDFile, header=T)
          gcm$tavg = (gcm$tmax + gcm$tmin)/2
          gcm$ten = ifelse(gcm$day<=10, 1, ifelse(gcm$day<=20, 2, 3))
          gcm$MT = sprintf("%02d-%02d", gcm$mon, gcm$ten)

          if(j == 1){
            obs_tday = aggregate(get(varnm) ~ MT, data = obs, FUN = sum)
            colnames(obs_tday) = c("MT", stnnm)

            gcm_tday = aggregate(get(varnm) ~ MT, data = gcm, FUN = sum)
            colnames(gcm_tday) = c("MT", stnnm)
          } else {
            obs_imsi = aggregate(get(varnm) ~ MT, data = obs, FUN = sum)
            colnames(obs_imsi) = c("MT", stnnm)
            obs_tday = merge(obs_tday, obs_imsi)

            gcm_imsi = aggregate(get(varnm) ~ MT, data = gcm, FUN = sum)
            colnames(gcm_imsi) = c("MT", stnnm)
            gcm_tday = merge(gcm_tday, gcm_imsi)
          }

        } # Station Loop

        rnms = obs_tday$MT
        # Transpose data frame
        obs_stn = as.data.frame(t(obs_tday[,-1]))
        colnames(obs_stn) = rnms

        gcm_stn = as.data.frame(t(gcm_tday[,-1]))
        colnames(gcm_stn) = rnms

        # Critical R value based on number of samples
        CR = critical.r(nrow(obs_stn))

        ### QUESTIONS
        for(mt in 1:ncol(obs_stn)){
          mtnm = colnames(obs_stn)[mt]
          if(nrow(unique(gcm_stn[c(mtnm)]))==1){
            for(istn in 1:nrow(obs_stn)){
              gcm_stn[istn,c(mtnm)] <- gcm_stn[istn,c(mtnm)] + (istn/10000)
            }
          } else {
            gcm_stn[c(mtnm)] <- gcm_stn[c(mtnm)]
          }
          cor = cor(obs_stn[c(mtnm)], gcm_stn[c(mtnm)], method="pearson")
          Out[i,mt,k] <- cor
          Rate[i,mt,k] <- ifelse(cor>=CR,1,0)
          CRtotal[i,mt,k] <- CR
        }
      } # Variable Loop
    } # GCM Loop
    for(i in 1:varcnt){
      Output[,,i] <- Out[,,i]
      j <- i+2
      Output[,,j] <- Rate[,,i]
      k <- i+4
      Output[,,k] <- CRtotal[,,i]
    }
  }

  rownames(Output) <- mdlnms
  colnames(Output) <- s[2:37]
  return(Output)
}
