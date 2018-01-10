#' @export
GCM.Rank.temporal <- function(gcmnms,dsnm,varnms,prjdir,DsDir,stndir,stnfile,syear_obs,eyear_obs,...){

  ###### Get Station ID, lat, and Lon information
  # gcmnms <- EnvList$gcmnms
  # dsnm <- EnvList$dsnms[1]
  # prjdir <- EnvList$prjdir
  # stndir <- EnvList$stndir
  # syear_obs <- EnvList$syear_obs
  # eyear_obs <- EnvList$eyear_obs
  # stnfile <- EnvList$stnfile
  # varnms <- varnms
  # dsnms <- EnvList$dsnms
  # DsDir <- EnvList$DsDir
  setwd(stndir)
  stninfo = read.csv(stnfile, header=T)
  stninfo = stninfo[,c("ID", "Lon", "Lat")]
  stnnms = matrix(stninfo$ID)
  stncnt = length(stnnms)
  sddir = file.path(DsDir, dsnm)

  ###### Get model names
  # mdlnms = list.dirs(sddir, recursive = F)
  # mdlmatrix = matrix(unlist(strsplit(mdlnms, "/")), nrow=length(mdlnms), byrow=T)
  # mdlnms = mdlmatrix[,ncol(mdlmatrix)]
  mdlnms = gcmnms
  mdlcnt = length(mdlnms)
  varcnt <- length(varnms)

  Out <- Rate <- CRtotal <- array(NA,c(mdlcnt,stncnt,varcnt))
  Output <- array(NA,c(mdlcnt,stncnt,(3*varcnt)))
  for(i in 1:mdlcnt){

    mdlnm = mdlnms[i]
    mdldir = file.path(sddir, mdlnm)

    for(j in 1: stncnt){

      stnnm = stnnms[j]

      # if(BC_mode == F){
      #   ObsDFile = GetFilenames(obsdir, toMatch = c(stnnm))
      #   GcmDFile = GetFilenames(mdldir, toMatch = c(stnnm, dsnm, mdlnm, "historical", "original"))
      # } else {
      #   ObsDFile = GetFilenames(obsdir, toMatch = c(stnnm))
      #   GcmDFile = GetFilenames(mdldir, toMatch = c(stnnm, dsnm, mdlnm, "historical"))
      #  }

      ObsDFile = GetFilenames(paste(DsDir,"/OBS",sep=""), toMatch = c(stnnm))
      GcmDFile = paste(mdldir,"/",stnnm,"_" ,dsnm,"_", mdlnm, "_historical_original.csv",sep="")

      gcm = read.csv(GcmDFile, header=T)
      gcm$tavg = (gcm$tmax + gcm$tmin)/2
      gcm$ten = ifelse(gcm$day<=10, 1, ifelse(gcm$day<=20, 2, 3))
      gcm$MT = sprintf("%02d-%02d", gcm$mon, gcm$ten)

      obs = read.csv(ObsDFile, header=T, na.strings = c(-99, "-99","NA"))
      #colnames(obs) <- c("year","mon","day","prcp","tmax","tmin","wspd","rhum","rsds")
      obs = obs[which(obs$year >= syear_obs & obs$year <= eyear_obs), ]
      obs$tavg = (as.numeric(obs$tmax) + as.numeric(obs$tmin))/2
      obs$ten = ifelse(obs$day<=10, 1, ifelse(obs$day<=20, 2, 3))
      obs$MT = sprintf("%02d-%02d", obs$mon, obs$ten)

      for(k in 1:varcnt){

        varnm = varnms[k]

        obs_tday = aggregate(get(varnm) ~ MT, data = obs, FUN = sum)
        colnames(obs_tday) = c("MT", "obs")
        #obs_tday = cast(obs, MT~year, sum, value = varnm)
        #obs_avg = data.frame(rowMeans(as.matrix(obs_tday), na.rm=T))

        gcm_tday = aggregate(get(varnm) ~ MT, data = gcm, FUN = sum)
        colnames(gcm_tday) = c("MT", "gcm")
        #gcm_tday = cast(gcm, MT~year, sum, value = varnm)
        #gcm_avg = data.frame(rowMeans(as.matrix(gcm_tday), na.rm=T))

        # Critical R value based on number of samples
        CR = critical.r(nrow(obs_tday))

        cor = cor(obs_tday[c("obs")], gcm_tday[c("gcm")], method="pearson")
        Out[i,j,k] <- cor
        Rate[i,j,k] <- ifelse(cor>=CR,1,0)
        CRtotal[i,j,k] <- CR
      }
    }
  }
  for(i in 1:varcnt){
    j <- i+2
	  k <- i+4
    Output[,,i] <- Out[,,i]
    Output[,,j] <- Rate[,,i]
	  Output[,,k] <- CRtotal[,,i]
  }
	rownames(Output) <- mdlnms
  colnames(Output) <- stnnms[1:stncnt]

  return(Output)
}
