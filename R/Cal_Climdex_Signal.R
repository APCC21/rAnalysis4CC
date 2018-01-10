#' @export
# dsnms <- EnvList$dsnms
# rcpnms <- EnvList$rcpnms
# prjdir <- EnvList$prjdir
# DsDir <- EnvList$DsDir
# sel_gcmnms <- EnvList$sel_gcmnms
# prcp_idxnms <- EnvList$prcp_idxnms
# temp_idxnms <- EnvList$temp_idxnms
# user_idxnms <- EnvList$temp_idxnms
# stndir <- EnvList$stndir
# stnfile <- EnvList$stnfile
# syear_obs <- EnvList$syear_obs
# eyear_obs <- EnvList$eyear_obs
# syear_his <- EnvList$syear_his
# eyear_his <- EnvList$eyear_his
# syear_scn <- EnvList$syear_scn
# eyear_scn <- EnvList$eyear_scn
# CindexDir <- EnvList$CindexDir
# VarNames <- EnvList$VarNames
Calculate_Cindex_Signal <- function(VarNames,sel_gcmnms,rcpnms,dsnms,prcp_idxnms,temp_idxnms,user_idxnms,prjdir,DsDir,CindexDir, stndir,stnfile,syear_obs,eyear_obs,syear_his,eyear_his,syear_scn,eyear_scn,...){

  gcmnms = sel_gcmnms

  if(sum(ifelse(VarNames %in% c("pr","tasmax","tasmin"),1,0))==3){
    idxnms <- unique(c(prcp_idxnms, temp_idxnms))
  } else if(sum(ifelse(VarNames %in% "pr",1,0))==1){
    idxnms <- unique(prcp_idxnms)
  } else if(sum(ifelse(VarNames %in% c("tasmax","tasmin"),1,0))==2){
    idxnms <- unique(temp_idxnms)
  }
  if(length(idxnms)>0){
  #setwd(stndir)
  stninfo = read.csv(file.path(stndir, stnfile), header=T)
  stninfo = stninfo[,c("ID", "Lon", "Lat")]
  stnnms = matrix(stninfo$ID)
  stncnt = length(stnnms)
  gcmcnt <- length(gcmnms)
  #stns = read.csv(paste(stndir,"/",stnfile,sep=""), header=T)
  #STN_Num <- substr(stninfo[,"ID"],3,5)

  N_CLIMDEX=length(idxnms)
  ##############list
  Index_obs_STN <- Index_CM_STN <- Index_DS_STN <- list()
  Index_Ave_obs_DM <- Index_Ave_CM_DM <- Index_Ave_DS_DM <- Index_Ave_YR_DM <- list()
  Signal_CM_DM <- Signal_DS_DM <- Index_CM_All <- Index_DS_All <- list()
  DSdir <- OutDSdir <- NA
  Futnms <- future.csn(syear_scn,eyear_scn)
      # for (i_GCMs in 1:1) { #Number_GCMs
      #    for(i_DS in 1:1){
      #      for(i_RCPs in 1:1){

  use_date <- cal.date(syear_obs,eyear_scn)

  for (i_GCMs in 1:gcmcnt) { #Number_GCMs
    for(i_DS in 1:length(dsnms)){
      for(i_RCPs in 1:length(rcpnms)){
		  DSdir[i_DS] <- file.path(DsDir, dsnms[i_DS])
        if(dir.exists(file.path(DSdir[i_DS], gcmnms[i_GCMs]))){

          OutDSdir[i_DS] <- file.path(CindexDir, "Raw", dsnms[i_DS])
          if(!dir.exists(OutDSdir[i_DS])){dir.create(OutDSdir[i_DS], showWarnings=F,recursive=T)}

          Indexdir <- file.path(CindexDir, "Raw")
          if(!dir.exists(Indexdir)){dir.create(Indexdir, showWarnings=F,recursive=T)}

          STNdir <- file.path(Indexdir, "OBS")
          if(!dir.exists(STNdir)){dir.create(STNdir, showWarnings=F,recursive=T)}

          GCMdir <- file.path(OutDSdir[i_DS], gcmnms[i_GCMs])
          if(!dir.exists(GCMdir)){dir.create(GCMdir, showWarnings=F,recursive=T)}

          for (i_STN in 1:stncnt) {
          # for (i_STN in 1:1) {
            print(paste("DownScaling Method = ",dsnms[i_DS],"  GCMNum = ",i_GCMs,"/",length(gcmnms),"  ",gcmnms[i_GCMs],
                        "  STNNum = ",i_STN,"/",stncnt,"  RCP = ",rcpnms[i_RCPs],sep=""))

            #STN_Num_text <- STN_Num[i_STN]
            STN_Num_text <- stnnms[i_STN]
            #stnnm <- stnnms[i_STN]
		      	STN_path <- paste(DsDir,"/OBS/",STN_Num_text,"_observed.csv",sep="")
            if(i_RCPs == 1 & i_GCMs == 1 & i_DS == 1){

              temp_date_obs <- use_date[min(which(use_date[,"year"]==syear_obs)):max(which(use_date[,"year"]==eyear_obs)),]
              temp_obs <-read.csv(STN_path, na.strings = c("NA","-99", -99,-999,"-999"))
              temp_obs$prcp = as.numeric(temp_obs$prcp); temp_obs$tmax = as.numeric(temp_obs$tmax); temp_obs$tmin = as.numeric(temp_obs$tmin)
              temp_obs$mon[temp_obs$mon<10] <- paste("0",temp_obs$mon[temp_obs$mon<10],sep="")
              temp_obs$day[temp_obs$day<10] <- paste("0",temp_obs$day[temp_obs$day<10],sep="")
              temp_matrix <- merge(temp_date_obs,temp_obs,by=c("year","mon","day"),all=T)
              temp_matrix <- merge(temp_date_obs,temp_matrix,by=c("year","mon","day"),na.rm=T)

              DATA_STN <- temp_matrix

              Date_STN<-paste(DATA_STN$year,DATA_STN$mon,DATA_STN$day,sep="-")
              Climdex_date_STN<-as.PCICt(Date_STN,cal="gregorian")
              assign("ci_STN", climdexInput.raw(DATA_STN$tmax,DATA_STN$tmin,DATA_STN$prcp,Climdex_date_STN,
                    Climdex_date_STN,Climdex_date_STN,base.range=c(DATA_STN[1,1]
                     ,DATA_STN[nrow(DATA_STN),1])), env=.GlobalEnv)

              #------------------------------------------------------------------------
              First_Yr_Hists <- DATA_STN[1,1]
              End_Yr_Hists <- DATA_STN[nrow(DATA_STN),1]
              Index_STN <- CLIMDEX.MAKE(idxnms,ci_STN,First_Yr_Hists,End_Yr_Hists)
              Index_obs_STN[[i_STN]] <- t(Index_STN)
              #Index_obs_STN_data_path <- paste(STNdir,"ID",STN_Num_text,"_observed.csv",sep="")
              Index_obs_STN_data_path <- paste(STNdir, "/", STN_Num_text,"_observed.csv",sep="")
              write.csv(Index_obs_STN[[i_STN]],Index_obs_STN_data_path,row.names = FALSE)
            }

            #Current_Path <- paste(DSdir[i_DS],gcmnms[i_GCMs],"/ID",STN_Num_text,"_",dsnms[i_DS],"_",
            #                      gcmnms[i_GCMs],"_historical_original.csv",sep="")
            Current_Path <- paste(DSdir[i_DS], "/", gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                  gcmnms[i_GCMs],"_historical_original.csv",sep="")
            Future_Path <- paste(DSdir[i_DS], "/", gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                 gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")

            DATA_Current <-read.csv(Current_Path, na.strings = c("NA","-99", -99,-999,"-999"))
            DATA_Current <- DATA_Current[min(which(DATA_Current[,1]==syear_his)):max(which(DATA_Current[,1]==eyear_his)),]
            DATA_Current$mon[DATA_Current$mon<10]<-paste("0",DATA_Current$mon[DATA_Current$mon<10],sep="")
            DATA_Current$day[DATA_Current$day<10]<-paste("0",DATA_Current$day[DATA_Current$day<10],sep="")

            DATA_Future <-read.csv(Future_Path, na.strings = c("NA","-99", -99,-999,"-999"))
              DATA_Future <- DATA_Future[min(which(DATA_Future[,1]==syear_scn[1])):
                                           max(which(DATA_Future[,1]==eyear_scn[length(eyear_scn)])),]

            DATA_Future$mon[DATA_Future$mon<10]<-paste("0",DATA_Future$mon[DATA_Future$mon<10],sep="")
            DATA_Future$day[DATA_Future$day<10]<-paste("0",DATA_Future$day[DATA_Future$day<10],sep="")



            temp_All_data_CM <- rbind(DATA_Current,DATA_Future)

            temp_date_cm <- temp_All_data_CM[min(which(temp_All_data_CM[,"year"]==syear_his)):
                                       max(which(temp_All_data_CM[,"year"]==eyear_scn[length(eyear_scn)])),]

              temp_matrix2 <- merge(temp_date_cm,temp_All_data_CM,by=c("year","mon","day"),all=T)
              temp_matrix2 <- merge(temp_date_cm,temp_matrix2,by=c("year","mon","day"))

            All_data_CM <- temp_matrix2
            First_Yr_Hists1 <- All_data_CM[1,1]
            End_Yr_Hists1 <- All_data_CM[nrow(All_data_CM),1]

            Date_merge<-paste(All_data_CM$year,All_data_CM$mon,All_data_CM$day,sep="-")
            Climdex_date<-as.PCICt(Date_merge,cal="gregorian")

            assign("ci_CM",climdexInput.raw(All_data_CM$tmax,All_data_CM$tmin,All_data_CM$prcp,Climdex_date,
                                            Climdex_date,Climdex_date,base.range=c(First_Yr_Hists1,End_Yr_Hists1)),env=.GlobalEnv)
            # CLIMDEX
            #----------------- GCMs -------------------------------------
            Index_CM <- CLIMDEX.MAKE(idxnms,ci_CM,First_Yr_Hists1,End_Yr_Hists1)


            DS_Current_Path <- paste(DSdir[i_DS], "/", gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_historical.csv",sep="")
            DS_Future_Path <- paste(DSdir[i_DS], "/", gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                    gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")

            DATA_DS_Current <-read.csv(DS_Current_Path, na.strings = c("NA","-99", -99,-999,"-999"))


            DATA_DS_Current <- DATA_DS_Current[min(which(DATA_DS_Current[,1]==syear_his)):max(which(DATA_DS_Current[,1]==eyear_his)),]
            DATA_DS_Current$mon[DATA_DS_Current$mon<10]<-paste("0",DATA_DS_Current$mon[DATA_DS_Current$mon<10],sep="")
            DATA_DS_Current$day[DATA_DS_Current$day<10]<-paste("0",DATA_DS_Current$day[DATA_DS_Current$day<10],sep="")

            DATA_DS_Future <- read.csv(DS_Future_Path, na.strings = c("NA","-99", -99,-999,"-999"))
              DATA_DS_Future <- DATA_DS_Future[min(which(DATA_DS_Future[,1]==syear_scn[1])):
                                                 max(which(DATA_DS_Future[,1]==eyear_scn[length(eyear_scn)])),]

            DATA_DS_Future$mon[DATA_DS_Future$mon<10]<-paste("0",DATA_DS_Future$mon[DATA_DS_Future$mon<10],sep="")
            DATA_DS_Future$day[DATA_DS_Future$day<10]<-paste("0",DATA_DS_Future$day[DATA_DS_Future$day<10],sep="")

            temp_All_data_DS <- rbind(DATA_DS_Current,DATA_DS_Future)


            temp_date_ds <- temp_All_data_DS[min(which(temp_All_data_DS[,"year"]==syear_his)):
                                       max(which(temp_All_data_DS[,"year"]==eyear_scn[length(eyear_scn)])),]

              temp_matrix3 <- merge(temp_date_ds,temp_All_data_DS,by=c("year","mon","day"),all=T)
              temp_matrix3 <- merge(temp_date_ds,temp_matrix3,by=c("year","mon","day"))

            DATA_DS <- temp_matrix3

            First_Yr_Hists2 <- DATA_DS[1,1]
            End_Yr_Hists2 <- DATA_DS[nrow(DATA_DS),1]

            Date_merge2<-paste(DATA_DS$year,DATA_DS$mon,DATA_DS$day,sep="-")
            Climdex_date2<-as.PCICt(Date_merge2,cal="gregorian")

            assign("ci_DS",climdexInput.raw(DATA_DS$tmax,DATA_DS$tmin,DATA_DS$prcp,Climdex_date2,
                                            Climdex_date2,Climdex_date2,base.range=c(First_Yr_Hists2,End_Yr_Hists2)),env=.GlobalEnv)
            #------------------------------------------------------------------------
            # CLIMDEX
            #----------------- DownScaling -------------------------------------
            Index_DS <- CLIMDEX.MAKE(idxnms,ci_DS,First_Yr_Hists2,End_Yr_Hists2)


            Index_CM_STN[[i_STN]] <- t(Index_CM)
            Index_DS_STN[[i_STN]] <- t(Index_DS)
            Index_CM_STN_Current_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                       gcmnms[i_GCMs],"_historical_original.csv",sep="")
            Index_CM_STN_Future_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                       gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")
            Index_DS_STN_Current_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                       gcmnms[i_GCMs],"_historical.csv",sep="")
            Index_DS_STN_Future_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/",STN_Num_text,"_",dsnms[i_DS],"_",
                                       gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")
            out <- Index_CM_STN[[i_STN]]
            gcmhis <- out[(min(which(out[,"year"]==syear_his)):which(out[,"year"]==eyear_his)),]
            gcmfut <- out[(min(which(out[,"year"]==syear_scn[1])):which(out[,"year"]==DATA_Future[nrow(DATA_Future),1])),]
            out <- Index_DS_STN[[i_STN]]
            dshis <- out[(min(which(out[,"year"]==syear_his)):which(out[,"year"]==eyear_his)),]
            dsfut <- out[(min(which(out[,"year"]==syear_scn[1])):which(out[,"year"]==DATA_DS_Future[nrow(DATA_DS_Future),1])),]
            write.csv(gcmhis,Index_CM_STN_Current_path,row.names = FALSE)
            write.csv(gcmfut,Index_CM_STN_Future_path,row.names = FALSE)

            out <- Index_DS_STN[[i_STN]]
            write.csv(dshis,Index_DS_STN_Current_path,row.names = FALSE)
            write.csv(dsfut,Index_DS_STN_Future_path,row.names = FALSE)

            #------------------------------------------------------------------------
          }# Stations
          temp_matrix1 <- matrix(NA,nrow=(max(eyear_his)-syear_his+1),ncol=(length(idxnms)+1))
          k <- 1

          for(i_year in syear_obs:eyear_obs){
              for(i_CLIM in 1:(N_CLIMDEX+1)){
                for(i_STN in 1:stncnt){
                  temp_data <- Index_obs_STN[[i_STN]]
                  wh <- which(temp_data[,"year"]==i_year)
                  if(i_STN==1){
                  temp_row <- temp_data[wh,i_CLIM]
                  } else {
                    temp_row <- c(temp_row,temp_data[wh,i_CLIM])
                  }
              } #stn loop
                temp_matrix1[k,i_CLIM] <- mean(temp_row,na.rm=T)
              }
            k <- k + 1
          }
          colnames(temp_matrix1) <- colnames(Index_CM_STN[[i_STN]])
          Index_Ave_obs_DM[[i_DS]] <- temp_matrix1
          Index_Ave_CM_DM[[i_DS]] <- Reduce("+",Index_CM_STN)/length(Index_CM_STN)
          Index_Ave_DS_DM[[i_DS]] <- Reduce("+",Index_DS_STN)/length(Index_DS_STN)
          Index_Ave_YR_DM[[i_DS]] <- cbind(c(First_Yr_Hists1:End_Yr_Hists1),c(First_Yr_Hists2:End_Yr_Hists2))
          if(length(eyear_scn)==3){
          if(End_Yr_Hists1 >= eyear_scn[3]){
            Number_Col1 <- 3
          }}
          if(length(eyear_scn)==2){
          if(End_Yr_Hists1 >= eyear_scn[2]){
            Number_Col1 <- 2
          }}
          if(length(eyear_scn)==1){
          if(End_Yr_Hists1 >= eyear_scn[1]){
            Number_Col1 <- 1
          }}
          if(length(eyear_scn)==3){
          if(End_Yr_Hists2 >= eyear_scn[3]){
            Number_Col2 <- 3
          }}
          if(length(eyear_scn)==2){
          if(End_Yr_Hists2 >= eyear_scn[2]){
            Number_Col2 <- 2
          }}
          if(length(eyear_scn)==1){
          if(End_Yr_Hists2 >= eyear_scn[1]){
            Number_Col2 <- 1
          }}
          Signal_CM_DM[[i_DS]] <- matrix(NA,N_CLIMDEX,Number_Col1)
          Signal_DS_DM[[i_DS]] <- matrix(NA,N_CLIMDEX,Number_Col2)
          for(i_CLIM in 1:N_CLIMDEX){
            Clim <- i_CLIM + 1
            Index_CM_All[[i_CLIM]] <- cbind(c(First_Yr_Hists1:End_Yr_Hists1),Index_Ave_CM_DM[[i_DS]][,Clim])
            Index_DS_All[[i_CLIM]] <- cbind(c(First_Yr_Hists2:End_Yr_Hists2),Index_Ave_DS_DM[[i_DS]][,Clim])

            for(i_futscn in 1:Number_Col1){
              if(End_Yr_Hists1 >= eyear_scn[i_futscn]){
                Signal_CM_DM[[i_DS]][i_CLIM,i_futscn] <- Cal.signal(syear_his,eyear_his,syear_scn[i_futscn],
                                                                    eyear_scn[i_futscn],Index_CM_All[[i_CLIM]])
              }
            }
            for(i_futscn in 1:Number_Col2){
              if(End_Yr_Hists1 >= eyear_scn[i_futscn]){
                Signal_DS_DM[[i_DS]][i_CLIM,i_futscn] <- Cal.signal(syear_his,eyear_his,syear_scn[i_futscn],
                                                                    eyear_scn[i_futscn],Index_DS_All[[i_CLIM]])
              }
            }
          } #CLIMDEX

          Index_Ave_obs_path <- paste(STNdir, "/Stn_Avg_observed.csv",sep="")
          write.csv(Index_Ave_obs_DM[[i_DS]],Index_Ave_obs_path,row.names = FALSE)

          Index_Ave_CM_Current_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_historical_original.csv",sep="")
          Index_Ave_CM_Future_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")
          Index_Ave_DS_Current_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_historical.csv",sep="")
          Index_Ave_DS_Future_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")
          out <- Index_Ave_CM_DM[[i_DS]]
          write.csv(out[(min(which(out[,"year"]==syear_his)):which(out[,"year"]==eyear_his)),],Index_Ave_CM_Current_path,row.names = FALSE)
          write.csv(out[(min(which(out[,"year"]==syear_scn[1])):which(out[,"year"]==DATA_Future[nrow(DATA_Future),1])),],
                    Index_Ave_CM_Future_path,row.names = FALSE)

          out <- Index_Ave_DS_DM[[i_DS]]
          write.csv(out[(min(which(out[,"year"]==syear_his)):which(out[,"year"]==eyear_his)),],Index_Ave_DS_Current_path,row.names = FALSE)
          write.csv(out[(min(which(out[,"year"]==syear_scn[1])):which(out[,"year"]==DATA_DS_Future[nrow(DATA_DS_Future),1])),],
                    Index_Ave_DS_Future_path,row.names = FALSE)

          Signal_CM_DM_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")
          Signal_DS_DM_path <- paste(OutDSdir[i_DS],"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")
          gcmsig <- cbind(idxnms,Signal_CM_DM[[i_DS]])
          dssig <- cbind(idxnms,Signal_DS_DM[[i_DS]])
          colnames(gcmsig) <- c("Index",Futnms)
          colnames(dssig) <- c("Index",Futnms)
          write.csv(gcmsig,Signal_CM_DM_path,row.names=FALSE)
          write.csv(dssig,Signal_DS_DM_path,row.names=FALSE)

        } #GCM TRUE
        #print(proc.time()-ptm1)
    } #rcp loop
    } #dsnm loop
  } #gcm loop
} # if idxnms true
}

