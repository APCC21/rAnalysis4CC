## Newly coustomed variogram final main program
#
# rm(list=ls())
# #working.directory <- "C:\\Users\\jowonil\\Documents\\variogramR\\"; setwd(working.directory)
# packages <- c("ggplot2", "dplyr", "gstat", "sp", "lattice", "chron", "zoo","rgdal", "RColorBrewer", "svMisc", "Hmisc")
# if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#   install.packages(setdiff(packages, rownames(installed.packages())))
# }
#
# lapply(packages, require, character.only = TRUE)
# theme_set(theme_bw())
# theme_update(legend.position='top')
# source("DATA_read.R")	# Prepare Data nicely
# source("plotting.R")	# Contains customized graphical effects
# source("customized_vrg.R") # offers a customized "gstatVariogram", "data.frame"
# source("New_Customized_Vrg.R")
# #Output_path <- "C:\\Users\\jowonil\\Documents\\variogramR\\Output\\"
# cls()

# DsDir <- EnvList$DsDir
# SpReproDir <- EnvList$SpReproDir
# stndir <- EnvList$stndir
# stnfile <- EnvList$stnfile
# idxdir <- EnvList$idxdir
# VarNames <- EnvList$VarNames
# syear_obs <- EnvList$syear_obs
# eyear_obs <- EnvList$eyear_obs
# syear_his <- EnvList$syear_his
# eyear_his <- EnvList$eyear_his
# sel_gcmnms <- EnvList$sel_gcmnms
# dsnms <- EnvList$dsnms
Spatial_Reproducibility <- function(DsDir,SpReproDir, stndir,stnfile,dsnms,VarNames,sel_gcmnms,syear_obs,eyear_obs,syear_his,eyear_his,...){

  gcmnms = sel_gcmnms

  ## Convert VarNames to varnms
  if("pr" %in% VarNames) variovar <- c("prcp")
  if("tasmax" %in% VarNames) variovar <- c(variovar, "tmax")
  if("tasmin" %in% VarNames) variovar <- c(variovar, "tmin")

  stns = read.csv(paste(stndir,"/",stnfile,sep=""), header=T)

  if(nrow(stns) >= 2) {

    if(!dir.exists(SpReproDir)){dir.create(SpReproDir, showWarnings=F,recursive=T)}
    if(!dir.exists(paste(SpReproDir,"/data",sep=""))){dir.create(paste(SpReproDir,"/data",sep=""), showWarnings=F,recursive=T)}
    if(!dir.exists(paste(SpReproDir,"/graph",sep=""))){dir.create(paste(SpReproDir,"/graph",sep=""), showWarnings=F,recursive=T)}
    Plot_OBS <- array(NA,c(choose(length(stns[,1]),2),2,length(variovar)))
    Plot_matrix <- list()
#0124
    for(i_dsn in 1:length(dsnms)){

      #models <- list.dirs(paste(prjdir,"/",dsnms[i_dsn],sep=""), full.names = FALSE, recursive = FALSE)
      #RCMs <- c("HadGEM3-RA_v1", "RegCM_v4", "SNU-MM5_v3")
      #gcmnms <- setdiff(models,RCMs)
      #gcmnms <- models
      Plot_matrix[[i_dsn]] <- array(NA,c(choose(length(stns[,1]),2),2,length(gcmnms),length(variovar)))
      # dim(length X, 2var(Y,X), GCM, dsnm, variovar)
      #GCMs

      dsnm <- dsnms[i_dsn]
      if(!dir.exists(paste(SpReproDir,"/data/",dsnm,sep=""))){
        dir.create(paste(SpReproDir,"/data/",dsnm,sep=""))}
      for(i in 1:length(variovar)){
        if(!dir.exists(paste(SpReproDir,"/data/",dsnm,"/",variovar[i],sep=""))){
          dir.create(paste(SpReproDir,"/data/",dsnm,"/",variovar[i],sep=""))}
        for(i_GCMs in 1:length(gcmnms)){
          print(paste("Downscaling Name=",dsnms[i_dsn]," ",i_dsn,"/",length(dsnms)," ",variovar[i]," ",i,"/",
                      length(variovar)," GCM=",gcmnms[i_GCMs]," ",i_GCMs,"/",length(gcmnms),sep=""))
          fileName <- paste(SpReproDir,"/data/",dsnm,"/",variovar[i],"/",gcmnms[i_GCMs],"_",variovar[i],".txt",sep="")
          #	print(fileName)
          data <- custom_vrg_GCM(DsDir,gcmnms[i_GCMs],variovar[i],stns,dsnm)

          write.table(data, fileName, row.names = FALSE)
          Plot_matrix[[i_dsn]][,,i_GCMs,i] <- data
        } # end of for(i_GCMs in 1:length(GCMs))

        PRCP_Obs_data <- custom_vrg_Obs(variovar[i], DsDir,stns,syear_obs,eyear_obs)
        write.table(PRCP_Obs_data,
                    paste(SpReproDir,"/data/",dsnm,"/",variovar[i],"/","observed_",variovar[i],".txt",sep=""),
                    row.names = FALSE)
        Plot_OBS[,,i] <- PRCP_Obs_data
      }
    } # dsnms

    sum_matrix <- all_matrix <- list()
    for(i in 1:length(variovar)){
      filenms <- paste(SpReproDir,"/graph/",variovar[i],"_variogram.png",sep="")
      print(paste("Make Variogram ",variovar[i],sep=""))
      for(i_dsn in 1:length(dsnms)){
        for(i_GCMs in 1:length(gcmnms)){
          if(i_GCMs == 1){
            sum_matrix[[i_dsn]] <- data.frame(Plot_matrix[[i_dsn]][,,i_GCMs,i],gcmnms[i_GCMs],dsnms[i_dsn])
          } else {
            sum_matrix[[i_dsn]] <- rbind(sum_matrix[[i_dsn]] ,
                                         data.frame(Plot_matrix[[i_dsn]][,,i_GCMs,i],gcmnms[i_GCMs],dsnms[i_dsn]))
          }
        }
        if(i_dsn == 1){
          all_matrix[[i]] <- sum_matrix[[i_dsn]]
        } else {
          all_matrix[[i]] <- rbind(all_matrix[[i]],sum_matrix[[i_dsn]])
        }
      }

      OBS_matrix <- cbind(Plot_OBS[,,i],"OBS","OBS")
      colnames(OBS_matrix) <- c("Distance","Gamma","GCMNames","DownscalingName")
      colnames(all_matrix[[i]]) <- c("Distance","Gamma","GCMNames","DownscalingName")
      all_matrix[[i]] <- rbind(all_matrix[[i]],OBS_matrix)
      all_matrix[[i]][,1] <- as.numeric(all_matrix[[i]][,1])
      all_matrix[[i]][,2] <- as.numeric(all_matrix[[i]][,2])
      all_temp <- all_matrix[[i]]
      for(ii in 1:length(unique(all_temp[,"DownscalingName"]))){
        if(ii==1){
          wh <- which(all_temp[,"DownscalingName"]=="OBS")
          line_matrix <- all_temp[wh,]
        } else {
          wh <- which(all_temp[,"DownscalingName"]==dsnms[(ii-1)])
          temp <- all_temp[wh,]
          for(j in 1:length(which(all_temp[,"DownscalingName"]=="OBS"))){
            wh1 <- which(temp[,1]==all_temp[which(all_temp[,"DownscalingName"]=="OBS")[j],1])
            temp2 <- cbind(all_temp[which(all_temp[,"DownscalingName"]=="OBS")[j],1],mean(temp[wh1,2]),
                           dsnms[(ii-1)],dsnms[(ii-1)])
            if(j == 1){
              temp3 <- temp2
            } else {
              temp3 <- rbind(temp3,temp2)
            }
          }
          colnames(temp3) <- colnames(line_matrix)
          line_matrix <- rbind(line_matrix,temp3)
        }
      } #loop dsnm + obs
      line_matrix[,"Distance"] <- as.numeric(line_matrix[,"Distance"])
      line_matrix[,"Gamma"] <- as.numeric(line_matrix[,"Gamma"])

      ggfile <- paste(SpReproDir,"/graph/",dsnms[i_dsn],"Variogram_",variovar[i],".png",sep="")
      gg <- ggplot(all_matrix[[i]],aes(Distance,Gamma)) +
        geom_point(aes(color=DownscalingName),alpha=1/5,pch=16) +
        scale_y_continuous(limit=c(0,(as.numeric(apply(all_matrix[[i]],2,max)[2])+1))) +
        scale_x_continuous(limit=c((as.numeric(apply(all_matrix[[i]],2,min)[1])-2),(as.numeric(apply(all_matrix[[i]],2,max)[1])+2)))+
        geom_line(data=line_matrix,aes(x=Distance,y=Gamma,color=DownscalingName)) +
        ggtitle(paste("Variogram ",variovar[i])) + theme(plot.title=element_text(size=15,hjust=0.5))
      if(nrow(stns)>10){
        ggsave(plot = gg,width=20,height=10,dpi=300,filename = ggfile)
      } else if(nrow(stns)<=10){
        ggsave(plot = gg,filename=ggfile)
      }
    }
  } else {
    message("Station number should be greater than 1")
  } # Station number should be greater than 1

}

# #######ploting variogram############
# colnms <- c("pink","skyblue","seagreen1","plum")
# colnms1 <- c("red","blue","green","orchid4")
# sum_matrix <- list()
# suma_matrix <-  array(NA,c(choose(length(stns[,1]),2),2,length(dsnms),length(variovar)))
# for(i in 1:length(variovar)){
#   filenms <- paste(analdir,"/04_SpatialReproducibility/graph/",variovar[i],"_variogram.png",sep="")
#   for(i_dsn in 1:length(dsnms)){
#     for(i_GCMs in 1:length(gcmnms)){
#       if(i_GCMs == 1){
#       sum_matrix[[i_dsn]] <- Plot_matrix[[i_dsn]][,,i_GCMs,i]
#       } else {
#         sum_matrix[[i_dsn]] <- rbind(sum_matrix[[i_dsn]] ,Plot_matrix[[i_dsn]][,,i_GCMs,i])
#       }
#     }
#     #png(filename=filenms)
#     header <- paste("Variogram ",variovar[i],sep="")
#     if(i_dsn == 1){
#       plot(sum_matrix[[i_dsn]][,1],sum_matrix[[i_dsn]][,2],col = colnms[i_dsn],pch=19,main=header,
#            xlab="Distance",ylab="Gamma")
#     } else {
#       points(sum_matrix[[i_dsn]][,1],sum_matrix[[i_dsn]][,2],col = colnms[i_dsn],pch=19)
#     }
#     Plot_DS <- matrix(NA,choose(length(stns[,1]),2),2)
#     for(j in 1:choose(length(stns[,1]),2)){
#       whi <- which(sum_matrix[[i_dsn]][,1]==Plot_OBS[j,1,i])
#       Plot_DS[j,1] <- Plot_OBS[j,1,i]
#       Plot_DS[j,2] <- mean(sum_matrix[[i_dsn]][whi,2])
#     }
#     lines(Plot_DS[,1],Plot_DS[,2],col=colnms1[i_dsn])
#   }
#   lines(Plot_OBS[,1,i],Plot_OBS[,2,i])
#   legend(min(Plot_OBS[,1,i]),(max(Plot_OBS[,2,i])+1),c("OBS","SQM","SDQDM"),col=c("black","red","orchid4"),lty=1)
#   #dev.off()
#
# }
