#' @export
Output <- list()
# gcmnms <- EnvList$gcmnms
# dsnms <- EnvList$dsnms
# prjdir <- EnvList$prjdir
# VarNames <- EnvList$VarNames
# syear_obs <- EnvList$syear_obs
# eyear_obs <- EnvList$eyear_obs
# stndir <- EnvList$stndir
# stnfile <- EnvList$stnfile
# DsDir <- EnvList$DsDir

Evaluate_Raw_Gcms <- function(gcmnms,dsnms,prjdir,DsDir, RawGcmDir,VarNames,stndir,stnfile,syear_obs,eyear_obs,...){

  if(length(gcmnms) >= 2){
    for(i_DS in 1:length(dsnms)){

      dsnm <- dsnms[1] #### Only use first Downscaling method (SQM)

      mdlnms <- gcmnms

      ## Convert VarNames to varnms
      if("pr" %in% VarNames) varnms <- c("prcp")
      if("tasmax" %in% VarNames & "tasmin" %in% VarNames) varnms <- c(varnms, "tavg")

      #
      stninfo = read.csv(file.path(stndir, stnfile), header=T)
      stninfo = stninfo[,c("ID", "Lon", "Lat")]
      stnnms = matrix(stninfo$ID)
      stncnt = length(stnnms)
      varcnt = length(varnms)

      #analdir <- paste(prjdir,"/Analysis",sep="")
      RawGcmDir <- paste(prjdir,"/RawGcmAnalysis",sep="")

      if(!dir.exists(RawGcmDir)){dir.create(RawGcmDir, showWarnings=F,recursive=T)}

      ####### Temporal and Spatial Correlation Coefficients
      #tem[mdlcnt,stncnt,varcnt]
      tem <- GCM.Rank.temporal(gcmnms,dsnm,varnms,prjdir,DsDir, stndir,stnfile,syear_obs,eyear_obs)
      spa <- GCM.Rank.spatial(gcmnms,dsnm,varnms,prjdir,DsDir,stndir,stnfile,syear_obs,eyear_obs)

      if(all(is.na(spa))){
        if(stncnt >= 2){
          Rate <- cbind((apply(tem[,,3],1,sum,na.rm=T)/ncol(tem[,,3])),
                        (apply(tem[,,4],1,sum,na.rm=T)/ncol(tem[,,4])))
          Corr <- cbind(apply(tem[,,1],1,mean,na.rm=T),
                        apply(tem[,,2],1,mean,na.rm=T))
          CR <- cbind(apply(tem[,,5],1,mean,na.rm=T),
                      apply(tem[,,6],1,mean,na.rm=T))
        } else {
          Rate <- cbind(tem[,,3], tem[,,4])
          Corr <- cbind(tem[,,1], tem[,,2])
          CR <- cbind(tem[,,5], tem[,,6])
        }
        colnames(Rate) <- c("tem_PrcpRate","tem_TempRate")
        colnames(Corr) <- c("tem_Prcp","tem_Temp")
        colnames(CR) <- c("tem_CR","tem_CR")

        Sample <- cbind(Corr[,"tem_Prcp"],Rate[,"tem_PrcpRate"],
                        Corr[,"tem_Temp"],Rate[,"tem_TempRate"])
        colnames(Sample) <- c("tem_Prcp","tem_PrcpRate","tem_Temp","tem_TempRate")

        #Rate1,Corr1,Rate2=NULL,Corr2=NULL
        Rank1 <- Rank.method(Rate1 = Sample[,"tem_PrcpRate"],Corr1 = Sample[,"tem_Prcp"],Rate2=NULL,Corr2=NULL)
        Rank2 <- Rank.method(Rate1 = Sample[,"tem_TempRate"],Corr1 = Sample[,"tem_Temp"],Rate2=NULL,Corr2=NULL)
        Rank_Total <- rank(Rank1+Rank2,ties.method = "random")

        Out<- data.frame(Corr[,1],Rate[,1],Rank1,Corr[,2],Rate[,2],Rank2,Rank_Total)
        colnames(Out) <- c("tem_Prcp","tem_PrcpRate","prcp_Rank","tem_Temp","tem_TempRate","temp_Rank","Rank_Total")
        Out1<- data.frame(Corr[,1],Rate[,1],Rank1)
        colnames(Out1) <- c("tem_Prcp","tem_PrcpRate","prcp_Rank")
        Out2<- data.frame(Corr[,2],Rate[,2],Rank2)
        colnames(Out2) <- c("tem_Temp","tem_TempRate","temp_Rank")
      } else {
        Rate <- cbind((apply(tem[,,3],1,sum,na.rm=T)/ncol(tem[,,3])),
                      (apply(tem[,,4],1,sum,na.rm=T)/ncol(tem[,,4])),
                      (apply(spa[,,3],1,sum,na.rm=T)/ncol(spa[,,3])),
                      (apply(spa[,,4],1,sum,na.rm=T)/ncol(spa[,,4])))
        colnames(Rate) <- c("tem_PrcpRate","tem_TempRate","Spa_PrcpRate","Spa_TempRate")
        Corr <- cbind(apply(tem[,,1],1,mean,na.rm=T),
                      apply(tem[,,2],1,mean,na.rm=T),
                      apply(spa[,,1],1,mean,na.rm=T),
                      apply(spa[,,2],1,mean,na.rm=T))
        colnames(Corr) <- c("tem_Prcp","tem_Temp","Spa_Prcp","Spa_Temp")
        CR <- cbind(apply(tem[,,5],1,mean,na.rm=T),
                    apply(tem[,,6],1,mean,na.rm=T),
                    apply(spa[,,5],1,mean,na.rm=T),
                    apply(spa[,,6],1,mean,na.rm=T))
        colnames(CR) <- c("tem_CR","tem_CR","spa_CR","spa_CR")

        Sample <- cbind(Corr[,"Spa_Prcp"],Rate[,"Spa_PrcpRate"],Corr[,"tem_Prcp"],Rate[,"tem_PrcpRate"],
                        Corr[,"Spa_Temp"],Rate[,"Spa_TempRate"],Corr[,"tem_Temp"],Rate[,"tem_TempRate"])
        colnames(Sample) <- c("Spa_Prcp","Spa_PrcpRate","tem_Prcp","tem_PrcpRate","Spa_Temp","Spa_TempRate","tem_Temp","tem_TempRate")

        Rank1 <- Rank.method(Sample[,"Spa_PrcpRate"],Sample[,"Spa_Prcp"],Sample[,"tem_PrcpRate"],Sample[,"tem_Prcp"])
        Rank2 <- Rank.method(Sample[,"Spa_TempRate"],Sample[,"Spa_Temp"],Sample[,"tem_TempRate"],Sample[,"tem_Temp"])
        Rank_Total <- rank(Rank1+Rank2,ties.method = "random")

        Out<- data.frame(Corr[,3],Rate[,3],Corr[,1],Rate[,1],Rank1,Corr[,4],Rate[,4],Corr[,2],Rate[,2],Rank2,Rank_Total)
        colnames(Out) <- c("Spa_Prcp","Spa_PrcpRate","tem_Prcp","tem_PrcpRate","prcp_Rank","Spa_Temp","Spa_TempRate","tem_Temp","tem_TempRate","temp_Rank","Rank_Total")
        Out1<- data.frame(Corr[,3],Rate[,3],Corr[,1],Rate[,1],Rank1)
        colnames(Out1) <- c("Spa_Prcp","Spa_PrcpRate","tem_Prcp","tem_PrcpRate","prcp_Rank")
        Out2<- data.frame(Corr[,4],Rate[,4],Corr[,2],Rate[,2],Rank2)
        colnames(Out2) <- c("Spa_Temp","Spa_TempRate","tem_Temp","tem_TempRate","temp_Rank")
      }


      ### QUESTIONS
      for(i in 1:nrow(Out)){
        wh1 <- which(Out[,"Rank_Total"]==i)
        wh2 <- which(Out1[,"prcp_Rank"]==i)
        wh3 <- which(Out2[,"temp_Rank"]==i)
        if(i == 1){
          Rank_total <- Out[wh1,]
          Rank_prcp <- Out1[wh2,]
          Rank_tavg <- Out2[wh3,]
        } else {
          Rank_total <- rbind(Rank_total,Out[wh1,])
          Rank_prcp <- rbind(Rank_prcp,Out1[wh2,])
          Rank_tavg <- rbind(Rank_tavg,Out2[wh3,])
        }
      }

      Rank_path_total <- paste(RawGcmDir,"/","Raw_GCM_Rank_Both(Prcp-Temp).csv",sep="")
      Rank_path_prcp <- paste(RawGcmDir,"/","Raw_GCM_Rank_precipitation.csv",sep="")
      Rank_path_tavg <- paste(RawGcmDir,"/","Raw_GCM_Rank_temperature.csv",sep="")

      write.csv(Rank_total,Rank_path_total)
      write.csv(Rank_prcp,Rank_path_prcp)
      write.csv(Rank_tavg,Rank_path_tavg)

      plot_path <- paste(RawGcmDir,"/Raw_GCM_Rank_Both(Prcp-Temp).png",sep="")
      Plot_rank <- data.frame(Rank_total[,"prcp_Rank"],Rank_total[,"temp_Rank"],Rank_total[,"Rank_Total"],rownames(Rank_total))
      colnames(Plot_rank) <- c("prcp_Rank","temp_Rank","Rank_Total","GCM_names")

      GCM <- c()
      for(i in 1:length(gcmnms)){
        GCM[i] <- paste("(",Plot_rank[i,"Rank_Total"],") ",Plot_rank[i,"GCM_names"],sep="")
      }
      Plot_rank <- data.frame(Plot_rank,GCM)
      # if(length(gcmnms)<4){
      #   brks <- seq(1,length(gcmnms),1)
      # } else if(length(gcmnms)==5){
      #   brks <- c(1,3,5)
      # } else if(length(gcmnms) < 10){
      #   brks <- c(1,round(median(1:length(gcmnms)),0),length(gcmnms))
      # } else if(length(gcmnms) < 19){
      #   brks <- seq(1,length(gcmnms),3)
      #   brks <- c(brks[-(length(brks))],length(gcmnms))
      # } else {
      #   brks <- seq(1,length(gcmnms),4)
      #   brks <- c(brks[-(length(brks))],length(gcmnms))
      # }
      brks <- c(1,length(gcmnms))

      gg <- ggplot(Plot_rank, aes(x=factor(temp_Rank),y=factor(prcp_Rank)
                                  , color= GCM)) + geom_point(pch=21)
      gg <-	gg + geom_text(aes(y=prcp_Rank-0.2,x=temp_Rank+0.4, label=Plot_rank[,"Rank_Total"]),size=5,vjust=0, show.legend=FALSE) +
        geom_abline(intercept=0,slope=1,color="darkred") +
        theme(plot.title=element_text(size=15,hjust=0.5),legend.position="bottom",legend.text=element_text(size=10)) +
        scale_x_discrete("Temporal Correlation Rank",breaks=factor(brks)) +
        scale_y_discrete("Spatial Correlation Rank",breaks=factor(brks)) +
        ggtitle("Raw GCM Multiple Rank")
      #plot(gg)
      ggsave(plot_path,gg, width = 9, height = 10, dpi = 100)

    } # Loop for
  } else {
    message("Raw GCM analysis requires more than two GCMs!")
  }


}
