#' @export
# syear_his <- EnvList$syear_his
# eyear_his <- EnvList$eyear_his
# syear_scn <- EnvList$syear_scn
# eyear_scn <- EnvList$eyear_scn
# sel_dsnms <- EnvList$sel_dsnms
# rcpnms <- EnvList$rcpnms
# sel_gcmnms <- EnvList$sel_gcmnms
# prcp_idxnms <- EnvList$prcp_idxnms
# temp_idxnms <- EnvList$temp_idxnms
# sel_idxnms <- EnvList$sel_idxnms
# prjdir <- EnvList$prjdir
# WeightDir <- EnvList$WeightDir
# CindexDir <- EnvList$CindexDir

Draw_Uncertainty_Range <- function(syear_his,eyear_his,syear_scn,eyear_scn,sel_dsnms,rcpnms,sel_gcmnms,prcp_idxnms,temp_idxnms,sel_idxnms,prjdir,CindexDir,WeightDir,...){

  if(length(sel_gcmnms) >= 2){
    fixed_idxnms = c("rx1day", "prcptot")

    outdir = WeightDir
    if(!dir.exists(outdir)){dir.create(outdir)}
    climdir <- file.path(CindexDir, "Raw")
    futcsn <- future.csn(syear_scn,eyear_scn)
    idxnms <- c(prcp_idxnms,temp_idxnms)
    gcmnms <- sel_gcmnms
    dsnms <- sel_dsnms

    #load data (Signal after bias-correction)
    result <- array(NA,c(length(dsnms),length(rcpnms),length(futcsn),length(gcmnms),length(idxnms)))
    for(i_ds in 1:length(dsnms)){
      for(i_GCMs in 1:length(gcmnms)){
        for(i_RCPs in 1:length(rcpnms)){
          out1dir <- paste(climdir,"/",dsnms[i_ds],"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_ds],"_",gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")
          out1 <- read.csv(file = out1dir,header=TRUE)[-1]
          for(i_fut in 1:length(futcsn)){
            result[i_ds,i_RCPs,i_fut,i_GCMs,] <- as.numeric(out1[,i_fut])
          }#future csn
        } #rcp loop
      } #gcm loop
    } #downscaling loop

    #####find select index, Plus 1 is for overall summary
    wh1 <- NA
    fig_idxnms <- unique(c(sel_idxnms, fixed_idxnms))
    for(i in 1:length(fig_idxnms)){
      wh1[i] <- which(idxnms==fig_idxnms[i])
    }
    wh_Num <- length(wh1)+1

    for(i_ds in 1:length(dsnms)){

      InDFile <- file.path(WeightDir, sprintf("%s_Distance_Summary.csv", dsnms[i_ds]))
      ranktbl <- read.csv(InDFile, header = T)

      #boxplot
      Ranked.matrix <- Ranked.Percentage <- matrix(NA,length(gcmnms),length(fig_idxnms))
      Index.matrix <- Index.Percentage <- matrix(NA,length(gcmnms),1)
      N.Pr.90 <- NA
      number <- seq(1,length(gcmnms),1)

      for(i_selidx in 1:wh_Num){

        if(i_selidx < wh_Num){

          pngname <- file.path(outdir, sprintf("%s_%s_uncertainty.pdf", dsnms[i_ds], fig_idxnms[i_selidx]))
          pdf(pngname)
          par(mfrow=c(length(rcpnms),length(eyear_scn)))
          for(i_RCPs in 1:length(rcpnms)){
            for(i_fut in 1:length(futcsn)){
              #result <- (dsnms,rcpnms,futcsn,gcmnms,idxnms)
              temp <- result[i_ds,i_RCPs,i_fut,,wh1[i_selidx]]
              main_name <- paste(rcpnms[i_RCPs]," ",futcsn[i_fut],sep="")
              for(i in 1:length(gcmnms)) {
                rank_gcmnm = ranktbl[i, "GCM"]
                gcm_index = which(gcmnms == rank_gcmnm)
                Index.matrix[i,1]<-temp[gcm_index]
              }
              for(i_GCMs in 1:length(gcmnms)){
                if(i_GCMs==1){
                  GCM.kkz <- Index.matrix[1:2,1]
                } else {
                  GCM.kkz <- Index.matrix[1:i_GCMs,1]
                }
                Max_value<-apply(Index.matrix,2,max)
                Min_value<-apply(Index.matrix,2,min)

                Max_value.kkz<-max(GCM.kkz)
                Min_value.kkz<-min(GCM.kkz)
                range.GCM20<-Max_value-Min_value
                range.kkz<-Max_value.kkz-Min_value.kkz

                Index.Percentage[i_GCMs]<-range.kkz/range.GCM20*100
              } #gcm loop

              #Pr.mean<-apply(Percentage,1,mean)  # Percentage fo ECTTDI indices with > 90 %
              plot(number,Index.Percentage,xlab="Number of GCMs",ylab="% of explanation (local range/total range)",
                   col="blue",pch=19,main=main_name,ylim=c(0,100))
              lines(number,Index.Percentage,col="blue")
              abline(h=80,col="red",lty=3,lwd=3)
              if(min(which(Index.Percentage>80)) == 1){
                N.maximum <- 2
              } else {
                N.maximum <- min(which(Index.Percentage>80))
              }
              abline(v=N.maximum,col="red",lty=3,lwd=3)
              #N.maximum <- min(which(Index.Percentage>80))
              Name <- paste("N=",N.maximum,sep="")
              text(N.maximum[1]+0.5,50,labels=c(Name))
            } #future loop
          } #rcp loop
          dev.off()

        } else {
          pngname <- file.path(outdir, sprintf("%s_overall_uncertainty.pdf", dsnms[i_ds]))
          pdf(pngname)
          par(mfrow=c(length(rcpnms),length(eyear_scn)))


          for(i_RCPs in 1:length(rcpnms)){
            for(i_fut in 1:length(futcsn)){
              #result <- (dsnms,rcpnms,futcsn,gcmnms,idxnms)
              #wh1 is list of locations(rownumber) of user defined index
              temp <- scale(result[i_ds,i_RCPs,i_fut,,wh1])
              main_name <- paste(rcpnms[i_RCPs]," ",futcsn[i_fut],sep="")
              for(i in 1:length(gcmnms)) {
                rank_gcmnm = ranktbl[i, "GCM"]
                gcm_index = which(gcmnms == rank_gcmnm)
                Ranked.matrix[i,]<-temp[gcm_index,]   #Change the period for relative changes
              }
              for(i_GCMs in 1:length(gcmnms)){
                if(i_GCMs==1){
                  GCM.kkz <- Ranked.matrix[1:2,]
                } else {
                  GCM.kkz <- Ranked.matrix[1:i_GCMs,]
                }
                Max_value<-apply(Ranked.matrix,2,max)
                Min_value<-apply(Ranked.matrix,2,min)

                Max_value.kkz<-apply(GCM.kkz,2,max)
                Min_value.kkz<-apply(GCM.kkz,2,min)
                range.GCM20<-Max_value-Min_value
                range.kkz<-Max_value.kkz-Min_value.kkz

                Ranked.Percentage[i_GCMs,]<-range.kkz/range.GCM20*100
              } #gcm loop

              Pr.mean<-apply(Ranked.Percentage,1,mean)  # Percentage fo ECTTDI indices with > 90 %
              plot(number,Pr.mean,xlab="Number of GCMs",ylab="% of explanation (local range/total range)",
                   col="blue",pch=19,main=main_name,ylim=c(0,100))
              lines(number,Pr.mean,col="blue")
              abline(h=80,col="red",lty=3,lwd=3)
              if(min(which(Pr.mean>80))==1){
                N.maximum <- 2
              } else {
              N.maximum <- min(which(Pr.mean>80))
              }
              abline(v=N.maximum,col="red",lty=3,lwd=3)
              Name <- paste("N=",N.maximum,sep="")
              text(N.maximum[1]+0.5,50,labels=c(Name))
            } #future loop
          } #rcp loop
          dev.off()

        } #if else
      }#sel_idx loop
    } #ds loop
  } else {
    message("Uncertainty Range analysis requires more than two selected GCMs!")
  }




}
