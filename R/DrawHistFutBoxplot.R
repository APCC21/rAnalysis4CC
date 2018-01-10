#' @export
# syear_his=EnvList$syear_his
# eyear_his=EnvList$eyear_his
# syear_scn=EnvList$syear_scn
# eyear_scn=EnvList$eyear_scn
# dsnms <- EnvList$dsnms
# rcpnms <- EnvList$rcpnms
# sel_gcmnms <- EnvList$sel_gcmnms
# prcp_idxnms <- EnvList$prcp_idxnms
# temp_idxnms <- EnvList$temp_idxnms
# user_idxnms <- EnvList$user_idxnms
# prjdir <- EnvList$prjdir
# CindexDir <- EnvList$CindexDir
# syear_obs <- EnvList$syear_obs
# eyear_obs <- EnvList$eyear_obs


Draw_Hist_Fut_Cindex_Graph <- function(syear_obs,eyear_obs,syear_his,eyear_his,syear_scn,eyear_scn,dsnms,rcpnms,sel_gcmnms,prcp_idxnms,temp_idxnms,user_idxnms,CindexDir,...){

  gcmnms = sel_gcmnms
  idxnms = c(prcp_idxnms, temp_idxnms, user_idxnms)

  TF_GCM <- length(gcmnms)
  N_CLIMDEX <- length(idxnms)
  CLIMDEX.ALL <- Read.Climdex.Data (gcmnms,rcpnms,dsnms,idxnms,CindexDir)
  Rep=as.numeric(eyear_obs)-as.numeric(syear_obs)+1

  name.dsnm <- name.dsnms1 <- name.dsnms2 <- name.dsn <- OBS <- list()
  DataBox <- name.gcms <- name.gcm <- list()
  Value.GCM <- Value.GCMs1 <- Value.GCMs2 <- list()

  DataBox1 <- futvar <- futdsn <- futgcm <- futrcp <- futcat <- list()
  futvar1 <- futdsn1 <- futgcm1 <- futrcp1 <- futcat1 <- list()
  futvar2 <- futdsn2 <- futgcm2 <- futrcp2 <- futcat2 <- list()
  futvar3 <- futdsn3 <- futgcm3 <- futrcp3 <- futcat3 <- list()
  futvar4 <- futdsn4 <- futgcm4 <- futrcp4 <- futcat4 <- list()
  futurescn <- future.csn(syear_scn,eyear_scn)

  for(i_CLIM in 1:N_CLIMDEX){
    colnames(CLIMDEX.ALL$Index_Ave_obs[[1]][[1]][[1]]) <-
      c("year","prcptot","cdd","cwd","r95ptot","r99ptot","rx1day","rx5day","sdii","rnnmm","r10mm","r20mm","su","id","txn","txx","tx10p",
        "tx90p","wsdi","fd","tr","tnn","tnx","tn10p","tn90p","csdi","dtr","gsl")
    OBS[[i_CLIM]] <- CLIMDEX.ALL$Index_Ave_obs[[1]][[1]][[1]][1:(eyear_his-syear_his+1),idxnms[i_CLIM]]
    name.dsn[[i_CLIM]] <- rep("OBS",each=Rep)
    name.gcm[[i_CLIM]] <- rep("OBS",each=Rep)

    for (i_GCMs in 1:TF_GCM) {
      for(i_DS in 1:length(dsnms)){
        SYear <- which(CLIMDEX.ALL$Index_Ave_DS_hist[[i_GCMs]][[i_DS]][,1]==syear_his)
        EYear <- which(CLIMDEX.ALL$Index_Ave_DS_hist[[i_GCMs]][[i_DS]][,1]==eyear_his)

        Value2 <- CLIMDEX.ALL$Index_Ave_DS_hist[[i_GCMs]][[i_DS]][SYear:EYear,idxnms[i_CLIM]]
        X2 <- rep(dsnms[[i_DS]],each=Rep)

        Value.GCMs2[[i_DS]] <- Value2
        name.dsnms2[[i_DS]] <- X2
      }####loop downscaling
      Value.GCM[[i_GCMs]] <- unlist(Value.GCMs2)
      name.dsnm[[i_GCMs]] <- unlist(name.dsnms2)
      name.gcms[[i_GCMs]] <- rep(gcmnms[i_GCMs],each=Rep*length(dsnms))
    }###loop GCM

    OBS[[i_CLIM]] <- c(OBS[[i_CLIM]],unlist(Value.GCM))
    name.dsn[[i_CLIM]] <- c(name.dsn[[i_CLIM]],unlist(name.dsnm))
    name.gcm[[i_CLIM]] <- c(name.gcm[[i_CLIM]],unlist(name.gcms))

    CATEX.Tmp <- c(rep("OBS",each=Rep),rep("Downscaling",each=Rep*length(dsnms)*TF_GCM))

    DataBox[[i_CLIM]] <- data.frame(OBS[[i_CLIM]],name.dsn[[i_CLIM]],name.gcm[[i_CLIM]],CATEX.Tmp)
    names(DataBox[[i_CLIM]]) <- c("value","dsnms","gcmnms","Cate")
    DataBox[[i_CLIM]]$Cate_f <- factor(DataBox[[i_CLIM]]$Cate, levels=c('OBS','Downscaling'))
    #boxdir <- CindexDir
    if(!dir.exists(CindexDir)){dir.create(CindexDir, showWarnings=F,recursive=T)}
    boxhistdir = file.path(CindexDir, "Historical")
    if(!dir.exists(boxhistdir)){dir.create(boxhistdir, showWarnings=F,recursive=T)}
    fname = paste(CindexDir,"/Historical/",idxnms[i_CLIM], "_Historical", ".png", sep="")

    if(sum(ifelse(is.na(DataBox[[i_CLIM]][,1])==TRUE,1,0))>=1){
      wh <- which(is.na(DataBox[[i_CLIM]])==TRUE)
      for(i in wh){
        print(paste("check NA in ",idxnms[i_CLIM],"_",DataBox[[i_CLIM]][i,2],"_",DataBox[[i_CLIM]][i,3],sep=""))
      }
      DataBox[[i_CLIM]] <- DataBox[[i_CLIM]][-wh,]
    }


    gg.boxplot.hist(DataBox[[i_CLIM]],fname,Rep,idxnms[i_CLIM])

  } #loop climdex

  gg.boxplot.fut(syear_his,eyear_his,syear_scn,eyear_scn,dsnms,rcpnms,gcmnms,idxnms,CindexDir)

}

