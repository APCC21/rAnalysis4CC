#' @export
Read.Climdex.Data <- function(gcmnms,rcpnms,dsnms,idxnms,CindexDir,...){
 # dsnms <- EnvList$dsnms
 # rcpnms <- EnvList$rcpnms
 # prjdir <- EnvList$prjdir
 # gcmnms <- EnvList$gcmnms
 # idxnms <- EnvList$idxnms

Index_Ave_obs_RCP <- Index_Ave_CM_RCP <- Index_Ave_DS_RCP <- Signal_CM_RCP <- Signal_DS_RCP <- list()
Index_Ave_CM_hist <- Index_Ave_DS_hist <- Index_Ave_CM_hists <- Index_Ave_DS_hists <- list()
Index_Ave_obs_DS <- Index_Ave_CM_DS <- Index_Ave_DS_DS <- Signal_CM_DS <- Signal_DS_DS <- list()
Index_Ave_obs <- Index_Ave_CM <- Index_Ave_DS <- Signal_CM <- Signal_DS <- list()

  TF_GCM <- length(gcmnms)
  N_CLIMDEX=nrow(idxnms)

STNdir <- paste(CindexDir,"/Raw/OBS/",sep="")
Index_Ave_obs_path <- paste(STNdir,"stn_Avg_observed.csv",sep="")
Index_Ave_obs_DM <- read.csv(Index_Ave_obs_path)
for(i_GCMs in 1:TF_GCM){
	for(i_DS in 1:length(dsnms)){
		OutDSdir <- paste(CindexDir,"/Raw/",dsnms[i_DS],sep="")
		GCMdir <- paste(OutDSdir,"/",gcmnms[i_GCMs],sep="")
		if(dir.exists(GCMdir)){
		  Index_Ave_CM_path_hist <- paste(OutDSdir,"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
		                             gcmnms[i_GCMs],"_historical_original.csv",sep="")
		  Index_Ave_DS_path_hist <- paste(OutDSdir,"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
		                             gcmnms[i_GCMs],"_historical.csv",sep="")
		  Index_Ave_CM_hist[[i_DS]] <- read.csv(Index_Ave_CM_path_hist)
		  Index_Ave_DS_hist[[i_DS]] <- read.csv(Index_Ave_DS_path_hist)
		for(i_RCPs in 1:length(rcpnms)){

          Index_Ave_CM_path <- paste(OutDSdir,"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")
          Index_Ave_DS_path <- paste(OutDSdir,"/",gcmnms[i_GCMs],"/Stn_Avg_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")
          Signal_CM_DM_path <- paste(OutDSdir,"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")
          Signal_DS_DM_path <- paste(OutDSdir,"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_DS],"_",
                                     gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")

			Index_Ave_obs_RCP[[i_RCPs]] <- Index_Ave_obs_DM
			Index_Ave_CM_RCP[[i_RCPs]] <- read.csv(Index_Ave_CM_path)
			Index_Ave_DS_RCP[[i_RCPs]] <- read.csv(Index_Ave_DS_path)
			Signal_CM_RCP[[i_RCPs]] <- read.csv(Signal_CM_DM_path)
			Signal_DS_RCP[[i_RCPs]] <- read.csv(Signal_DS_DM_path)
		} #RCP loop
		} #GCM dir TRUE
	  Index_Ave_obs_DS[[i_DS]] <- Index_Ave_obs_RCP
    Index_Ave_CM_DS[[i_DS]] <- Index_Ave_CM_RCP
    Index_Ave_DS_DS[[i_DS]] <- Index_Ave_DS_RCP
    Signal_CM_DS[[i_DS]] <- Signal_CM_RCP
    Signal_DS_DS[[i_DS]] <- Signal_DS_RCP
	} #DS loop
	  Index_Ave_obs[[i_GCMs]] <- Index_Ave_obs_DS
	  Index_Ave_CM_hists[[i_GCMs]] <- Index_Ave_CM_hist
	  Index_Ave_DS_hists[[i_GCMs]] <- Index_Ave_DS_hist
    Index_Ave_CM[[i_GCMs]] <- Index_Ave_CM_DS
    Index_Ave_DS[[i_GCMs]] <- Index_Ave_DS_DS
    Signal_CM[[i_GCMs]] <- Signal_CM_DS
    Signal_DS[[i_GCMs]] <- Signal_DS_DS
} #GCM loop

  CLIMDEX_Result <- list()
  CLIMDEX_Result[[1]] <- Index_Ave_obs
  CLIMDEX_Result[[2]] <- Index_Ave_CM_hists
  CLIMDEX_Result[[3]] <- Index_Ave_DS_hists
  CLIMDEX_Result[[4]] <- Index_Ave_CM
  CLIMDEX_Result[[5]] <- Index_Ave_DS
  CLIMDEX_Result[[6]] <- Signal_CM
  CLIMDEX_Result[[7]] <- Signal_DS

  names(CLIMDEX_Result) <- c("Index_Ave_obs","Index_Ave_CM_hist","Index_Ave_DS_hist","Index_Ave_CM","Index_Ave_DS","Signal_CM","Signal_DS")
  return(CLIMDEX_Result)
 }
