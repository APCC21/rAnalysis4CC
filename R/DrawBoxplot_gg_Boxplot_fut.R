#' @export
gg.boxplot.fut <- function(syear_his,eyear_his,syear_scn,eyear_scn,dsnms,rcpnms,gcmnms,idxnms,CindexDir,...){

  climdir <- paste(CindexDir,"/Raw",sep="")
	futcsn <- future.csn(syear_scn,eyear_scn)
  for(i_ds in 1:length(dsnms)){
  	for(i_GCMs in 1:length(gcmnms)){
  		for(i_RCPs in 1:length(rcpnms)){
  			out1dir <- paste(climdir,"/",dsnms[i_ds],"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_ds],"_",gcmnms[i_GCMs],"_",rcpnms[i_RCPs],"_original.csv",sep="")
  			out2dir <- paste(climdir,"/",dsnms[i_ds],"/",gcmnms[i_GCMs],"/Signal_",dsnms[i_ds],"_",gcmnms[i_GCMs],"_",rcpnms[i_RCPs],".csv",sep="")
  			out1 <- read.csv(file = out1dir,header=TRUE)
  			out2 <- read.csv(out2dir,header=TRUE)
  			col_len <- ncol(out1)
  			outt1 <- out1[,2:col_len]
  			col_len <- ncol(out2)
  			outt2 <- out2[,2:col_len]
  			out <- outt1 - outt2
  			for(i_fut in 1:length(futcsn)){
  				temp <- cbind(out[,i_fut],dsnms[i_ds],gcmnms[i_GCMs],rcpnms[i_RCPs],futcsn[i_fut],idxnms)
  				if(i_ds==1 & i_GCMs==1 & i_RCPs==1 & i_fut==1){
  					result <- temp
  					} else {
  					result <- rbind(result,temp)
  					}
  			}#future csn
  		} #rcp loop
  	} #gcm loop
  } #downscaling loop
  colnames(result) <- c("Error","Downscaling","GCMName","RCP","Future","Index")
  futdir <- file.path(CindexDir,"Future")
  if(!dir.exists(futdir)){dir.create(futdir)}
  for(i_idx in 1:length(idxnms)){
  	idxnm <- idxnms[i_idx]
  	fname <- paste(CindexDir,"/Future/",idxnms[i_idx],"_Future.png",sep="")

  	wh <- which(result[,"Index"]==idxnms[i_idx])
  	X <- data.frame(result[wh,])
    X[,"Error"] <- as.numeric(X[,"Error"])
  	gg.boxplot(X,fname,idxnm)
  }
}

