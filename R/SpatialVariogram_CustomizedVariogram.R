# New_Customized_Vrg.R
# Referred to a matlab program, lots of built-in logics are re-written.
#' @export
F_variogram <- function(date1, date2, data1_, data2_, location1, location2)
{
## TO DO.
}

#' @export
Gamma <- function(vec1, vec2)
{
	value <- sum(((vec1 - mean(vec1)) - (vec2 - mean(vec2)))^2)/length(vec1)
	value
}

#' @export
Distance <- function(loc1, loc2)
{
	value <- sqrt(((loc1[1] - loc2[1])*92)^2 + ((loc1[2] - loc2[2])*114)^2)
	value
}
#Station.All$STN_Name[1]

#' @export
custom_vrg_GCM <- function(DsDir,gcmnm, Variable,stns,dsnm,...)
{
	# Usage :
	# custom_vrg( "HadGEM3-RA_v1", "PRCP")
	#cat(paste(Model, " variogram step.\n", sep=""))
	result <- matrix(0, nrow = 1, ncol = 2 )

	for(i in 1:(length(stns[,1])-1)) {
		for(j in seq(i+1, length(stns[,1]), by = 1)) {
		  #cat(paste("\nStation : ", Station_Info$STN_Name[i], " and Station : ", Station_Info$STN_Name[j], " Under Calculation...\n", sep=""))
		  tmp1.dir <- paste(DsDir,"/",dsnm,"/",gcmnm,"/",stns[i,"ID"],"_",dsnm,"_",gcmnm,"_historical.csv",sep="")
		  tmp1 <- read.csv(tmp1.dir,header = TRUE)
		  tmp2.dir <- paste(DsDir,"/",dsnm,"/",gcmnm,"/",stns[j,"ID"],"_",dsnm,"_",gcmnm,"_historical.csv",sep="")
		  tmp2 <- read.csv(tmp2.dir,header = TRUE)

			location1 <- c(stns[i,"Lon"],stns[i,"Lat"])
			location2 <- c(stns[j,"Lon"],stns[j,"Lat"])
			distance_and_gamma <- c( Distance(location1, location2), Gamma(tmp1[,Variable], tmp2[,Variable]))
			result <- rbind(result, distance_and_gamma)
			#cat(paste("Station : ", Station_Info$STN_Name[i], " and Station : ", Station_Info$STN_Name[j], " Calculation done...\n", sep=""))
		} # end of for(j in i+1:length(Station.All[,1]))
	} # end of for(i in 1:length(Station.All[,1]))

	final <- result[-1,]
	colnames(final) <- c("distance", "gamma")
	rownames(final) <- NULL
	final
} # end of custom_vrg(Model, Variable)


#' @export
# custom_vrg_RCM <- function(Model, Variable)
# {
# 	# Usage :
# 	# custom_vrg( "HadGEM3-RA_v1", "PRCP")
# 	cat(paste(Model, " variogram step.\n", sep=""))
# 	result <- matrix(0, nrow = 1, ncol = 2 )
#
# 	for(i in 1:(length(Station.All[,1])-1))
# 	{
# 		for(j in seq(i+1, length(Station.All[,1]), by = 1))
# 		{
# 			cat(paste("\nStation : ", Station_Info$STN_Name[i], " and Station : ", Station_Info$STN_Name[j], " Under Calculation...\n", sep=""))
# 			tmp1 <- Read_DATA_Historical_RCM(Path = "C:\\Users\\jowonil\\Desktop\\DATA\\",
#      				   	      Model_name = Model,
# 					      rcp = "rcp45",
# 				      	STN_Num =  Station.All$STN_Num[i],
# 			      		Method = "QDM",
# 						Variables = c("TMAX", "TMIN", "PRCP"))
# 			tmp2 <- Read_DATA_Historical_RCM(Path = "C:\\Users\\jowonil\\Desktop\\DATA\\",
#      					   	Model_name = Model,
# 					      rcp = "rcp45",
#  					      STN_Num =  Station.All$STN_Num[j],
#  				      	Method = "QDM",
# 						Variables = c("TMAX", "TMIN", "PRCP"))
# 			location1 <- Station.All[Station.All$STN_Num==Station.All$STN_Num[i],2:3]; location2 <- Station.All[Station.All$STN_Num==Station.All$STN_Num[j],2:3]
# 			distance_and_gamma <- c( Distance(location1, location2), gamma <- Gamma(tmp1[,Variable], tmp2[,Variable]))
# 			result <- rbind(result, distance_and_gamma)
# 			cat(paste("Station : ", Station_Info$STN_Name[i], " and Station : ", Station_Info$STN_Name[j], " Calculation done...\n", sep=""))
# 		} # end of for(j in i+1:length(Station.All[,1]))
# 	} # end of for(i in 1:length(Station.All[,1]))
#
# 	final <- result[-1,]
# 	colnames(final) <- c("distance", "gamma")
# 	rownames(final) <- NULL
# 	final
# } # end of custom_vrg(Model, Variable)

#' @export
custom_vrg_Obs <- function(Variable,DsDir,stns,syear_obs,eyear_obs,...)
{
	#cat(paste(Variable, "'s Observation variogram step.\n", sep=""))
	result <- matrix(0, nrow = 1, ncol = 2 )

	for(i in 1:(length(stns[,1])-1)) {
	  for(j in seq(i+1, length(stns[,1]), by = 1)) {
		  tmp1.dir <- paste(DsDir,"/OBS/",stns[i,"ID"],"_observed.csv",sep="")
		  tmp1 <- read.csv(tmp1.dir,header = TRUE,na.strings = c("NA",-999,"-999",-99,"-99"))
		  syear_tmp1 <- min(which(tmp1[,"year"]==syear_obs))
		  eyear_tmp1 <- max(which(tmp1[,"year"]==eyear_obs))
		  tmp1 <- tmp1[syear_tmp1:eyear_tmp1,]
		  wh1 <- which(is.na(tmp1[,Variable])==TRUE)
		  tmp2.dir <- paste(DsDir,"/OBS/",stns[j,"ID"],"_observed.csv",sep="")
		  tmp2 <- read.csv(tmp2.dir,header = TRUE,na.strings = c("NA",-999,"-999",-99,"-99"))
		  syear_tmp2 <- min(which(tmp2[,"year"]==syear_obs))
		  eyear_tmp2 <- max(which(tmp2[,"year"]==eyear_obs))
		  tmp2 <- tmp2[syear_tmp2:eyear_tmp2,]
		  wh2 <- which(is.na(tmp2[,Variable])==TRUE)
		  wh <- unique(c(wh1,wh2))
		  if(length(wh)>=1){
		  tmp1 <- tmp1[-wh,]
		  tmp2 <- tmp2[-wh,]
		  }
			cat(paste("Station : ",stns[i,"Ename"],", and Station : ", stns[i,"ID"]," processing...\n"))
			location1 <- c(stns[i,"Lon"],stns[i,"Lat"])
			location2 <- c(stns[j,"Lon"],stns[j,"Lat"])
			distance_and_gamma <- c( Distance(location1, location2), Gamma(tmp1[,Variable], tmp2[,Variable]))
			result <- rbind(result, distance_and_gamma)
		} # end of for(j in i+1:length(Station.All[,1]))
	} # end of for(i in 1:length(Station.All[,1]))

	final <- result[-1,]
	colnames(final) <- c("distance", "gamma")
	rownames(final) <- NULL
	final
} # end of custom_vrg(Model, Variable)


