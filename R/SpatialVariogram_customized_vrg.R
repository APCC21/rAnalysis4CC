## Customized Varigram ##
## returns a customized "gstatVariogram", "data.frame",
# Specifically, calculate variograms each day and over the specified period, yield a mean of gamma over all pairs
# It is expected to give a good criterion for comparing a bunch of downscaling methods.
### 2017/05/25, Wonil-Cho, APEC CLIMATE CENTER
#' @export
Vrg_Over_Period <- function( Path, Period, Model_Name, rcp, Downscaling_Method, Variables,...)
{
# Returns a data.frame for visualizing a customized variogram in a daily sense.
# Usage:
# Vrg_Over_Period( Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",	# where the downscaled results lie in.
#			    	   Period = '1988.01.01-1988.12.17',
#			    	   Model_Name = "HadGEM3-RA_v1",
#			    	   rcp = "rcp45",
#			    	   Downscaling_Method = "DQM",
#			    	   Variables = c("TMAX", "TMIN", "PRCP"))

#	Station_Info <- read.table("Station_XYZ.prn",header=TRUE)
#	names( Station_Info ) <- c("STN_Name", "STN_Num", "STN_Lon", "STN_Lat","Height")
#	Station_Info <- data.frame(Station_Info)

	Lower.Bound <- strsplit(Period, "-")[[1]][1]; Upper.Bound <- strsplit(Period, "-")[[1]][2]
	Lower.splt = strsplit(Lower.Bound, "[.]"); Upper.splt = strsplit(Upper.Bound, "[.]")
	Lower = as.Date(paste(Lower.splt[[1]][1],"-",Lower.splt[[1]][2],"-",Lower.splt[[1]][3],sep=""))
	Upper = as.Date(paste(Upper.splt[[1]][1],"-",Upper.splt[[1]][2],"-",Upper.splt[[1]][3],sep=""))

	return.list1 <- list();return.list2 <- list();return.list3 <- list()
	# 3 lists to store variogram results corresponding to 3 variables respectively.

	Days <- seq(Lower, Upper, by="days")
	cls()  # invoke
	cat(" Calculating variogram each day over period \n");cat("\n")
	cat("###########################################################\n")
	cat("# BE CAREFUL !! FOR ALL VARIABLES, CONSTANT TREND ASSUMED #\n")
	cat("###########################################################\n");cat("\n")

	pb <- txtProgressBar(min = 0, max =length(Days), initial = 0, char = "=", style = 3)
	for(i in 1:length(Days))
	{
		temp <- Combine_Data.Frame_Daily( Path,	# where the downscaled results lie in.
	  	                  	 	    Days[i],	# Date format
			    			 	    Model_Name,
			    				    rcp,
			    			 	    Downscaling_Method,
			    			  	    Variables)

		return.list1[[i]] <- variogram(temp[[4]]~1, data = temp)
		return.list2[[i]] <- variogram(temp[[5]]~1, data = temp)
		return.list3[[i]] <- variogram(temp[[6]]~1, data = temp)
		rm(temp); setTxtProgressBar(pb, i)
#		cat("\n");cat("============================================================================\n")
#		cat(paste( Days[i], " variogram successfully calculated...\n", sep=""));cat("\n")
	} # end of for(i in length(Days)
	cat("DATA Successfully Loaded ! ")
	close(pb)

	# return.list1 : list with objects over the given period. i.e, if the period is 5 days, has 5 objects,
	#		     each object has data for drawing variogram for the corresponding day with first variable.
	# return.list2 : for second variable
	# return.list3 : for third variable

	# print(paste("dim of return.list1 ",length(return.list1)))
	vrg1 <- return.list1[[1]]; vrg2 <- return.list2[[1]]; vrg3 <- return.list3[[1]]
	# print(vrg1);print(vrg2);print(vrg3)
	# target attribute is 'gamma'
	vrg1[,"gamma"] <- apply(Gamma_mean(return.list1), MARGIN = 1, mean)
	vrg2[,"gamma"] <- apply(Gamma_mean(return.list2), MARGIN = 1, mean)
	vrg3[,"gamma"] <- apply(Gamma_mean(return.list3), MARGIN = 1, mean)

	Data_describe2()
	final.result <- list(vrg1, vrg2, vrg3)
	names(final.result) <- Variables
	final.result
} # End of Vrg_Over_Period


#' @export
Vrg_Over_Months <- function(Path, Years, Months, Model_Name, rcp, Downscaling_Method, Variables,...)
{
	# Returns a data.frame for visualizing a customized variogram in a daily sense.
	# Usage:
	# Vrg_Over_Months( Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",	# where the downscaled results lie in.
	#				   Years = 1999:2001 # doesn`t need to be consecutive. You can give an argument like c(1988, 1995, 2000)
	#			         Months = 1:5, 	 # means that from January to May,
										 # doesn`t need to be consecutive. That is, you can give it c(1,5,12) [Jan. May. Dec.]
	#				   Model_Name = "HadGEM3-RA_v1",
	#			 	   rcp = "rcp45",
	#			 	   Downscaling_Method = "DQM",
	#			 	   Variables = c("TMAX", "TMIN", "PRCP"))
	Station_Info <- read.table("Station_XYZ.prn",header=TRUE)
	names( Station_Info ) <- c("STN_Name", "STN_Num", "STN_Lon", "STN_Lat","Height")
	Station_Info <- data.frame(Station_Info)

	return.list1 <- list();return.list2 <- list();return.list3 <- list()
	# 3 lists to store variogram results corresponding to 3 variables respectively.

	Days <- c(as.Date('1988-12-17'))	# To fit the type of 'Days' vector, temporarily assigned
	for(i_Yr in 1:length(Years))
	{
		for(i_Mn in 1:length(Months))
		{
			begin <- as.Date(paste(Years[i_Yr], "-", Months[i_Mn], "-01", sep=""))
			end <- as.Date(paste(Years[i_Yr], "-", Months[i_Mn], "-", monthDays(begin), sep=""))
			Days <- c(Days, seq(begin, end, by = "days"))
		}
	}
	Days <- Days[-1]	# take away one now having no value in use

	cls()  # invoke
	cat(" Calculating variogram each day over give Months \n");cat("\n")
	cat("###########################################################\n")
	cat("# BE CAREFUL !! FOR ALL VARIABLES, CONSTANT TREND ASSUMED #\n")
	cat("###########################################################\n");cat("\n")


	pb <- txtProgressBar(min = 0, max =length(Days), initial = 0, char = "=", style = 3)
	for(i in 1:length(Days))
	{
		temp <- Combine_Data.Frame_Daily( Path,	# where the downscaled results lie in.
	  	                  	 	    Days[i],	# Date format
			    			 	    Model_Name,
			    				    rcp,
			    			 	    Downscaling_Method,
			    			  	    Variables)

		return.list1[[i]] <- variogram(temp[[4]]~1, data = temp)
		return.list2[[i]] <- variogram(temp[[5]]~1, data = temp)
		return.list3[[i]] <- variogram(temp[[6]]~1, data = temp)
		rm(temp); setTxtProgressBar(pb, i)

	} # end of for(i in 1:length(Days))
	cat("DATA Successfully Loaded ! ")
	close(pb)

	# return.list1 : list with objects over the given period. i.e, if the period is 5 days, has 5 objects,
	#		     each object has data for drawing variogram for the corresponding day with first variable.
	# return.list2 : for second variable
	# return.list3 : for third variable

	# print(paste("dim of return.list1 ",length(return.list1)))
	vrg1 <- return.list1[[1]]; vrg2 <- return.list2[[1]]; vrg3 <- return.list3[[1]]
	# print(vrg1);print(vrg2);print(vrg3)
	# target attribute is 'gamma'
	vrg1[,"gamma"] <- apply(Gamma_mean(return.list1), MARGIN = 1, mean)
	vrg2[,"gamma"] <- apply(Gamma_mean(return.list2), MARGIN = 1, mean)
	vrg3[,"gamma"] <- apply(Gamma_mean(return.list3), MARGIN = 1, mean)

	Data_describe2()
	final.result <- list(vrg1, vrg2, vrg3)
	names(final.result) <- Variables
	final.result



} # End of Vrg_Over_Months

#' @export
Gamma_mean <- function(vrg.list)
{
	n <- length(vrg.list)
	# Actually, n is the number of days
	flag <- n
	while(flag)
	{
		if(flag==n)
		{
			result <- vrg.list[[flag]]$"gamma"
			flag <- flag-1
		}
		result <- cbind(result, vrg.list[[flag]]$"gamma")
		flag <- flag-1
	}
	result
} # end of Gamma_mean

#' @export
Data_describe2 <-function()
{
	cat("****************************************************************************\n")
	cat("		       Mean Variogram over the given Period		    \n")
	cat("****************************************************************************\n")
	Sys.sleep(1)
	cat("		            	Data Structure				    \n")
	cat("****************************************************************************\n")
	cat(" List of 3 and each element looks like below\n")
	cat("  $ :Classes gstatVariogram and data.frame:       	\n")
	cat("..$ np     :number of points in each bin	\n")
	cat("..$ dist   :average distance each bin	\n")
	cat("..$ gamma  :average gamma value each bin	\n")
	cat("****************************************************************************\n")
	cat("		                  Example Data				    \n")
	cat("****************************************************************************\n")
	cat("[[1]]	first 3 rows\n")
	cat("    np      dist     gamma dir.hor dir.ver   id	\n")
	cat("1    9 0.2153448 3.3197951       0       0 var1	\n")
	cat("2   35 0.3360925 0.3693925       0       0 var1	\n")
	cat("3   47 0.4611990 0.3804411       0       0 var1	\n")
	cat("****************************************************************************\n")
	cat(" USAGE \n")
	cat(" One can draw variogram with this data frame intactly\n")
	cat(" EX : ggplot(df[[1]],aes(x=dist,y=gamma,size=np)) + geom_point() \n")
	cat("	df[[n]] for the n-th variable \n")
} # end of Data_describe

