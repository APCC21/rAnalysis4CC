### Tools loading ASCII formatted files, make them suitable for a purpose of spatial analysis and return them.
### 2017/05/24, Wonil-Cho, APEC CLIMATE CENTER

#' @export
Read_DATA <- function(Path, Model_name, rcp, STN_Num, Method, Variables,...)
{
# Usage :
# Read_DATA(Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",
#            Model_name = "HadGEM3-RA_v1",
#		 rcp = "rcp45",
#		 STN_Num =  90,
#		 Method = "DQM",
#		 Variables = c("TMAX", "TMIN", "PRCP"))

#   cat(paste("Station : ", STN_Num," loaded...\n"))

    if (STN_Num<100) {
      STN_Num_text <- paste("0",STN_Num,sep="")
    } else {
      STN_Num_text <- toString(STN_Num)
    }
 	Pattern = paste(STN_Num_text,".*",Method,".*\\.txt",sep="")
	file.path <- list.files(path = paste(Path,Model_name,"\\",rcp,"\\",sep=""),
				 pattern = Pattern,
				 full.names = TRUE)
	file = read.table(file.path, header=FALSE)
	head(file)
	names(file) = c("Year", "Month", "Day", Variables)
	data.frame(file)
} # end of read_DATA

#' @export
Read_DATA_by_Row <- function(Path, Model_name, rcp, STN_Num, Method, Row, Variables,...)
{
# Usage :
# Read_DATA(Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",
#            Model_name = "HadGEM3-RA_v1",
#		 rcp = "rcp45",
#		 STN_Num =  90,
#		 Method = "DQM",
#		 Row = 2,
#		 Variables = c("TMAX", "TMIN", "PRCP"))

#   cat(paste("Station : ", STN_Num," loaded...\n"))

    if (STN_Num<100) {
      STN_Num_text <- paste("0",STN_Num,sep="")
    } else {
      STN_Num_text <- toString(STN_Num)
    }
 	Pattern = paste(STN_Num_text,".*",Method,".*\\.txt",sep="")
	file.path <- list.files(path = paste(Path,Model_name,"\\",rcp,"\\",sep=""),
				 pattern = Pattern,
				 full.names = TRUE)
	file = read.table(file.path, header=FALSE, nrows = 1, skip = Row-1)
	head(file)
	names(file) = c("Year", "Month", "Day", Variables)
	data.frame(file)
} # end of read_DATA_by_Row


#' @export
Combine_Data.Frame_Period <- function(Path, Period, Model_Name, rcp, Downscaling_Method, Variables,...)
{
# Usage:
# Combine_Data.Frame_Period( Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",	# where the downscaled results lie in.
#			    		Period = '1988.01.01-1988.12.17',
#			    		Model_Name = "HadGEM3-RA_v1",
#			    		rcp = "rcp45",
#			    		Downscaling_Method = "DQM"
#			    		Variables = c("TMAX", "TMIN", "PRCP"))
	Station_Info <- read.table("Station_XYZ.prn",header=TRUE)
	names( Station_Info ) <- c("STN_Name", "STN_Num", "STN_Lon", "STN_Lat","Height")
	Station_Info <- data.frame(Station_Info)

	Lower.Bound <- strsplit(Period, "-")[[1]][1]; Upper.Bound <- strsplit(Period, "-")[[1]][2]
	Lower.splt = strsplit(Lower.Bound, "[.]"); Upper.splt = strsplit(Upper.Bound, "[.]")
	Lower = as.Date(paste(Lower.splt[[1]][1],"-",Lower.splt[[1]][2],"-",Lower.splt[[1]][3],sep=""))
	Upper = as.Date(paste(Upper.splt[[1]][1],"-",Upper.splt[[1]][2],"-",Upper.splt[[1]][3],sep=""))

	temp = Read_DATA(Path = Path,
			     Model_name = Model_Name,
			     rcp = rcp,
			     STN_Num = (Station_Info$STN_Num)[1],
			     Method = Downscaling_Method,
			     Variables = Variables)

	Begin = as.Date( paste( (temp$Year)[1], "-", (temp$Month)[1], "-", (temp$Day)[1], sep=""))
	End = as.Date( paste( (temp$Year)[length(temp$Year)], "-", (temp$Month)[length(temp$Year)], "-", (temp$Day)[length(temp$Year)], sep=""))
	rm(temp)
	Day.All = seq(Begin, End, by="days")
	Day.Tar = seq(Lower, Upper, by = "days")
	Criterion = Day.All %in% Day.Tar	# Boolean Vector like "FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE"
							# used for extracting target instances(rows) from a whole data

	Target = matrix(ncol = 3, nrow = length(Station_Info$STN_Num))
	# This 'Target' matrix is to be returned, containing manipulated information for visualizing, analysis and etc...
	# A coordinated data.frame look like below
	# for variables of interest, say them TMAX, TMIN, PRCP,
	# STN_NUM,	TMAX,	TMIN, PRCP
	#	1	  5	  -1	 .5
	#	2	  6	  .5	 1.5
	#    ...	 ...	 ...	....
	# Where STN_NUM = 2, say that we are interested in PRCP, 1.5 is the average of PRCP values over the target period(the Period argument) at STN no.2

	pb <- winProgressBar(title="Load Station Data...", label="0% loaded", min=0, max=100, initial=0)
	for(i_STN in 1:length(Station_Info$STN_Num))
	{
		temp2 <- matrix(ncol = length(Variables), nrow = sum(Criterion))
		dd <- Read_DATA(Path, Model_Name, rcp, (Station_Info$STN_Num)[i_STN], Downscaling_Method, Variables)
		Target[i_STN,] <- apply(dd[Criterion,4:6], MARGIN = 2, mean)
		info <- sprintf("%d%% loaded", round((i_STN/length(Station_Info$STN_Num))*100))
		setWinProgressBar(pb, (i_STN/length(Station_Info$STN_Num))*100, label=info)
		rm(temp2)
	} # end of for(i_STN in 1:(Station_Info$STN_Num))
	close(pb)
	Target <- data.frame(Target)
	df <-	cbind(Station_Info, Target)
	names(df) <- c(names( Station_Info ), Variables)

	coordinates(df) = ~ STN_Lon+STN_Lat
	Data_Describe()
	df
} # end of Combine_Data.Frame_Period

#' @export
Combine_Data.Frame_Annual <- function(Path, Year, Model_Name, rcp, Downscaling_Method, Variables,...)
{
# Usage:
# Combine_Data.Frame_Annual( Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",	# where the downscaled results lie in.
#			    		Period = 2000,
#			    		Model_Name = "HadGEM3-RA_v1",
#			    		rcp = "rcp45",
#			    		Downscaling_Method = "DQM",
#			    		Variables = c("TMAX", "TMIN", "PRCP"))
	Station_Info <- read.table("Station_XYZ.prn",header=TRUE)
	names( Station_Info ) <- c("STN_Name", "STN_Num", "STN_Lon", "STN_Lat","Height")
	Station_Info <- data.frame(Station_Info)

	temp = Read_DATA(Path = Path,
			     Model_name = Model_Name,
			     rcp = rcp,
			     STN_Num = (Station_Info$STN_Num)[1],
			     Method = Downscaling_Method,
			     Variables = Variables)

	Criterion = rep(0, length(temp[[1]]))
	Criterion = (temp[[1]] == Year)	# Boolean Vector like "FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE"
							# used for extracting target instances(rows) from a whole data
	rm(temp)
	Target = matrix(ncol = 3, nrow = length(Station_Info$STN_Num))
	# This 'Target' matrix is to be returned, containing manipulated information for visualizing, analysis and etc...
	# A coordinated data.frame look like below
	# for variables of interest, say them TMAX, TMIN, PRCP,
	# STN_NUM,	TMAX,	TMIN, PRCP
	#	1	  5	  -1	 .5
	#	2	  6	  .5	 1.5
	#    ...	 ...	 ...	....
	# Where STN_NUM = 2, say that we are interested in PRCP, 1.5 is the average of PRCP values over the target period(the Period argument) at STN no.2

	pb <- winProgressBar(title="Load Station Data...", label="0% loaded", min=0, max=100, initial=0)
	for(i_STN in 1:length(Station_Info$STN_Num))
	{
		temp2 <- matrix(ncol = length(Variables), nrow = sum(Criterion))
		dd <- Read_DATA(Path, Model_Name, rcp, (Station_Info$STN_Num)[i_STN], Downscaling_Method, Variables)
		Target[i_STN,] <- apply(dd[Criterion,4:6], MARGIN = 2, mean)
		info <- sprintf("%d%% loaded", round((i_STN/length(Station_Info$STN_Num))*100))
		setWinProgressBar(pb, (i_STN/length(Station_Info$STN_Num))*100, label=info)
		rm(temp2)
	} # end of for(i_STN in 1:(Station_Info$STN_Num))
	close(pb)
	Target <- data.frame(Target)
	df <-	cbind(Station_Info, Target)
	names(df) <- c(names( Station_Info ), Variables)

	coordinates(df) = ~ STN_Lon+STN_Lat
	Data_Describe()
	df
} # end of Combine_Data.Frame_Annual


#' @export
Combine_Data.Frame_Monthly <- function(Path, Month, Model_Name, rcp, Downscaling_Method, Variables,...)
{
# Usage:
# Combine_Data.Frame_Monthly( Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",	# where the downscaled results lie in.
#			    		Month = 4,
#			    		Model_Name = "HadGEM3-RA_v1",
#			    		rcp = "rcp45",
#			    		Downscaling_Method = "DQM",
#			    		Variables = c("TMAX", "TMIN", "PRCP"))
	Station_Info <- read.table("Station_XYZ.prn",header=TRUE)
	names( Station_Info ) <- c("STN_Name", "STN_Num", "STN_Lon", "STN_Lat","Height")
	Station_Info <- data.frame(Station_Info)

	temp = Read_DATA(Path = Path,
			     Model_name = Model_Name,
			     rcp = rcp,
			     STN_Num = (Station_Info$STN_Num)[1],
			     Method = Downscaling_Method,
			     Variables = Variables)

	Criterion = rep(0, length(temp[[1]]))
	Criterion = (temp[[2]] == Month)	# Boolean Vector like "FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE"
							# used for extracting target instances(rows) from a whole data
	rm(temp)
	Target = matrix(ncol = 3, nrow = length(Station_Info$STN_Num))
	# This 'Target' matrix is to be returned, containing manipulated information for visualizing, analysis and etc...
	# A coordinated data.frame look like below
	# for variables of interest, say that TMAX, TMIN, PRCP
	# STN_NUM,	TMAX,	TMIN, PRCP
	#	1	  5	  -1	 .5
	#	2	  6	  .5	 1.5
	#    ...	 ...	 ...	....
	# Where STN_NUM = 2, say that we are interested in PRCP, 1.5 is the average of PRCP values over the target period(the Period argument) at STN no.2

	pb <- winProgressBar(title="Load Station Data...", label="0% loaded", min=0, max=100, initial=0)
	for(i_STN in 1:length(Station_Info$STN_Num))
	{
#		temp2 <- matrix(ncol = length(Variables), nrow = sum(Criterion))
		dd <- Read_DATA(Path, Model_Name, rcp, (Station_Info$STN_Num)[i_STN], Downscaling_Method, Variables)
		Target[i_STN,] <- apply(dd[Criterion,4:6], MARGIN = 2, mean)
		info <- sprintf("%d%% loaded", round((i_STN/length(Station_Info$STN_Num))*100))
		setWinProgressBar(pb, (i_STN/length(Station_Info$STN_Num))*100, label=info)
#		rm(temp2)
	} # end of for(i_STN in 1:(Station_Info$STN_Num))
	close(pb)
	Target <- data.frame(Target)
	df <-	cbind(Station_Info, Target)
	names(df) <- c(names( Station_Info ), Variables)

	coordinates(df) = ~ STN_Lon+STN_Lat
	Data_Describe()
	df

} # end of Combine_Data.Frame_Monthly


#' @export
Combine_Data.Frame_Daily <- function(Path, Day, Model_Name, rcp, Downscaling_Method, Variables,...)
{
# Usage:
# Combine_Data.Frame_Daily( Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",	# where the downscaled results lie in.
#			    		Day = '2010-01-01',	# Date format
#			    		Model_Name = "HadGEM3-RA_v1",
#			    		rcp = "rcp45",
#			    		Downscaling_Method = "DQM",
#			    		Variables = c("TMAX", "TMIN", "PRCP"))
	Station_Info <- read.table("Station_XYZ.prn",header=TRUE)
	names( Station_Info ) <- c("STN_Name", "STN_Num", "STN_Lon", "STN_Lat","Height")
	Station_Info <- data.frame(Station_Info)

	df <- data.frame(date = Day,
                 year = as.numeric(format(Day, format = "%Y")),
                 month = as.numeric(format(Day, format = "%m")),
                 day = as.numeric(format(Day, format = "%d")))

	temp <- Read_DATA(Path = Path,
			     Model_name = Model_Name,
			     rcp = rcp,
			     STN_Num = (Station_Info$STN_Num)[1],
			     Method = Downscaling_Method,
			     Variables = Variables)

	Criterion <- rep(0, length(temp[[1]]))
	Criterion <- (temp[[1]] == df$year) & (temp[[2]] == df$month) & (temp[[3]] == df$day)  	# Boolean Vector like "FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE"
																# used for extracting target instances(rows) from a whole data

	rm(temp)
	Target = matrix(ncol = 3, nrow = length(Station_Info$STN_Num))
	# This 'Target' matrix is to be returned, containing manipulated information for visualizing, analysis and etc...
	# A coordinated data.frame look like below
	# for variables of interest, say them TMAX, TMIN, PRCP,
	# STN_NUM,	TMAX,	TMIN, PRCP
	#	1	  5	  -1	 .5
	#	2	  6	  .5	 1.5
	#    ...	 ...	 ...	....
	# Where STN_NUM = 2, say that we are interested in PRCP, 1.5 is the average of PRCP values over the target period(the Period argument) at STN no.2
	pb <- winProgressBar(title="Load Station Data...", label="0% loaded", min=0, max=100, initial=0)
	for(i_STN in 1:length(Station_Info$STN_Num))
	{
#		temp2 <- matrix(ncol = length(Variables), nrow = sum(Criterion))
		dd <- Read_DATA(Path, Model_Name, rcp, (Station_Info$STN_Num)[i_STN], Downscaling_Method, Variables)
		Target[i_STN,] <- apply(dd[Criterion,4:6], MARGIN = 2, mean)
		info <- sprintf("%s data %d%% loaded", Day, round((i_STN/length(Station_Info$STN_Num))*100))
		setWinProgressBar(pb, (i_STN/length(Station_Info$STN_Num))*100, label=info)
#		rm(temp2)	# Looks redundant... why I declared this?
	} # end of for(i_STN in 1:(Station_Info$STN_Num))
	close(pb)
	Target <- data.frame(Target)
	df <-	cbind(Station_Info, Target)
	names(df) <- c(names( Station_Info ), Variables)

	coordinates(df) = ~ STN_Lon+STN_Lat
#	Data_Describe()	# Too verbose
	df

} # end of Combine_Data.Frame_Daily

cls <- function() {
        require(RDCOMClient)
        wsh <- COMCreate("Wscript.Shell")
        wsh$SendKeys("\014")
        invisible(wsh)
} # clear console programmatically



#' @export
Read_DATA_Historical_RCM <- function(Path, Model_name, rcp, STN_Num, Method, Variables,...)
{
# Usage :
# Read_DATA_Historical_RCM(Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",
#            Model_name = "HadGEM3-RA_v1",
#		 rcp = "rcp45",
#		 STN_Num =  90,
#		 Method = "DQM",
#		 Variables = c("TMAX", "TMIN", "PRCP"))

#   cat(paste("Station : ", STN_Num," loaded...\n"))

    if (STN_Num<100) {
      STN_Num_text <- paste("0",STN_Num,sep="")
    } else {
      STN_Num_text <- toString(STN_Num)
    }
 	Pattern = paste(STN_Num_text,".*",Method,".*\\.txt",sep="")
	file.path <- list.files(path = paste(Path,Model_name,"\\",rcp,"\\",sep=""),
				 pattern = Pattern,
				 full.names = TRUE)
	Row = difftime(as.Date('2005-12-31'), as.Date('1979-01-01'), units="days")
	file = read.table(file.path, header=FALSE, nrows = Row+1)
	head(file)
	names(file) = c("Year", "Month", "Day", Variables)
	new_file = file[!((file[,2]==6)|(file[,2]==7)|(file[,2]==8)|(file[,2]==9)),]
	data.frame(new_file)
} # end of read_DATA_by_Row

#' @export
Read_DATA_Historical_GCM <- function(Path, Model_name, rcp, STN_Num, Method, Variables,...)
{
# Usage :
# Read_DATA_Historical_GCM(Path = "C:\\Users\\jowonil\\Desktop\\Cordex Output\\",
#            Model_name = "HadGEM3-RA_v1",
#		 rcp = "rcp45",
#		 STN_Num =  90,
#		 Method = "DQM",
#		 Variables = c("TMAX", "TMIN", "PRCP"))

#   cat(paste("Station : ", STN_Num," loaded...\n"))

    if (STN_Num<100) {
      STN_Num_text <- paste("0",STN_Num,sep="")
    } else {
      STN_Num_text <- toString(STN_Num)
    }
 	Pattern = paste(STN_Num_text,".*",Method,".*\\.txt",sep="")
	file.path <- list.files(path = paste(Path,Model_name,"\\",rcp,"\\",sep=""),
				 pattern = Pattern,
				 full.names = TRUE)
	Start = difftime(as.Date('1979-01-01'), as.Date('1976-01-01'), units="days")
	Row = difftime(as.Date('2005-12-31'), as.Date('1979-01-01'), units="days")
	file = read.table(file.path, header=FALSE, skip = Start, nrows = Row+1)
	head(file)
	names(file) = c("Year", "Month", "Day", Variables)
	new_file = file[!((file[,2]==6)|(file[,2]==7)|(file[,2]==8)|(file[,2]==9)),]
	data.frame(new_file)
} # end of read_DATA_by_Row


#' @export
Read_DATA_Historical_Obs <- function(STN_Num)
{
    if (STN_Num<100) {
      STN_Num_text <- paste("0",STN_Num,sep="")
    } else {
      STN_Num_text <- toString(STN_Num)
    }
 	Pattern = paste(STN_Num_text,".daily.19730101-20101231_QC.asc",sep="")
	# 101.daily.19730101-20101231_QC
#	print(Pattern)
	file.path <- list.files("D:\\Projects\\Downscaling\\DATA\\AMS\\",
				 pattern = Pattern,
				 full.names = TRUE)
	print(file.path)
	Start = difftime(as.Date('1979-01-01'), as.Date('1973-01-01'), units="days")
	Row = difftime(as.Date('2005-12-31'), as.Date('1979-01-01'), units="days")
	file = read.table(file.path, header=FALSE, skip = Start+1, nrows = Row+1)
	head(file)
	names(file) = c("Year", "Month", "Day", "PRCP", "TAVE", "TMAX", "TMIN", "DEWP", "WAVE", "WMAX", "HAVE", "HMIN")
	file = file[,c("Year", "Month", "Day", "PRCP", "TMAX", "TMIN")]
	final.file = cbind(file[,1:3], file[,5], file[,6], file[,4])
	names(final.file) = c("Year", "Month", "Day", "TMAX", "TMIN", "PRCP")
	new_file = final.file[!((final.file[,2]==6)|(final.file[,2]==7)|(final.file[,2]==8)|(final.file[,2]==9)),]
	data.frame(new_file)
} # end of read_DATA_by_Row


#' @export
Data_Describe <- function(){
	cat("============================================================================\n")
	cat("****************************************************************************\n")
	cat("		       Spatial Point Data Frame Successfully Loaded		    \n")
	cat("****************************************************************************\n")
	cat("============================================================================\n")
	Sys.sleep(1)
	cat("****************************************************************************\n")
	cat("		            	Data Structure				    \n")
	cat("****************************************************************************\n")
	cat("============================================================================\n")
	cat("..@ data       :'data.frame': 60 obs. of  6 variables:\n")
	cat("..$ STN_Name: Factor w/ 60 levels \"Boeun\",\"Boryeong\"..\n")
	cat("..$ STN_Num : int [1:60] 90 100 101 105 108 112 114 ..\n")
	cat("..$ Height  : num [1:60] 18.1 772.6 77.7 26 85.8 .....\n")
	cat("..$ TMAX    : num [1:60] 15.6 12.5 18.4 17.8 17.2 ....\n")
	cat("..$ TMIN    : num [1:60] 5.93 0.256 3.485 6.969 6.42..\n")
	cat("..$ PRCP    : num [1:60] 2.71 3.32 3.58 2.76 3.32 ....\n")
	cat("============================================================================\n")
	cat("****************************************************************************\n")
	cat("		                  Example Data				    \n")
	cat("****************************************************************************\n")
	cat("============================================================================\n")

	cat("	coordinates      STN_Name STN_Num Height     TMAX      TMIN     PRCP\n")
	cat("    (128.55, 38.25)        Sokcho      90   18.1 15.63984 5.9300219 2.707618\n")
	cat("(128.7167, 37.6667) Daegwallyeong     100  772.6 12.49800 0.2562238 3.317928\n")
	cat("   (127.7333, 37.9)     Chuncheon     101   77.7 18.42330 3.4848475 3.584353\n")
	cat("  (128.8833, 37.75)     Gangneung     105   26.0 17.79455 6.9694531 2.764799\n")
	cat("  (126.95, 37.5667)         Seoul     108   85.8 17.18394 6.4279464 3.322001\n")
	cat("(126.6167, 37.4667)       Incheon     112   68.2 15.49831 6.0334878 2.935091\n")
	cat("****************************************************************************\n")
	cat("============================================================================\n")
}
