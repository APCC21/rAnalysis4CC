#' @export

cal.date <- function(syear_his,eyear_scn){
  start <- syear_his%/%4
  end <- (eyear_scn[length(eyear_scn)]%/%4)+1
day1 <- c(seq(1:31),seq(1:28),seq(1:31),seq(1:30),seq(1:31),seq(1:30),seq(1:31),
          seq(1:31),seq(1:30),seq(1:31),seq(1:30),seq(1:31))
day2 <- c(seq(1:31),seq(1:29),seq(1:31),seq(1:30),seq(1:31),seq(1:30),seq(1:31),
          seq(1:31),seq(1:30),seq(1:31),seq(1:30),seq(1:31))

mon1 <- c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
          rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))
mon2 <- c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),
          rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))

monday1 <- cbind(mon1,day1)
monday2 <- cbind(mon2,day2)

for(i in start:end){
  if(i == 1){
    i1 <- 1
    i2 <- 2
    i3 <- 3
  } else {
    i1 <- ((i-1)*4)+1
    i2 <- ((i-1)*4)+2
    i3 <- ((i-1)*4)+3
  }
  i4 <- (i*4)
  out1 <- cbind(i1,monday1)
  out2 <- cbind(i2,monday1)
  out3 <- cbind(i3,monday1)
  out4 <- cbind(i4,monday2)
  if(i == start){
    out <- rbind(out1,out2,out3,out4)
  } else {
    out <- rbind(out,out1,out2,out3,out4)
  }
}
output <- out[min(which(out[,1]==syear_his)):max(which(out[,1]==eyear_scn[length(eyear_scn)])),]
output <- data.frame(output)
colnames(output) <- c("year","mon","day")
output$mon[output$mon<10]<-paste("0",output$mon[output$mon<10],sep="")
output$day[output$day<10]<-paste("0",output$day[output$day<10],sep="")
return(output)
}


################################################################
#' @export
filldate <- function(x, sdate=NULL, edate=NULL) {

  if(!missing(sdate) & !missing(edate)){
    sdate = as.Date(sdate)
    edate = as.Date(edate)
    imsi = as.data.frame(seq(sdate, edate, by=1))
    colnames(imsi) = "date"

    x = x[which(x[,1]>=sdate & x[,1]<=edate),]

    data = merge(imsi, x, all=T)

    year=as.numeric (format(data[,1],"%Y"))
    mon=as.numeric (format(data[,1],"%m"))
    day=as.numeric (format(data[,1],"%d"))
    yearmon=as.character (format(data[,1],"%Y-%m"))
    data <- cbind(year, mon, day, yearmon, data)
  }

  if(!missing(sdate) & missing(edate)){
    sdate = as.Date(sdate)
    nrows = length(x[,1])
    date = seq(sdate, sdate+nrows-1, by=1)

    data = cbind.data.frame(date, x)

    year=as.numeric (format(data[,1],"%Y"))
    mon=as.numeric (format(data[,1],"%m"))
    day=as.numeric (format(data[,1],"%d"))
    yearmon=as.character (format(data[,1],"%Y-%m"))
    data <- cbind(year, mon, day, yearmon, data)

  }

  if(missing(sdate) & missing(edate)){
    x[,1]=as.Date(x[,1])
    year=as.numeric (format(x[,1],"%Y"))
    mon=as.numeric (format(x[,1],"%m"))
    day=as.numeric (format(x[,1],"%d"))
    yearmon=as.character (format(x[,1],"%Y-%m"))
    data <- cbind(year, mon, day, yearmon, x)
  }

  return(data)

}


#' @export
Rank.method <- function(Rate1,Corr1,Rate2=NULL,Corr2=NULL){

  if(is.null(Rate2) & is.null(Corr2)){
    Input <- data.frame(Rate1,Corr1)
    Scale.Input <- scale(Input)
    if(mean(Rate1)==0){Scale.Input[,"Rate1"] <- 0}
    if(mean(Rate1)==1){Scale.Input[,"Rate1"] <- 1}
    if(length(unique(Rate1))==1){
      Scale.Input[,1] <- 0
    }
    if(length(unique(Corr1))==1){
      Scale.Input[,2] <- 0
    }
    Ranksum <- Scale.Input[,1]
    Corrsum <- Scale.Input[,2]
  } else {
    Input <- data.frame(Rate1,Rate2,Corr1,Corr2)
    Scale.Input <- scale(Input)
    if(mean(Rate1)==0){Scale.Input[,"Rate1"] <- 0}
    if(mean(Rate2)==0){Scale.Input[,"Rate2"] <- 0}
    if(mean(Rate1)==1){Scale.Input[,"Rate1"] <- 1}
    if(mean(Rate2)==1){Scale.Input[,"Rate2"] <- 1}
    if(length(unique(Rate1))==1){
      Scale.Input[,1] <- 0
    }
    if(length(unique(Rate2))==1){
      Scale.Input[,2] <- 0
    }
    if(length(unique(Corr1))==1){
      Scale.Input[,3] <- 0
    }
    if(length(unique(Corr2))==1){
      Scale.Input[,4] <- 0
    }
    Ranksum <- Scale.Input[,1]+Scale.Input[,2]
    Corrsum <- Scale.Input[,3]+Scale.Input[,4]
  }

  Ranksum.temp <- Ranksum
  Corrsum.temp <- Corrsum

  Sort.Ranksum <- sort(Ranksum,decreasing = TRUE)

  Temp <- Sort.Ranksum

  cnt <- length(Temp)
  j <- 1
  wh1 <- NA
  for(i in 1:cnt){
    wh <- which(Temp[i]==Ranksum)
    if(length(wh)>1){
      wh1[j] <- i
      j <- j+1
    }
  }

  plus <- matrix(0,length(Rate1),1)

  if(length(wh1) > 1){
  corrtemp <- Corrsum
  for(i in c(wh1)){
    if(sum(ifelse(i == wh1,1,0))==1){
      wh <- which(Temp[i]==Ranksum)
      k <- 0

      for(j in 1:length(wh)){
        wh2 <- which(Corrsum[wh]==min(Corrsum[wh]))
        if(length(wh2)==1){
          plus[wh[wh2],] <- k
          k <- k+1
          wh <- wh[-wh2]
        } else {
          k <- mean(seq(0,length(wh2),1))
          plus[wh,] <- k
        }
      }
    }
  }
  }

  if(is.null(Rate2)){
    temp.Rank <- rank(Rate1,ties.method = "min")
  } else {
    temp.Rank <- rank(Rate1+Rate2,ties.method = "min")
  }

  Out <- (length(Rate1)+1)-(temp.Rank + plus)
  #cbind(temp.Rank,plus,Out,(Scale.Input[,3]+Scale.Input[,4]),Scale.Input)
  return(Out)
}

################################################################
#' @export
GetFilenames <- function(wdir, toMatch) {

  flists = list.files(wdir, full.names = T)

  nloop = length(toMatch)

  for(i in 1:nloop){
    flists = flists[grep(toMatch[i], flists)]
  }

  return(flists)

}


#' GetVarLists4CMIP5
#'
#' Extract necessary list from NetCDF file names according to column number (coln: 1: variables, 2: time-step, 3: models, 4: scenarios, 5: ensambles, 6: periods)
#'
#' @param basedir Directory path containing NetCDF files
#' @param coln Column number for extracting available list (1: variables, 2: time-step, 3: models, 4: scenarios, 5: ensambles, 6: periods)
#'
#' @return varnm List of available variable
#' @export
#'
GetVarLists4CMIP5 <- function(basedir, coln) {


  srchstr = "*.nc"
  flist = list.files(basedir, pattern = glob2rx(srchstr), full.names = F)
  varnm = sapply(strsplit(flist, "_"), function(x) x[[coln]])
  varnm = unique(varnm)

  return(varnm)

}


#' @export
critical.r <- function( n, alpha = 0.05 ) {
  df <- n - 2
  critical.t <- qt( alpha/2, df, lower.tail = F )
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return( critical.r )
}

#' @export
future.csn <- function(syear_scn,eyear_scn,...){
  #syear_scn <- EnvList$syear_scn
  #eyear_scn <- EnvList$eyear_scn

  out <- list()
  for(i in 1:length(syear_scn)){
    out[[i]] <- paste("S",i,"_",substr(ceiling(mean(c(syear_scn[i],eyear_scn[i]))),3,4),sep="")
  }
  output <- unlist(out)
  return(output)
}


