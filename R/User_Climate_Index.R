################################################################################
# Number of rain days with more than X mm
################################################################################
#' @export
Users.Prcp.Xmm.Prcp.Days <- function(data, syear, eyear, xmm) {
  
  out = data.frame(matrix(vector(), 0, 2), stringsAsFactors=FALSE)
  colnames(out) = c("year", "value")
  
  for(i in syear:eyear){
    imsi = data[which(data$year == i & data$prcp >= xmm),]
    cnt = nrow(imsi)
    result = c(i, cnt)
    
    out = rbind(out, result)
    colnames(out) = c("year", "value")
  }
  
  return(out)
  
}

################################################################################
# Maximum precipitation amount in X-days
################################################################################
#' @export
Users.Prcp.Xday.Max.Prcp <- function(data, syear, eyear, xday) {
  
  out = data.frame(matrix(vector(), 0, 2), stringsAsFactors=FALSE)
  colnames(out) = c("year", "value")
  
  for(i in syear:eyear){
    imsi = data[which(data$year == i),c("prcp")]
    daycnt = length(imsi)
    
    if(daycnt > 0){
      
      eday = daycnt - xday + 1
      
      maxval = 0
      for(j in 1:eday){
        accu = sum(imsi[j:(j+xday-1)], na.rm=T)
        if(accu >= maxval){
          maxval = accu
        } 
      }
      result = c(i, maxval)
      
      out = rbind(out, result)
      colnames(out) = c("year", "value")
      
    }
    
  }
  
  return(out)
  
}

################################################################################
# Max. dry spells
################################################################################
#' @export
Users.Prcp.Max.Dry.Day <- function(data, syear, eyear) {
  
  options(stringsAsFactors = FALSE)
  
  out = data.frame(matrix(vector(), 0, 2), stringsAsFactors=FALSE)
  colnames(out) = c("year", "value")
  
  for(i in syear:eyear){
    imsi = data[which(data$year == i),c("prcp")]
    imsi[which(is.na(imsi))] = -99
    daycnt = length(imsi)
    
    if(daycnt > 0){
      
      cnt = 0
      maxcnt = 0
      
      for(j in 1:daycnt){
        if(imsi[j] == 0){
          cnt = cnt + 1
          if(cnt >= maxcnt) {maxcnt = cnt}
        } else {
          cnt = 0
        }
      }
      result = c(i, maxcnt)
      
      out = rbind(out, result)
      colnames(out) = c("year", "value")
      
    }
    
  }
  
  return(out)
  
}

################################################################################
# Precipitation intensity
################################################################################
#' @export
Users.Prcp.Intensity <- function(data, syear, eyear) {
  
  out = data.frame(matrix(vector(), 0, 2), stringsAsFactors=FALSE)
  colnames(out) = c("year", "value")
  
  for(i in syear:eyear){
    sum = sum(data[which(data$year == i & data$prcp > 0),c("prcp")])
    cnt = length(data[which(data$year == i & data$prcp > 0),c("prcp")])
    pint = sum/cnt
    result = c(i, pint)
    out = rbind(out, result)
    colnames(out) = c("year", "value")
  }
  
  return(out)
  
}

################################################################################
# Seasonal precipitation amount (X mon ~ Y mon)
################################################################################
#' @export
Users.Prcp.Monthly.Sum <- function(data, syear, eyear, smonth, emonth) {
  
  out = data.frame(matrix(vector(), 0, 2), stringsAsFactors=FALSE)
  colnames(out) = c("year", "value")
  
  if(smonth < emonth){
    for(i in syear:eyear){
      ptot = sum(data[which(data$year == i & data$month >= smonth & data$month <= emonth),c("prcp")], na.rm=T)
      result = c(i, ptot)
      out = rbind(out, result)
      colnames(out) = c("year", "value")
    }
  } else {
    for(i in syear:eyear){
      ptot1 = sum(data[which(data$year == i & data$month >= smonth & data$month <= 12),c("prcp")], na.rm=T)
      ptot2 = sum(data[which(data$year == i & data$month >= 1 & data$month <= emonth),c("prcp")], na.rm=T)
      ptot = ptot1 + ptot2
      
      result = c(i, ptot)
      
      out = rbind(out, result)
      colnames(out) = c("year", "value")
    }
    
  }
  
  return(out)
  
}

