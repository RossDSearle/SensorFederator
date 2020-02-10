library(XML)
library(xts)
library(stringr)

#print(paste0(' BEU = ',sensorRootDir ))
#source(paste0(sensorRootDir, '/Backends/Backends.R'))



convertJSONtoDF <- function(resp){

  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))

  for (i in 1:nrow(xin)) {
    d <- xin[i,]
    dd <- d$DataStream
    outDF[, (i+1)] <- dd[[1]]$v

   # d$DataType
  #  d$UpperDepthCm

    if(is.null(d$UpperDepth[1])){
      suffix = paste0('_x', i)
    }else if(is.na(d$UpperDepth[1])){
      suffix = paste0('_x', i)
    }else if(d$UpperDepth == d$LowerDepth[1]){
      suffix = paste0('_', d$UpperDepth[1])
    }else{
      suffix =  paste0('_', d$UpperDepth[1], '_', d$LowerDepth[1])
    }
    cnames[i+1] <- c(paste0(d$DataType[1],suffix))
  }
  colnames(outDF) <- cnames
  return(outDF)
}


makeNestedDF <- function(TS, sensors, startDate, endDate, aggperiod){


  DF <- to.DF(TS)

  x <- data.frame(matrix(NA, nrow= (ncol(DF)-1), ncol=18))

  colnames(x) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Latitude', 'Longitude', 'SensorID', 'SensorName',
                   'UpperDepthCm', 'LowerDepthCm', 'RequestStartDate', 'RequestEndDate', 'AggregationPeriod', 'DataType',
                   'Units', 'Calibrated', 'DataStream')

  x$SiteID <- sensors$SiteID
  x$SiteName <- sensors$SiteName
  x$Provider <- sensors$Provider
  x$Backend <- sensors$Backend
  x$Access <- sensors$Access
  x$Latitude <- sensors$Latitude
  x$Longitude <- sensors$Longitude
  x$SensorID <- sensors$SensorID
  x$SensorName <- sensors$SensorName
  x$UpperDepthCm <- sensors$UpperDepth
  x$LowerDepthCm <- sensors$LowerDepth
  x$RequestStartDate <- startDate
  x$RequestEndDate <- endDate
  x$AggregationPeriod <- aggperiod
  x$DataType <- sensors$DataType
  x$Units <- sensors$Units
  x$Calibrated <- sensors$Calibrated

  TSout <- vector("list", ncol(DF)-1)

  for (i in 1 : (ncol(DF)-1)) {
    rdf <- data.frame(t=DF$DateTime, v=DF[i+1])
    colnames(rdf) <- c('t','v')
    TSout[[i]] <- rdf
  }


  x$DataStream <- I(TSout)

  return(x)

}


delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
  x.list[unlist(lapply(x.list, length) != 0)]
}


mergedfTSList <- function(listofDFTS, streams){

  #lodf = list(length=nrow(streams))  # This is wrong - need to fix to


  lodf <-   vector("list", length(listofDFTS) )
  for(i in 1:length(listofDFTS)){

    ndf <- listofDFTS[[i]]
    sensor = streams[i, ]
    if(is.na(sensor$UpperDepth[1])){
      suffix = paste0('_x', i)
    }else if(sensor$UpperDepth == sensor$LowerDepth[1]){
      suffix = paste0('_', sensor$UpperDepth[1])
    }else{
      suffix =  paste0('_', sensor$UpperDepth[1], '_', sensor$LowerDepth[1])
    }
    colnames(ndf) <- c('Date', paste0(streams$DataType[1],suffix))

    lodf[[i]] <- ndf
  }


  mdf = Reduce(function(...) merge(..., all=T), lodf)
  return(mdf)

}



convertToXML <- function(df){

  xml <- xmlTree()
  xml$addTag("document", close=FALSE)
  for (i in 1:nrow(df)) {
    xml$addTag("row", close=FALSE)
    for (j in names(df)) {
      xml$addTag(j, df[i, j])
    }
    xml$closeTag()
  }
  xml$closeTag()
  outXml <- cat(saveXML(xml))
  return (outXml)
}


# to.TS <- function(df){
#   cns <-names(df)
# df <- na.omit(df)
# #write.csv(df, 'c:/temp/ts.csv')

#   ts <- xts(x=df[,-1], order.by=as.POSIXlt(df[,1], format = "%Y-%m-%d %H:%M:%S"), tzone =  "Australia/Brisbane")
#   #ts <- xts(x=df[,-1],unique = FALSE, order.by=as.POSIXlt(df[,1], format = "%Y-%m-%d %H:%M:%S"), tzone =  Sys.getenv("TZ"))
#
#   colnames(ts) <- cns[-1]
#
#   return(ts)
# }



to.TS <- function(df){
  cns <-names(df)
  # change over of daylight saving causes this to throw an error if we don't get rid of resultant NAs in the index
  d <- as.POSIXct(str_trim(df$Date) , format = "%Y-%m-%d %H:%M:%S")
  dfdaylightsaving <- df[!is.na(d), ]

  ts <- xts(x=dfdaylightsaving[,-1],unique = FALSE, order.by=as.POSIXct(dfdaylightsaving[,1], format = "%Y-%m-%d %H:%M:%S"), tzone =  Sys.getenv("TZ"))
  #ts <- xts(x=df[,-1],unique = FALSE, order.by=as.Date(df[,1], format = "%Y-%m-%d %H:%M:%S"), tzone =  Sys.getenv("TZ"))

  colnames(ts) <- cns[-1]

  indexFormat(ts) <- "%Y-%m-%dT%H:%M:%S"

  return(ts)
}



to.DF <- function(ts){
  DF <- data.frame(DateTime=index(ts), coredata(ts),row.names=NULL)
}


cMax <- function(x){
  apply(x, 2, function(x) max(x, na.rm = TRUE))
}

cMin <- function(x){
  apply(x, 2, function(x) min(x, na.rm = TRUE))
}

cSum <- function(x){
  apply(x, 2, function(x) sum(x, na.rm = TRUE))
}

cMean<- function(x){
  apply(x, 2, function(x) mean(x, na.rm = TRUE))
}


resampleTS <- function(inTS, aggPeriod=timeSteps$day, ftype=timeAggMethods$none, startDate = startDate, endDate=endDate ){

  if(ftype==timeAggMethods$mean){
    outTS <- doAgg(inTS, aggPeriod, cMean, startDate=startDate, endDate = endDate)
    return(outTS)
  }else if(ftype==timeAggMethods$sum){
    outTS <- doAgg(ts=inTS, agg=aggPeriod, FUN=cSum, startDate=startDate, endDate = endDate)
    return(outTS)
  }else if(ftype==timeAggMethods$max){
    outTS <- doAgg(inTS, aggPeriod, cMax, startDate=startDate, endDate = endDate)
    return(outTS)
  }else if(ftype==timeAggMethods$min){
    outTS <- doAgg(inTS, aggPeriod, cMin, startDate=startDate, endDate = endDate)
    return(outTS)
  }else if(ftype==timeAggMethods$none){
    return(inTS)
  }else{
    return(inTS)
  }

}

resampleTS_Old <- function(inTS, aggPeriod=timeSteps$day, ftype=timeAggMethods$mean){

  ends <- endpoints(inTS,aggPeriod,1)


  if(ftype==timeAggMethods$mean){
    outTS <- period.apply(inTS,ends ,cMean)
    return(outTS)
  }else if(ftype==timeAggMethods$sum){
    outTS <- period.apply(inTS,ends ,cSum)
    return(outTS)
  }else if(ftype==timeAggMethods$max){
    outTS <- period.apply(inTS,ends ,cmax)
    return(outTS)
  }else if(ftype==timeAggMethods$min){
    outTS <- period.apply(inTS,ends ,cMin)
    return(outTS)
  }else if(ftype==timeAggMethods$none){
    return(inTS)
  }else{
    return(inTS)
  }

}

doAgg <- function(ts, agg, FUN, startDate, endDate ){

  aggP <- str_replace_all(agg, 's', '')
  exPeriod <- 0

  exPeriod <- as.numeric(timeStepDurations[agg][1])

  sd <- as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%S")
  ed <- as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%S") + exPeriod  ## bit of a hack allows us to go past the end date so we can query the last range

  #d1 <- as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%S" )
  dateSeq <- seq(sd, ed, by=paste0("1 ", aggP))
  mat <- matrix(data=NA, nrow = length(dateSeq)-1, ncol = ncol(ts))

  # the date reported for the aggregation is the end point of the ag interval
  repd <- character(length(index(dateSeq))-1)

  for (i in 1:(length(index(dateSeq))-1)) {
    d1 <- as.POSIXct(dateSeq[i], format = "%Y-%m-%dT%H:%M:%S")
    d2<- as.POSIXct(dateSeq[i+1]-1, format = "%Y-%m-%dT%H:%M:%S")


    p <- ts[paste0(d1,'/',d2),]
    repd[i] <- as.character(d2, format = "%Y-%m-%dT%H:%M:%S")

    for(j in 1:ncol(ts)){
      if(length(p) > 0){
        mat[i, j] <- FUN(p[,j])
      }
    }
  }
  dts <- xts(mat , as.POSIXct(repd, format = "%Y-%m-%dT%H:%M:%S"))
  #dts <- xts(mat , dateSeq)
  dts <- na.omit(dts)
  names(dts) <- names(ts)
  indexFormat(dts) <- "%Y-%m-%dT%H:%M:%S"


  ###  Fill missing dates with NA
  sdna <- as.POSIXct(repd[1], format = "%Y-%m-%dT%H:%M:%S")
  edna <- as.POSIXct(repd[length(repd)], format = "%Y-%m-%dT%H:%M:%S")
  dateSeq <- seq(sdna, edna, by=paste0("1 ", aggP))
  df2 <- merge(dts,zoo(NULL, dateSeq ), all=TRUE)

  return(df2)
}


doAggOld <- function(ts, agg, FUN, startDate, endDate ){

  aggP <- str_replace_all(agg, 's', '')
  exPeriod <- 0

  exPeriod <- as.numeric(timeStepDurations[agg][1])

  sd <- as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%S")
  ed <- as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%S") + exPeriod  ## bit of a hack allows us to go past the end date so we can query the last range

  d1 <- as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%S" )
  dateSeq <- seq(sd, ed, by=paste0("1 ", aggP))
  mat <- matrix(data=NA, nrow = length(dateSeq), ncol = ncol(ts))

  for (i in 1:length(index(dateSeq))-1) {
    d1 <- as.POSIXct(dateSeq[i], format = "%Y-%m-%dT%H:%M:%S")
    d2<- as.POSIXct(dateSeq[i+1]-1, format = "%Y-%m-%dT%H:%M:%S")
    #print(paste0(d1, '    ', d2))

    p <- ts[paste0(d1,'/',d2),]

    for(j in 1:ncol(ts)){
      if(length(p) > 0){
          mat[i, j] <- FUN(p[,j])
      }
    }
  }

  dts <- xts(mat , dateSeq)
  dts <- na.omit(dts)
  names(dts) <- names(ts)
  indexFormat(dts) <- "%Y-%m-%dT%H:%M:%S"


  ###  Fill missing dates with NA
  sdna <- as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%S")
  edna <- as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%S")
  dateSeq <- seq(sdna, edna, by=paste0("1 ", aggP))
  df2 <- merge(dts,zoo(NULL, dateSeq ), all=TRUE)

  #lead_x <- lag(df2, k = -1)

  return(df2)
}


resampleTS_old <- function(inTS, aggPeriod=timeSteps$day, ftype=timeAggMethods$mean){

  ends <- endpoints(inTS,aggPeriod,1)
  #print(ends)

  if(ftype==timeAggMethods$mean){
    outTS <- period.apply(inTS,ends ,cMean)
    return(outTS)
  }else if(ftype==timeAggMethods$sum){
    outTS <- period.apply(inTS,ends ,cSum)
    return(outTS)
  }else if(ftype==timeAggMethods$max){
    outTS <- period.apply(inTS,ends ,cmax)
    return(outTS)
  }else if(ftype==timeAggMethods$min){
    outTS <- period.apply(inTS,ends ,cMin)
    return(outTS)
  }else if(ftype==timeAggMethods$none){
    return(inTS)
  }else{
    return(inTS)
  }

}



getEmptySensorDF <- function(){

  sensorDF <- data.frame( SiteID = character(), SiteName = character(), Provider = character(), Organisation = character(),
                          Backend = character(), Access = character(), Usr = character(), pwd = character(),
                          Latitude = numeric(), Longitude=numeric(), serverName=character(), SensorID = character(),
                          sensorName = character(), StartDate = character(), EndDate = character(), DataType = character(),
                          UpperDepth = numeric(), LowerDepth = numeric(), Calibrated = logical(), Units = character(), stringsAsFactors = F )

}


