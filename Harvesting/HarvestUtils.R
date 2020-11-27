library(xts)
library(DBI)
library(RSQLite)


getPeriod <- function(ts){

  data.frame(startDate=start(ts), endDate=end(ts), stringsAsFactors = F)
}

gapiness <- function(ts){
  tz <- na.omit(ts)
  gap_ts_Start=index(tz[ which( diff(index(tz))>1 )])
  gap_ts_End=index(tz[ which( diff(index(tz))>1 ) +1 ])

  ngaps <- length(gap_ts_Start)
  numdays <- gap_ts_End - gap_ts_Start
  gapi <- as.numeric(sum(numdays) / length(ts)) * 100

  return(data.frame(numGaps=ngaps, numDays=as.numeric(sum(numdays)), gapiness=gapi, stringsAsFactors = F))
}

sensorIsActive <- function(ts, days){

  lastRec <- as.Date(end(ts))
  if(Sys.Date()-lastRec > days){
    return('FALSE')
  }else{
    return('TRUE')
  }

}

doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}

sendStatement <- function(con, sql){
  rs <- dbSendStatement(con, sql)
  dbHasCompleted(rs)
  nrec <- dbGetRowsAffected(rs)
  dbClearResult(rs)
  return(nrec)
}



getSensorData <- function(conStore, sensorNum, sdate=NULL, edate=NULL){
  sql <- paste0("SELECT sensorNum, datetime(sensorData.dateTime) as dt, sensorData.value
      FROM SensorData
      WHERE SensorData.sensorNum = ", sensorNum,"")
  if(is.null(sdate)){
    sql <- paste0(sql,  " ORDER BY dt;")
  }else{
    sql <- paste0(sql,  "  and dt  between '",sdate,"' and '",edate,"' ORDER BY dt;")
  }
  #print(sql)
  df <-  doQuery(conStore, sql)

  return(df)
}

getsensors <- function(conFed, dataType=NULL){
  sql <- paste0("SELECT * from sensors")
  if(!is.null(dataType)){
    sql <- paste0(sql,  "  where dataType = '", dataType, "'")
  }
  df <- doQuery(conFed, sql)
  return(df)
}

getTidyTS <- function(ts, removeNA=T, upperBound=NULL, lowerBound=NULL, flatCnt=NULL, removeOutliers=NULL, removeStartDays=NULL){

  ######   exlude values outside bounds
  if(!is.null(upperBound)){
    ts[ts>upperBound] <- NA
  }
  if(!is.null(lowerBound)){
    ts[ts<lowerBound] <- NA
  }

  if(all(is.na(ts))){
    return(NULL)
  }

  ######  remove flat spot values
  if(!is.null(flatCnt)){
    sdf <- aggregate(data.frame(count = its), list(value = its$value), length)
    colnames(sdf) <- c('value', 'count')
    sorteddata <- sdf[order(-sdf$count),]
    remVals <- sorteddata[sorteddata$count >= flatCnt, ]$value
    if(length(remVals) > 0){
      for (i in 1:length(remVals)) {
        ts[ts==remVals[i]] <- NA
      }
    }
  }

  if(all(is.na(ts))){
    return(NULL)
  }

  ######   remove outliers
  if(!is.null(removeOutliers)){

    # lq <- quantile(its, probs = c(0.01), na.rm=T)
    # lb <- as.numeric(lq[1])
    # its[its<lb] <- NA

    i <- tsoutliers(ts)
    ts[i$index] <- NA
  }

  if(all(is.na(ts))){
    return(NULL)
  }

  if(!is.null(removeStartDays)){
    ts <- ts[removeStartDays:nrow(ts)]
  }


  if(removeNA){
    ts <-  na.omit(ts)
  }

  if(all(is.na(ts))){
    return(NULL)
  }

  return(ts)
}



getBlankQualityRecord <- function(){

  ret<-list()

  ret$QualityIndex=0
  ret$isValidTS='FALSE'
  ret$LowestIndexName = ''
  ret$LowestIndexValue = 0
  ret$IinRange <- 0
  ret$IvalidProp <- 0
  ret$InumGaps <- 0
  ret$ItotalGapsDays <- 0
  ret$IdesiredNumDays <- 0
  ret$Start <- '0001-01-01'
  ret$End <- '0001-01-01'
  ret$TotalNumDays <- 0
  ret$TotalValidDays <- 0
  ret$validProportion <- 0
  ret$numGaps <- 0
  ret$numGapDays <- 0
  ret$numInRange <- 0
  ret$StandardDeviation <- 0
  ret$MeanValue <- 0
  ret$IsActive <- 'FALSE'
  ret$HarvestDate <- Sys.Date()
  ret$HasData = 'FALSE'
  ret$MinValue = 0
  ret$MaxValue = 0

  return(ret)
}

getQualitySQL <- function(q){

  sqlUp <- paste0("UPDATE sensors
                          SET StartDate = '", q$Start , "', ",
                  "EndDate = '", q$End, "', ",
                  "TotalNumDays = ", q$TotalNumDays, ", ",
                  "TotalValidDays = ", q$TotalValidDays, ", ",
                  "validProportion = ", q$validProportion, ", ",
                  "QualityIndex = ", q$QualityIndex, ", ",
                  "LowestIndexName = '", q$LowestIndexName, "', ",
                  "LowestIndexValue = ", q$LowestIndexValue, ", ",
                  "IinRange = ", q$IinRange, ", ",
                  "IvalidProp = ", q$IvalidProp, ", ",
                  "InumGaps = ", q$InumGaps, ", ",
                  "ItotalGapsDays = ", q$ItotalGapsDays, ", ",
                  "IdesiredNumDays = ", q$IdesiredNumDays, ", ",
                  "NumGaps = ", q$numGaps, ", ",
                  "NumGapDays = ", q$numGapDays, ", ",
                  "NumInRange = ", q$numInRange, ", ",
                  "StandardDeviation = ", q$StandardDeviation, ", ",
                  "MeanValue = ", q$MeanValue, ", ",
                  "MinimumValue = ", q$MaxValue , ", ",
                  "MaximumValue = ", q$MeanValue, ", ",
                  "IsActive = '", q$IsActive, "', ",
                  "HarvestDate = '", q$HarvestDate, "', ",
                  "hasdata = '", q$HasData ,"' ",
                  "WHERE SiteID='", rec$SiteID, "' and SensorID='", rec$SensorID, "' and upperDepth='", rec$UpperDepth, "' and lowerDepth='", rec$LowerDepth, "' and DataType='", rec$DataType, "';")

  return(sqlUp)
}

assessTSQuality <- function(ts=NULL, verbose=T, maxVal=NULL,  minVal=NULL, minNumDays=NULL, desiredNumDays=NULL, numDaysForActive=NULL){

ret <- getBlankQualityRecord()

  tz <- na.omit(ts)
  if(nrow(tz)==0){
    ret$LowestIndexName='No valid values'
    return(ret)
  }

  if(nrow(tz) < minNumDays){
    ret$LowestIndexName=paste0('Number of values below minNumDays = ', minNumDays)
    return(ret)
  }

  tsSD <- sd(tz)
  if(tsSD==0){
    ret$LowestIndexName=paste0('All the values are the same')
    return(ret)}



  #Proportion of values in bounds
  inRangeCnt <- length(which( ts <= maxVal & ts >= minVal ))
  IinRange <- inRangeCnt/length(tz)

  #Proportion of valid values
  tsStart <- start(ts)
  tsEnd <- end(ts)
  tsNumDaysAll <- as.numeric(as.Date(tsEnd) - as.Date(tsStart))
  tsNumDaysValid <- length(tz)
  IvalidProp <- tsNumDaysValid/tsNumDaysAll

  # Gappiness
  gap_ts_Start=index(tz[ which( diff(index(tz))>1 )])
  gap_ts_End=index(tz[ which( diff(index(tz))>1 ) +1 ])
  #gapi <- as.numeric(sum(numdays) / length(ts)) * 100

  ngaps <- length(gap_ts_Start)
  InumGaps <- ((length(ts)/2) - ngaps)/(length(ts)/2)

  numdays <- gap_ts_End - gap_ts_Start
  ItotalGapsDays <- (length(ts) - as.numeric(sum(numdays))) / length(ts)

  # TS Numdays as a proprtion of desired
  IdesiredNumDays <- min(tsNumDaysValid/desiredNumDays, 1)


  # Overall quality indice
  indiceNames <- c('inRange' , 'validProp', 'numGaps', 'totalGapsDays', 'desiredNumDays')
  indices <- c(IinRange , IvalidProp, InumGaps, ItotalGapsDays, IdesiredNumDays)
  Itot <- sum(indices)/length(indices)
  lowestIndName <- indiceNames[which(indices == min(indices))]
  lowestInd <- indices[which(indices == min(indices))]

  ret$QualityIndex=Itot
  ret$LowestIndexName = lowestIndName[1]
  ret$LowestIndexValue = lowestInd[1]

  if(!verbose){
    return(ret)
  }else{
    ret$IinRange <- IinRange
    ret$IvalidProp <- IvalidProp
    ret$InumGaps <- InumGaps
    ret$ItotalGapsDays <- ItotalGapsDays
    ret$IdesiredNumDays <- IdesiredNumDays
    ret$Start <- tsStart
    ret$End <- tsEnd
    ret$TotalNumDays <- tsNumDaysAll
    ret$TotalValidDays <- tsNumDaysValid
    ret$validProportion <- IvalidProp
    ret$numGaps <- ngaps
    ret$numGapDays <- as.numeric(sum(numdays))
    ret$numInRange <- inRangeCnt
    ret$StandardDeviation <- tsSD
    ret$MeanValue <- mean(tz)
    ret$IsActive <- sensorIsActive(tz, numDaysForActive)
    ret$isValidTS='TRUE'
    ret$HasData = 'TRUE'
    ret$MinValue = min(tz)
    ret$MaxValue = max(tz)
    return(ret)

  }
}


