doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}

sendStatement <- function(con, sql){
  rs <- dbSendStatement(con, sql)
  dbHasCompleted(rs)
  dbGetRowsAffected(rs)
  dbClearResult(rs)
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

getTidyTS <- function(df, removeNA=T, enforceBounds=T){
  ts <- xts(df[,3], order.by=as.Date(df[,2]))

  if(enforceBounds){
    ts[ts<0] <- NA
    ts[ts>100] <- NA
  }

  ts <- na.trim(ts)

  if(removeNA){
    ts <-  na.omit(ts)
  }
  if(enforceBounds){
    ts[ts < 0] <-   NA
  }
  return(ts)
}


assessTSQuality <- function(ts=NULL, verbose=T, maxVal=80,  minVal=5, minNumDays=20, desiredNumDays=100){

  ret<-list()

  ret$QualityIndex=0
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

  if(all(is.na(ts))){
    ret$LowestIndexName='No valid values'
    return(ret)
  }
  if(nrow(ts) < minNumDays){
    ret$LowestIndexName=paste0('Number of values below minNumDays = ', minNumDays)
    return(ret)
  }

  tz <- na.omit(ts)

  if(sd(tz)==0){
    ret$LowestIndexName=paste0('All the values are the same')
    return(ret)}



  #Proportion of values in bounds
  inRangeCnt <- length(which( ts < maxVal & ts > minVal ))
  IinRange <- inRangeCnt/length(tz)

  #Proportion of valid values
  tsStart <- start(ts)
  tsEnd <- end(ts)
  tsNumDaysAll <- as.numeric(as.Date(tsEnd) - as.Date(tsStart))
  tsNumDaysValid <- length(tz)
  IvalidProp <- tsNumDaysValid/tsNumDaysAll

  # Gappiness
  gap_ts_Start=index(tz[ which( diff(index(ts))/24>1 )])
  gap_ts_End=index(tz[ which( diff(index(ts))/24>1 ) +1 ])
  #gapi <- as.numeric(sum(numdays) / length(ts)) * 100

  ngaps <- length(gap_ts_Start)
  #InumGaps <- ((length(ts)/2) - ngaps)/(length(ts)/2)
  InumGaps <- 1/ngaps

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
    ret$numGapDays <- sum(numdays)
    ret$numInRange <- inRangeCnt
    return(ret)
  }
}