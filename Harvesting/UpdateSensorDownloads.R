library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(DBI)
library(RSQLite)
library(stringr)


machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'FANCY-DP'){
  rootDir <-  'C:/Projects/SensorFederator'
  dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
}else {
  dbPath <- ""
}

dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")

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



dev = T

if(dev){
  serverPath <- "http://127.0.0.1:6863/SensorAPI"
}else{
  serverPath <- "https://esoil.io/SensorFederationWebAPI/SensorAPI"
}



sendRequest <- function(url, rtype){

  outcome = list()
  ue <- URLencode(url)
  #h <- handle_reset(url)
  resp <- GET(ue)

  if(http_error(resp)){
    outcome$OK = F
    outcome$Error = 'ERROR - HTTP error'
    outcome$Data = NULL
    return(outcome)
  }

  response <- content(resp, "text", encoding = 'UTF-8')
  stream <- fromJSON(response)

  if(!is.null(stream$error)){
    outcome$OK = F
    outcome$Error = paste0('ERROR - API Error : ', stream$error)
    outcome$Data = NULL
    return(outcome)
  }


  if(rtype == 'DataStream'){

    df <-convertJSONtoDF(response)


    if(nrow(df) == 0){
      outcome$OK = F
      outcome$Error = paste0('ERROR - No records found')
      outcome$Data = NULL
      return(outcome)

    }else{
      outcome$OK = T
      outcome$Error = paste0('None')
      outcome$Data = df

      outcome$siteID <- stream$SiteID[1]
      outcome$sensorID <- stream$SensorID[1]
      outcome$upperDepth <- stream$UpperDepthCm[1]
      outcome$lowerDepth <- stream$LowerDepthCm[1]
      outcome$ datatype<- stream$DataType[1]

      return(outcome)
    }

  }else{
    d <- fromJSON(response)
    if(nrow(d) == 0){
      outcome$OK = F
      outcome$Error = paste0('ERROR - No records found')
      outcome$Data = NULL
      return(outcome)

    }else{
      outcome$OK = T
      outcome$Error = paste0('None')
      outcome$Data = d
      return(outcome)
    }
  }
}

pingSensor <- function(rec, logName, serverPath, sDate, eDate){

  sid = rec$SiteID
  senID <- rec$SensorID
  ud <- rec$UpperDepth
  ld <- rec$LowerDepth
  dtype <- rec$DataType
  siteName <- rec$SiteName
  senGrp <- rec$SensorGroup
  BE <- rec$Backend

    url <- paste0(serverPath, '/getSensorDataStreams?siteid=', sid, '&sensorid=', senID, '&sensortype=', dtype, '&startdate=', sDate ,'&enddate=', eDate ,'&aggperiod=days')

    print(url)
    r <- sendRequest(url, 'DataStream')
    if(!r$OK){
      cat(paste0('Error, ', r$Error, ',', url, ', ', sid, ', ', siteName, ', ', senID, ', ', dtype, ', ', BE, ', ', senGrp, '\n'), file = logName, append = T)
      return(NULL)
    }
    df <- r$Data

    allts <- to.TS(df)
    tr <- na.trim(allts)
    tr[tr==-99] <- NA
    siteID <- r$siteID[1]
    sensorID <- r$sensorID[1]
    upperDepth <- r$upperDepth[1]
    lowerDepth <- r$lowerDepth[1]
    prop <- r$datatype[1]
    outDF <- data.frame(dt=index(tr), coredata(tr)[,1],row.names=NULL, stringsAsFactors = F)
    colnames(outDF)[2] <- paste0(prop, '_', upperDepth, '_', lowerDepth)
    #print(outFile)
    #write.csv(outDF, paste0(outFile), row.names = F)

    cat(paste0('OK, ', r$Error, ',', url, ', ', sid, ', ', siteName, ', ', senID, ', ', dtype, ', ', BE, ', ', senGrp, '\n'), file = logName, append = T)
    return(outDF)
}



getOutputFilename <- function(rec, outDataDir){

    sid = rec$SiteID
    senID <- rec$SensorID
    ud <- rec$UpperDepth
    ld <- rec$LowerDepth
    dtype <- rec$DataType
    siteName <- rec$SiteName
    senGrp <- rec$SensorGroup
    BE <- rec$Backend
    outFile <- paste0(outDataDir, '/', sid, '!', senID, '!',  ud, '!', ld, '!', dtype, '.csv')

    return(outFile)
}





conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RO)
sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE Sensors.IsActive = 'TRUE' and Sites.SensorGroup<>'BoM'"
sensors <- doQuery(conFed, sql)
dbDisconnect(conFed)


sDate <- '2010-01-01T00:00:00'
eDate <- paste0(Sys.Date(), 'T23:59:59')


outDataDir <- paste0(rootDir, '/dataDumps/at_', Sys.Date())
if(!dir.exists(outDataDir)){dir.create(outDataDir, recursive = T)}

logName <- paste0(outDataDir, '/UpdateProcessingLog.csv')
cat('Outcome, Error, URL, SiteID, SiteName, SensorID, DataType, Backend, SensorGroup, \n', file = logName)

conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)


pb <- pbCreate(nrow(sensors), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

maxRetry=1


for (i in 1:nrow(sensors)) {

  sensorRec <- sensors[i,]
  sqlsens <- paste0("select * from Sensors where  SiteID='", sensors$SiteID[i], "' and SensorID='", sensors$SensorID[i], "' and upperDepth='", sensors$UpperDepth[i], "' and lowerDepth='", sensors$LowerDepth[i], "' and DataType='", sensors$DataType[i], "'")
  existingSensorData <- doQuery(conStore, sqlsens)
  sNum <- existingSensorData$sensorNum

  outFn <- getOutputFilename(sensorRec, outDataDir)
  ##########   grab the latest data from the sensorFederator  ########################################

  sql <-  paste0("SELECT * from sensorData where sensorNum = " , sNum  )
  recs <- doQuery(conStore, sql)

  if(nrow(dts) > 0){
    sDate <- paste0(max(as.Date(recs$dateTime)), 'T00:00:00')
  }

    r <- NULL
    attempt <- 1
    while( is.null(r) && attempt <= maxRetry ) {
      attempt <- attempt + 1
      Sys.sleep(1)
      try(
        r <- pingSensor(rec=sensorRec, logName=logName, serverPath = serverPath, sDate = sDate, eDate = eDate  )
      )
    }

    write.csv(r, outFn)

    # sdate <- tsMax
    # edate <- paste0(Sys.Date())
    #
    # newData <- ts[paste0(sdate, "/", edate)]
    # sdf <- data.frame( siteNum=sNum, dateTime=as.character(index(newData)), value=coredata(newData))
    # dbWriteTable(con, "sensorData", sdf, append = TRUE)

  }





pbClose(pb)






