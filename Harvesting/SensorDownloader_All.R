library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(DBI)
library(RSQLite)


dev = T
machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'FANCY-DP'){
  rootDir <-  'C:/Projects/SensorFederator'
  dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backends.R')
}else {
  dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
}

if(dev){
  serverPath <- "http://127.0.0.1:6740/SensorAPI"
}else{
  serverPath <- "http://esoil.io/SensorFederationWebAPI/SensorAPI"
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

pingSensorFromAPI <- function(rec, outDir, logName, serverPath){

  sid = rec$SiteID
  senID <- rec$SensorID
  ud <- rec$UpperDepth
  ld <- rec$LowerDepth
  dtype <- rec$DataType
  siteName <- rec$SiteName
  senGrp <- rec$SensorGroup
  BE <- rec$Backend


  outFile <- paste0(outDir, '/', sid, '!', senID, '!',  ud, '!', ld, '!', dtype, '.csv')

  if(!file.exists(outFile)){

    url <- paste0(serverPath, '/getSensorDataStreams?siteid=', sid, '&sensorid=', senID, '&sensortype=', dtype, '&startdate=', sDate ,'&enddate=', eDate ,'&aggperiod=days', '&usr=Admin&pwd=JWEJTOhwCuH8sQEKD2ft4KAPg')

    #print(url)
    r <- sendRequest(url, 'DataStream')
    if(!r$OK){
      cat(paste0('Error, ', r$Error, ',', url, ', ', sid, ', ', siteName, ', ', senID, ', ', dtype, ', ', BE, ', ', senGrp, '\n'), file = logName, append = T)
      return('Error')
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
    print(outFile)
    write.csv(outDF, paste0(outFile), row.names = F)
    cat(paste0('OK, ', r$Error, ',', url, ', ', sid, ', ', siteName, ', ', senID, ', ', dtype, ', ', BE, ', ', senGrp, '\n'), file = logName, append = T)
    return('OK')
  }
  return('FileExists')
}


pingSensorFromDB <- function(rec, outDir, logName){

  # sid = rec$SiteID
  # senID <- rec$SensorID
  # ud <- rec$UpperDepth
  # ld <- rec$LowerDepth
  # dtype <- rec$DataType
  # siteName <- rec$SiteName
  # senGrp <- rec$SensorGroup
  # BE <- rec$Backend


  outFile <- paste0(outDir, '/', rec$SiteID, '!', rec$SensorID, '!',  rec$UpperDepth, '!', rec$LowerDepth, '!', rec$DataType, '.csv')

  if(!file.exists(outFile)){

    allts <- getSensorData(streams=rec , startDate = sDate, endDate = eDate, aggPeriod=timeSteps$days, numrecs=maxRecs, outFormat='simpleTS')


    # url <- paste0(serverPath, '/getSensorDataStreams?siteid=', sid, '&sensorid=', senID, '&sensortype=', dtype, '&startdate=', sDate ,'&enddate=', eDate ,'&aggperiod=days', '&usr=Admin&pwd=JWEJTOhwCuH8sQEKD2ft4KAPg')
    #
    # #print(url)
    # r <- sendRequest(url, 'DataStream')
    # if(!r$OK){
    #   cat(paste0('Error, ', r$Error, ',', url, ', ', sid, ', ', siteName, ', ', senID, ', ', dtype, ', ', BE, ', ', senGrp, '\n'), file = logName, append = T)
    #   return('Error')
    # }
    # df <- r$Data


    tr <- na.trim(allts)
    tr[tr==-99] <- NA
    siteID <- rec$SiteID
    sensorID <- rec$SensorID
    upperDepth <- rec$UpperDepth
    lowerDepth <- rec$LowerDepth
    prop <- rec$DataType
    outDF <- data.frame(dt=index(tr), coredata(tr)[,1],row.names=NULL, stringsAsFactors = F)
    colnames(outDF)[2] <- paste0(prop, '_', upperDepth, '_', lowerDepth)
    print(outFile)
    write.csv(outDF, paste0(outFile), row.names = F)
    cat(paste0('OK, ', '', ',', '', ', ', rec$SiteID, ', ', rec$SiteName, ', ', rec$SensorID, ', ', rec$DataType, ', ', rec$Backend, ', ', rec$SensorGroup, '\n'), file = logName, append = T)
    return('OK')
  }
  return('FileExists')
}



grp <- 'GRDCWASoilWaterProbes'

sDate <- '2010-01-01T00:00:00'
eDate <- paste0(Sys.Date(), 'T23:59:59')

sql <- paste0("SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE sites.SensorGroup = '", grp,"'")
# sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE sites.Access = 'Public' and Sites.SensorGroup<>'BoM'"
# sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE sites.SensorGroup = 'SFS'"

logName <- paste0(rootDir, '/ProcessingLog_', grp, '.csv')
cat('Outcome, Error, URL, SiteID, SiteName, SensorID, DataType, Backend, SensorGroup, \n', file = logName)

con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RO)



outDir <- paste0(rootDir, '/DataDumps/', grp)
if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}

res <- dbSendQuery(con, sql)
sensors <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con)


pb <- pbCreate(nrow(sensors), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

maxRetry=1
for (i in 1:nrow(sensors)) {

  rec = sensors[i,]
  outFile <- paste0(outDir, rec$SiteID, '!', rec$SensorID, '!',  rec$UpperDepth, '!', rec$LowerDepth, '!', rec$DataType, '.csv')

  cntr <- cntr + 1
  pbStep(pb, step=cntr)

      if(!file.exists(outFile)){

          print(paste0(rec$SiteName, ' ', rec$SensorGroup, ' ', rec$Backend, ' ', rec$DataType))
          r <- NULL
          attempt <- 1
          while( is.null(r) && attempt <= maxRetry ) {
            attempt <- attempt + 1
            Sys.sleep(0.5)
            try(
              #r <- pingSensorFromAPI(rec=rec, outDir=outDir, logName=logName, serverPath = serverPath)
              r <- pingSensorFromDB(rec=rec, outDir=outDir, logName=logName)
            )
          }
          if(attempt==maxRetry){
            cat(paste0('ExceededMaxTry, ', 'Couldnt communicate with the server', ',', 'None', ', ', rec$SiteID, ', ', rec$SiteName, ', ', rec$SensorID, ', ', rec$DataType, ', ', rec$Backend, ', ', rec$SensorGroup, '\n'), file = logName, append = T)
            print(paste0('ExceededMaxTry : ', rec$SiteID, ', ', rec$SiteName, ', ', rec$SensorID, ', ', rec$DataType, ', ', rec$Backend, ', ', rec$SensorGroup, '\n'))
          }
      }else{
        cat(paste0('FileExists, ', '', ',', 'None', ', ', rec$SiteID, ', ', rec$SiteName, ', ', rec$SensorID, ', ', rec$DataType, ', ', rec$Backend, ', ', rec$SensorGroup, '\n'), file = logName, append = T)
        print(paste0('FileExists, : ', rec$SiteID, ', ', rec$SiteName, ', ', rec$SensorID, ', ', rec$DataType, ', ', rec$Backend, ', ', rec$SensorGroup, '\n'))
        }
}

pbClose(pb)






