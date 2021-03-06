


# All of the settings are contained in Backend_Config.R

#sitesInfo <- read.csv(paste0(sensorRootDir, '/SensorInfo/AllSites.csv'), stringsAsFactors = F)

#' Retrieve a timeseries of sensor data
#'
#' Gets a timeseries of sensor data as a dataframe from a given sensor platform
#'
#' @param streamID The unique ID used to identify the sensor - taken from the sensor metadata
#' @param backEnd The telemetry platform to query
#' @param startDate Start date of data stream to retrieve in dd-mm-yyyy format
#' @param endDate End date of data stream to retrieve in dd-mm-yyyy format
#' @param aggregSeconds Aggregation time step in seconds
#' @param numrecs Total number of records to return
#'
#' @return DataFrame containing date and data columns
#'
#' @examples
#' getSensorData('cerdi.sfs.4935.stream.4983.soil-moisture.1000mm')
#'
#' @export
getSensorData <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$days, numrecs=maxRecs, outFormat='simpleTS', verbose=F, tempCorrect=NULL ){

  out <- tryCatch({



    backEnd <- streams$Backend[[1]][1]


    if(backEnd %in% knownBackends ){

      dnowYMD <- format(Sys.time(), "%Y-%m-%d")
      #### generate dates in ISO if none supplied
      if(is.null(endDate))
      {
        isoEDate <- paste0(dnowYMD, 'T', defaultStartTime, '')
      }else{

        # check to see supplied edate is not greater than today - if so fix it
        bits <- str_split(endDate, 'T')
        dnowYMD <- format(Sys.time(), "%Y-%m-%d")
        dnowPos <- as.POSIXct(dnowYMD)
        endDatePos <- as.POSIXct(bits[[1]][1])
        dtdiff <- endDatePos-dnowPos
        ndiff <- as.numeric(dtdiff, units = "days")

        if(ndiff > 0){
          isoEDate <- paste0(dnowYMD, 'T', bits[[1]][2])
        }else{
          isoEDate <- endDate
        }
      }

      if(is.null(startDate))
      {
        if(is.null(endDate))
        {
          ed <- format(Sys.time(), "%Y-%m-%d")
          ed <- paste0(ed,  'T', defaultStartTime, '')
        }else{
          ed <- endDate
        }
        edp <- strptime(ed, "%Y-%m-%dT%H:%M:%S")
        py <- edp - 31536000
        #as.character(py)
        isoSDate <- str_replace_all(as.character(py), ' ', 'T')
      }else{
        isoSDate <- startDate
      }




      # send the request off to the various backends

      if(backEnd == 'Senaps'){
        dfTS <- getSensorData_Senaps(streams=streams, startDate=isoSDate, endDate=isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'Adcon') {
        dfTS <- getSensorData_Adcon(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'OutPost') {
        dfTS <- getSensorData_Outpost(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'Outpost2') {
        dfTS <- getSensorData_Outpost2(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'Cosmoz') {
        dfTS <- getSensorData_Cosmoz(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'DAFWA') {
        dfTS <- getSensorData_DAFWA(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'Mait') {
        dfTS <- getSensorData_Mait(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'DataFarmer') {
        dfTS <- getSensorData_DataFarmer(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'SenFedStore') {
        dfTS <- getSensorData_SenFedStore(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'IOT_CERDI') {
        dfTS <- getSensorData_IOT(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'BoM_Latest') {
        dfTS <- getSensorData_BoMLatest(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'SILO') {
        dfTS <- getSensorData_SILO(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs )
      }else if(backEnd == 'EPARF') {
        dfTS <- getSensorData_EPARF(streams=streams, startDate=isoSDate, endDate = isoEDate, aggPeriod=aggPeriod, numrecs=numrecs, tempCorrect )
      }



      #### This deals with the situation where a sensor returns a blank list
      #### We need to get rid of these out of the list of data frames and the recs in the sensor dataframe
      outSensors <- data.frame()
      for (i in 1:length(dfTS)) {
        #print(head(dfTS[[i]]))
        if(length( dfTS[[i]]) !=0){
          if(nrow( dfTS[[i]]) !=0){
            #print(paste0('i = ',  i))
            outSensors <- rbind(outSensors, streams[i,])
          }
        }
      }
      #print(outSensors)
      nnl <- delete.NULLs(dfTS)
      nnl[sapply(nnl, function(x) dim(x)[1]) > 0]
      #print(nnl)
      #dfTSm <- mergedfTSList(nnl, streams = streams)
      dfTSm <- mergedfTSList(nnl, streams = outSensors)


      if(nrow(dfTSm) > 0){
        outts <- to.TS(dfTSm)
        if(aggPeriod != 'none'){
          outTS <- resampleTS(inTS=outts, aggPeriod=aggPeriod, ftype=FeatureAggTypes[streams$DataType][1], startDate=isoSDate, endDate = isoEDate)
        }else{
          outTS <- outts
        }

        #print(numrecs)
        #outTS <- tail(outTS, numrecs)

        if(outFormat=='nestedTS'){
          ndf <- makeNestedDF(outTS, outSensors, isoSDate, isoEDate, aggPeriod, verbose)
          return(ndf)
        }else{
          return(outTS)
        }
      }else{
        return(NULL)
      }

    }else{

      stop(paste0('Backend "' , backEnd, '" is not currently supported'), call. = F)
    }

  }, error = function(e)
  {

    stop(geterrmessage())
  }

  )

}




getSensorData_SenFedStore <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){


  # isoSDate <- as.Date(startDate, format = "%Y-%m-%d")
  # isoEDate <- as.Date(endDate, format = "%Y-%m-%d")

  # sd <- as.POSIXct(startDate, format="%Y-%m-%d %H:%M:%S")
  # ed <- as.POSIXct(endDate, format="%Y-%m-%d %H:%M:%S")

  sd <- str_replace(startDate, 'T', ' ')
  ed <- str_replace(endDate, 'T', ' ')
  dtype <- streams$DataType[1]
  site <- str_split(streams$SiteID[1], '_')[[1]][2]
  dataStreamsDF <- list(nrow(streams))

  for (i in 1:nrow(streams)) {
    dataStreamsDF[[i]] <- getData_SenFedStore(sid=site, datatype = dtype, sensorID = streams$SensorID[i], sdate = sd, edate = ed )
  }

  # tryCatch({
  #   dataStreamsDF <-  getData_SenFedStore
  #   #dataStreamsDF <- synchronise(async_map( urls,  getURLAsync_Mait, .limit = asyncThreadNum ))
  # }, error = function(e)
  # {
  #   stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  # })

  return(dataStreamsDF)
}



getSensorData_DataFarmer <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){


  isoSDate <- as.Date(startDate, format = "%Y-%m-%d")
  isoEDate <- as.Date(endDate, format = "%Y-%m-%d")

  siteid <- str_split(streams$SiteID[1], '_')[[1]][2]
  networkID <- streams$SensorGroup[1]
  usr = streams$Usr[1]
  pwd = streams$Pwd[1]

  url <- paste0('https://datafarmer.com.au/ServiceV1/DataFarmerService.svc/GetWeatherReadings?tkn=', usr ,'&aptkn=', pwd,'&dvc=', siteid,'&sd=', isoSDate ,'&ed=', isoEDate)


  tryCatch({
    dataStreamsDF <-  getURL_DataFarmer(url, streams)
    #dataStreamsDF <- synchronise(async_map( urls,  getURLAsync_Mait, .limit = asyncThreadNum ))
  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })

  return(dataStreamsDF)
}


getSensorData_Mait <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  isoSDate <-  as.Date(str_replace_all(as.Date(startDate, format = "%Y-%m-%d"), '-', '/'))
  isoEDate <-  as.Date(str_replace_all(as.Date(endDate, format = "%Y-%m-%d"), '-', '/'))


  fromDt <- seq.Date(isoSDate, isoEDate, by='year')
  toDt <- c(fromDt[-1] -1, isoEDate)

  siteid <- str_remove(streams$SiteID, paste0(streams$SensorGroup, '_'))

  sensorid <- streams$SensorID[1]
  bits <- str_split(sensorid, '_')

  moduleID <- bits[[1]][1]
  sensorName <- bits[[1]][2]

  nbits <- str_split(streams$SensorGroup, '_')
  networkID <- nbits[[1]][2]
  apiRoot <- streams$ServerName[1]

  print(networkID)
  urls <- paste0(apiRoot, "/getdata?network=", networkID, "&module=", moduleID ,"&startdate=", fromDt, "&enddate=", toDt, '|', streams$Usr[1], '|', streams$Pwd[1], '|', paste0(streams$SensorName, collapse = ";"))
  #url <- paste0(apiRoot, "/getdata?network=", networkID, "&module=", moduleID ,"&startdate=", fromDt, "&enddate=", toDt, '|', streams$Usr[1], '|', streams$Pwd[1], '|', paste0(streams$SensorName, collapse = ";"))





  dataStreamsDF <- synchronise(async_map( urls,  getURLAsync_Mait, .limit = asyncThreadNum ))



  o <- vector("list", length = length(streams$SensorName))
  for (i in 1:length(streams$SensorName)) {

    for (j in 1:length(dataStreamsDF)) {

      if(j==1){
        odf <- as.data.frame(dataStreamsDF[[j]][i])
      }else{
        odf <- rbind(odf, as.data.frame(dataStreamsDF[[j]][i]))
      }
    }
    odf[odf==-950] <- NA
    o[[i]] <- odf
  }


  return(o)



}



getSensorData_DAFWA <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  # https://api.agric.wa.gov.au/v1/weatherstations/dailysummary.json?station_code=', 'BR', '&fromDate=2016-01-01&toDate=2016-09-29&api_key=CCB3F85A64008C6AC1789E4F.apikey

  isoSDate <- as.Date(startDate)
  isoEDate <- as.Date(endDate)

  fromDt <- seq.Date(isoSDate, isoEDate, by='year')
  toDt <- c(fromDt[-1] -1, isoEDate)

  siteid <- str_remove(streams$SiteID, paste0(streams$SensorGroup, '_'))

  # urls <- paste0( streams$ServerName, '/weatherstations/dailysummary.json?station_code=',siteid, '&fromDate=',isoSDate,'&toDate=',isoEDate ,'&api_key=CCB3F85A64008C6AC1789E4F.apikey')
  urls <- paste0( streams$ServerName, '/weatherstations/dailysummary.json?station_code=',siteid, '&fromDate=',fromDt,'&toDate=',toDt ,'&api_key=CCB3F85A64008C6AC1789E4F.apikey')
  #print(urls)
  tryCatch({
    dataStreamsDF <- synchronise(async_map( urls,  getURLAsync_DAFWA, .limit = asyncThreadNum ))
  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })


  allDF <- do.call("rbind", dataStreamsDF)
  o <- list()
  o[[1]] <- allDF
  return(o)
}


getSensorData_Cosmoz <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  siteid <- str_remove(streams$SiteID, paste0(streams$SensorGroup, '_'))

  if(str_to_lower(streams$DataType) == 'soil-moisture'){
    filt <- 'soil_moist_filtered'
  }else{
    filt <- 'rainfall'
  }
  server <- 'https://esoil.io/cosmoz-data-pipeline'
  #urls <- paste0( streams$ServerName, '/rest/station/', siteid, '/records?processing_level=4', '&startdate=',startDate,'Z&enddate=',endDate ,'Z&property_filter=', filt,  '&count=', format(numrecs, scientific = FALSE) , '&offset=0')
  urls <- paste0( server, '/rest/stations/', siteid, '/observations?processing_level=4', '&startdate=',startDate,'Z&enddate=',endDate ,'Z&property_filter=', filt,  '&count=', format(numrecs, scientific = FALSE) , '&offset=0')
#print(urls)

  tryCatch({
    dataStreamsDF <- synchronise(async_map( urls,  getURLAsync_Cosmoz, .limit = asyncThreadNum ))

  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })
  return(dataStreamsDF)
}


getSensorData_Senaps <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  isoSDate <- paste0(startDate, '.000Z')
  isoEDate <- paste0(endDate, '.000Z')


  if(streams$SensorGroup[1]=='Booroowa' & streams$DataType[1]=='Soil-Moisture'){

    urlsDEC <- paste0( streams$ServerName, '/observations?streamid=', streams$SensorID,'&start=',isoSDate,'&end=',isoEDate , '&limit=', format(numrecs, scientific = FALSE), '|', streams$Usr[1], '|', streams$Pwd[1])
    senTemps <- str_replace(streams$SensorID, 'dielectric_constant', 'soil_temperature')
    urlsTemp <- paste0( streams$ServerName, '/observations?streamid=', senTemps,'&start=',isoSDate,'&end=',isoEDate , '&limit=', format(numrecs, scientific = FALSE), '|', streams$Usr[1], '|', streams$Pwd[1])

    dataStreamsDFDEC <- synchronise(async_map(urlsDEC, getURLAsync_Senaps, .limit = asyncThreadNum))
    dataStreamsDFTEMP <- synchronise(async_map(urlsTemp, getURLAsync_Senaps, .limit = asyncThreadNum))

    for(i in 1:length(dataStreamsDFDEC)){
      ErT=as.numeric(dataStreamsDFDEC[[i]]$Values)*1.011/(1.045+0.01*as.numeric(dataStreamsDFTEMP[[i]]$Values))
      dataStreamsDFDEC[[i]]$Values <- (0.055  * sqrt(ErT) + -0.015) * 100
    }

    return(dataStreamsDFDEC)

  }else{
      urls <- paste0( streams$ServerName, '/observations?streamid=', streams$SensorID,'&start=',isoSDate,'&end=',isoEDate , '&limit=', format(numrecs, scientific = FALSE), '|', streams$Usr[1], '|', streams$Pwd[1])

      tryCatch({
        dataStreamsDF <- synchronise(async_map(urls, getURLAsync_Senaps, .limit = asyncThreadNum))
      }, error = function(e)
      {
        stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
      }
      )

       return(dataStreamsDF)
  }
}


getSensorData_Adcon <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  sd <- as.POSIXct(startDate, format="%Y-%m-%dT%H:%M:%S")
  ed <- as.POSIXct(endDate, format="%Y-%m-%dT%H:%M:%S")
  isoSDate <- str_remove_all(startDate, '-')

  server <- streams$ServerName
  auth <- adconLogin(usr=streams$Usr[1], pwd=streams$Pwd[1])

  inter = pokeDuration_Adcon(usr=streams$Usr[1], pwd=streams$Pwd[1], nodeID=streams$SensorID[1], date=isoSDate, slots = 2)
  deltaSecs <- as.numeric(ed-sd,units="secs")
  slots <- round(deltaSecs/inter)

  urls <- paste0(streams$ServerName, '/addUPI?function=getdata&session-id=', auth , '&id=', nodeID=streams$SensorID, '&date=', isoSDate, '&slots=', slots , '&mode=' , mode)

  adconServer <- streams$ServerName
  tryCatch({
    dataStreamsDF <- synchronise(async_map(urls, getURLAsync_Adcon, .limit = asyncThreadNum ))
  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })
  adconLogout(AuthID = auth)

  return(dataStreamsDF)

}


getSensorData_Outpost <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  isoSDate <- str_replace_all(startDate, '-', '/')
  isoEDate <- str_replace_all(endDate, '-', '/')

  urls <- paste0(streams$ServerName, '/api/2.0/dataservice/mydata.aspx?userName=',  streams$Usr, '&password=', streams$Pwd,
                 '&dateFrom=' , isoSDate, '&dateTo=', isoEDate, '&outpostID=', streams$SiteID, '&inputID=', streams$SensorID)

  tryCatch({
    dataStreamsDF <- synchronise(async_map(urls, getURLAsync_OutPost, .limit = asyncThreadNum))

  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })

  return(dataStreamsDF)
}


getSensorData_Outpost2 <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  isoSDate <- str_replace_all(startDate, '-', '/')
  isoEDate <- str_replace_all(endDate, '-', '/')

  urls <- paste0(streams$ServerName, '/api/2.0/dataservice/mydata.aspx?userName=',  streams$Usr, '&password=', streams$Pwd,
                 '&dateFrom=' , isoSDate, '&dateTo=', isoEDate, '&inputID=', streams$SensorID, '|', str_remove(streams$SiteID, 'opSID_'))

  tryCatch({
    dataStreamsDF <- synchronise(async_map(urls, getURLAsync_OutPost2, .limit = asyncThreadNum))

  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })

  return(dataStreamsDF)
}




getSensorData_IOT <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  SDate <- str_remove_all( str_split(startDate, 'T')[[1]][1], '-')
  bits <- str_split(streams$SiteID, '_')
  sid <- sapply(bits, function (x) x[2])[1]

  sensorIDs <- streams$SensorID

  # x <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams(20)/Observations/aggregate/day|Ross.Searle@csiro.au|uT8tGtyZSUqL'
  #https://services.cerdi.edu.au/sfs/v1.0/Datastreams(569)/Observations/aggregate/day?$top=5
  urls <- paste0('https://services.cerdi.edu.au/sfs/v1.0/Datastreams(', sensorIDs, ')/Observations/aggregate/day?fromDate=', SDate,'|Ross.Searle@csiro.au|uT8tGtyZSUqL|', startDate, '|', endDate, '|', streams$DataType)
  #urls <- paste0('https://services.cerdi.edu.au/sfs/v1.0/Datastreams(', sensorIDs, ')/Observations?fromDate=', SDate,'|Ross.Searle@csiro.au|uT8tGtyZSUqL|', startDate, '|', endDate)

  tryCatch({
    dataStreamsDF <- synchronise(async_map(urls, getURLAsync_IOT, .limit = asyncThreadNum))

  }, error = function(e)
  {
    stop(e)
   # stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })

  return(dataStreamsDF)
}




getSensorData_BoMLatest<- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  siteID <- streams$SiteID[1]
  sensorID <- streams$SensorID[1]
  bits <- str_split(siteID, '_')
  sid <- bits[[1]][3]
  wmo <- bits[[1]][2]

  rt <- streams$ProviderURL

  urls <- paste0(rt, '/fwo/', sid, '/', sid, '.', wmo,  '.json|', sensorID)
   tryCatch({
  dataStreamsDF <- synchronise(async_map(urls, getURLAsync_BoM_Latest, .limit = asyncThreadNum))

  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })

  dataStreamsDF[[1]] <- dataStreamsDF[[1]][as.POSIXct(dataStreamsDF[[1]]$theDate) >= as.POSIXct(startDate, format='%Y-%m-%dT%H:%M:%S') & as.POSIXct(dataStreamsDF[[1]]$theDate) <= as.POSIXct(endDate, format='%Y-%m-%dT%H:%M:%S'), ]
  return(dataStreamsDF)
}


getSensorData_SILO <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs ){

  siteID <- streams$SiteID[1]
  sensorID <- streams$SensorID[1]
  server <- streams$ServerName[1]
  usr <- 'CSIROESB15'
  pwd <- 'DISKW8026'
  sd <- '20160101'
  ed <- '20160110'

  #url <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=standard&station=40223&start=20160101&finish=20160110&username=CSIROESB15&password=DISKW8026'

  url <- paste0(server, '/PatchedPointDataset.php?format=standard&station=', siteID, '&start=', sd, '&finish=', ed, '&username=', usr,  '&password=', pwd, '|', sensorID)


  tryCatch({
    dataStreamsDF <- synchronise(async_map(url, getURLAsync_SILO, .limit = asyncThreadNum))

  }, error = function(e)
  {
    stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
  })



  return(dataStreamsDF)
}



getSensorData_EPARF <- function(streams, startDate = NULL, endDate = NULL, aggPeriod=timeSteps$day, numrecs=maxRecs,tempCorrect ){

  Tref=18
  f = 0.1 # 0 - 0.5

    isoSDate <- str_replace_all(startDate, '-', '/')
    isoEDate <- str_replace_all(endDate, '-', '/')

    idxs <- which(!is.na(streams$UpperDepth))
    streams <- streams[idxs,]

    #tryCatch({

        if(streams$DataType[1]=='Soil-Moisture'){

          if(!is.null(tempCorrect)){
            print('Applying temperature correction')
          urlsDEC <- paste0(streams$ServerName, '/api/2.0/dataservice/mydata.aspx?userName=',  streams$Usr, '&password=', streams$Pwd,
                         '&dateFrom=' , isoSDate, '&dateTo=', isoEDate, '&inputID=', streams$SensorID, '|', str_remove(streams$SiteID, 'opSID_'))

          soilTemp <- getSensorInfo(usr='Public', pwd='Public', siteID=streams$SiteID[1] , sensorType='Soil-Temperature', verbose=F, sensorGroup=NULL, backend=NULL, owner=NULL)
          urlsTemp <- list(nrow(streams))

          for (i in 1:nrow(streams)){
            rec <- soilTemp[soilTemp$UpperDepth == streams$UpperDepth[i], ]
            urlsTemp[i] <- paste0(rec$ServerName, '/api/2.0/dataservice/mydata.aspx?userName=',  streams$Usr[1], '&password=', streams$Pwd[1],
                               '&dateFrom=' , isoSDate, '&dateTo=', isoEDate, '&inputID=', rec$SensorID[1], '|', str_remove(streams$SiteID[1], 'opSID_'))
          }


          smRaw <- synchronise(async_map(urlsDEC, getURLAsync_EPARF, .limit = asyncThreadNum))
          temps <- synchronise(async_map(urlsTemp, getURLAsync_EPARF, .limit = asyncThreadNum))

          outDF <- list(length(smRaw))
          for(i in 1:length(smRaw)){
            if(!is.null(smRaw[[i]]) & !is.null(temps[[i]])){
              smts <- xts(smRaw[[i]][,-1], order.by=as.POSIXct( smRaw[[i]][,1]))
              tempts <- xts(temps[[i]][,-1], order.by=as.POSIXct( temps[[i]][,1]))
              ts <- merge(smts, tempts)
              if(str_to_upper(tempCorrect) == 'M1'){
              ts$TempCorrected <- ts$smts  + (Tref-ts$tempts) * f
              }
              else{
                ts$TempCorrected <- ts$smts-((ts$tempts-20)*5)
              }
              df <- data.frame(theDate=index(ts), Values=ts$TempCorrected , row.names = NULL)
              colnames(df)<-c(' theDate','Values')
              outDF[[i]]<-df
            }
          }

          return(outDF)
          }else{
            print('Not applying temperature correction')
            urls <- paste0(streams$ServerName, '/api/2.0/dataservice/mydata.aspx?userName=',  streams$Usr, '&password=', streams$Pwd,
                           '&dateFrom=' , isoSDate, '&dateTo=', isoEDate, '&inputID=', streams$SensorID, '|', str_remove(streams$SiteID, 'opSID_'))
            dataStreamsDF <- synchronise(async_map(urls, getURLAsync_EPARF, .limit = asyncThreadNum))
          }

      }else{

        urls <- paste0(streams$ServerName, '/api/2.0/dataservice/mydata.aspx?userName=',  streams$Usr, '&password=', streams$Pwd,
                       '&dateFrom=' , isoSDate, '&dateTo=', isoEDate, '&inputID=', streams$SensorID, '|', str_remove(streams$SiteID, 'opSID_'))
        dataStreamsDF <- synchronise(async_map(urls, getURLAsync_EPARF, .limit = asyncThreadNum))
      }

    # }, error = function(e)
    # {
    #   stop('No records were returned for the specified query. Most likely there is no data available in the date range specified - (async processing error)')
    # })

    return(dataStreamsDF)
  }





getSensorFields <- function(){
  return (colnames(sensorInfo))
}



getSensorLocations <- function(usr='Public', pwd='Public', siteID=NULL, sensorType=NULL, sensorGroup=NULL,backend=NULL, owner=NULL, longitude=NULL, latitude=NULL, extendedSet=F, radius_km=NULL, bbox=NULL,  numToReturn=NULL){

  sensors <- getAuthorisedSensors(usr=usr, pwd=pwd)
  #print(extendedSet)
  if(!extendedSet){
    sensors = sensors[!tolower(sensors$Backend) %in% c('bom_latest', 'silo'),]
  }


  #print(tail(sensors))

  if(!is.null(siteID)){
    sensors <- sensors[tolower(sensors$SiteID)==tolower(siteID),]
  }
  if(!is.null(sensorType)){
    sensors <- sensors[tolower(sensors$DataType)==tolower(sensorType),]
  }
  if(!is.null(sensorGroup)){
    sensors <- sensors[tolower(sensors$SensorGroup)==tolower(sensorGroup),]
  }
  if(!is.null(backend)){
    sensors <- sensors[tolower(sensors$Backend)==tolower(backend),]
  }
  if(!is.null(owner)){
    sensors <- sensors[tolower(sensors$Owner)==tolower(owner),]
  }

  s <- sensors[row.names(unique(sensors[,c("SiteName", "SiteID")])),]

  #df1 <- merge(sitesInfo, s, by='SiteID')
  df1 <- s

  outDF <- data.frame(df1$SiteID,df1$SiteName, df1$SensorGroup, df1$Backend,
                      df1$Longitude, df1$Latitude, df1$IsActive , df1$Owner, df1$Contact,
                      df1$ProviderURL,df1$NetworkInfoWebsite, df1$Description, df1$StartDate, df1$EndDate, stringsAsFactors = F)
  colnames(outDF) <- c('SiteID','SiteName','SensorGroup','Backend','Longitude','Latitude',
                       'IsActive','Owner','Contact','ProviderURL','NetworkInfoWebsite', 'Description','StartDate','EndDate')
  outDF[is.na(outDF)] <-"NA"

  if(nrow(outDF) == 0){
    stop("No sensors could be found : getSensorLocations")
  }


  ########   Spatial Filtering ############################
  if(!is.null(bbox)){
    qtype = 'bbox'
  }else if (!is.null(longitude) & !is.null(latitude)){
    qtype = 'point'
  }else{
    qtype = 'All'
  }


  if(qtype=='point'){
    coordinates(outDF) <- ~Longitude+Latitude
    crs(outDF) <- CRS("+proj=longlat +datum=WGS84")
    dist <- spDistsN1(outDF,c(as.numeric(longitude), as.numeric(latitude)), longlat = T)
    dfDist <- data.frame(outDF, distance=dist)
    outdfraw <- dfDist[order(dfDist$distance),]

    if(!is.null(radius_km)){
      outdf <- outdfraw[outdfraw$distance <= as.numeric(radius_km), ]
    }else{
      outdf <- outdfraw
    }

  }else if(qtype=='bbox'){
    bits <- str_split(bbox, ';')
    ymin <- as.numeric(bits[[1]][1])
    xmin <- as.numeric(bits[[1]][2])
    ymax <- as.numeric(bits[[1]][3])
    xmax <- as.numeric(bits[[1]][4])
    outdf <- outDF[outDF$Longitude >= xmin & outDF$Longitude <= xmax & outDF$Latitude >= ymin & outDF$Latitude <= ymax, ]

  }else{
    outdf <- outDF
  }


  n <- min(nrow(outdf), numToReturn)
  return(outdf[1:n, ])

}


getSensorInfo <-  function(usr='Public', pwd='Public', siteID=NULL, sensorType=NULL, verbose=F, sensorGroup=NULL, backend=NULL, owner=NULL) {

  sensors <- getAuthorisedSensors(usr=usr, pwd=pwd)
  # if(!is.null(siteID) & !is.null(sensorType))
  # {
  #   sensors <- sensors[tolower(sensors$SiteID) == tolower(siteID) & tolower(sensors$DataType) == tolower(sensorType), ]
  # }else if(!is.null(siteID)){
  #   sensors <- sensors[tolower(sensors$SiteID) == tolower(siteID), ]
  # }else if(!is.null(sensorType)){
  #   sensors <- sensors[tolower(sensors$DataType) == tolower(sensorType), ]
  # }

  if(!is.null(siteID) & !is.null(sensorType))
  {
    sensors <- sensors[tolower(sensors$SiteID) == tolower(siteID), ]
  }
  if(!is.null(sensorType)){
    sensors <- sensors[tolower(sensors$DataType) == tolower(sensorType), ]
  }

  if(!is.null(sensorGroup)){
    sensors <- sensors[tolower(sensors$SensorGroup)==tolower(sensorGroup),]
  }
  if(!is.null(backend)){
    sensors <- sensors[tolower(sensors$Backend)==tolower(backend),]
  }
  if(!is.null(owner)){
    sensors <- sensors[tolower(sensors$Owner)==tolower(owner),]
  }

  drops <- c("Usr","Pwd")
  outDF <-  sensors[ , !(tolower(names(sensors)) %in% tolower(drops))]
  cols <- which(str_detect(names(outDF), 'SiteID'))
  outDF <-  outDF[ , -cols[2]]

  if(verbose){
    return(outDF)
  }else{
    return(outDF[, 1:27])
  }


}


getSensorDataStreams <-  function(usr='Public', pwd='Public', siteID=NULL, sensorType=NULL, sensorID=NULL, tempCorrect=NULL, startDate=NULL, endDate=NULL, aggPeriod=timeSteps$none, outFormat='simpleTS', verbose=F ){

  # restricted to a single location for so as to not overload backend requests
  # have to restrict requests to a single data type as they have different aggregation types - could not aggregat but this may not be a common use case
  if(is.null(siteID) & is.null(sensorID))
    return(NULL)
  if(is.null(sensorType) & is.null(sensorID))
    return(NULL)



  sensors <- getAuthorisedSensors(usr=usr, pwd=pwd)

  if(!is.null(sensorID)){
    sensors <- sensors[tolower(sensors$SiteID) == tolower(siteID) & tolower(sensors$DataType) == tolower(sensorType) & tolower(sensors$SensorID) == tolower(sensorID) & !is.na(sensors$DataType), ]
    if(nrow(sensors) < 1){stop('Could not find the specified sensor')}
  }else{
    sensors <- sensors[tolower(sensors$SiteID) == tolower(siteID) & tolower(sensors$DataType) == tolower(sensorType) & !is.na(sensors$DataType), ]
    if(nrow(sensors) < 1){stop('Could not find the specified sensor')}
  }

 # print(sensorType)

  d <- getSensorData(streams=sensors, aggPeriod=aggPeriod, startDate=startDate, endDate=endDate, outFormat=outFormat, verbose=verbose, tempCorrect=tempCorrect  )

  return(d)
}


plotSensorLocationsImage <- function(DF){

  #coordinates(DF) <- ~Longitude+Latitude

  # scale.parameter = 1  # scaling paramter. less than 1 is zooming in, more than 1 zooming out.
  # xshift = -0.1  # Shift to right in map units.
  # yshift = 0.2  # Shift to left in map units.
  # original.bbox = austBdy@bbox  # Pass bbox of your Spatial* Object.
  #
  # edges = original.bbox
  # edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,]) + xshift
  # edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,]) + yshift
  #
  # rbPal <- colorRampPalette(c('red','blue'))
  #
  # Col <- rbPal( length(knownBackends))
  # levels <- knownBackends
  # rv = list("sp.polygons", austBdy, fill = "grey")
  # spp <- spplot(as.factor(DF["Backend"]), sp.layout = list(rv), key.space = "bottom", main = "Sensor Locations", xlim = edges[1, ], ylim = edges[2, ])

  #return(spp)

  pPath <- paste0(sensorRootDir, '/AncillaryData/Aust.shp')
  austBdy <- read_sf(pPath)
  meuse_sf = st_as_sf(DF, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

  bkd <-  as.numeric(unique(as.factor(meuse_sf$Backend )))
  palt <-brewer.pal(length(bkd),"Set1")

  par(mar=c(0,0,0,0))
  plot(st_geometry(austBdy), border='black', reset=FALSE, col='beige')

  plot(meuse_sf[4], pch=20, add=T, pal=palt  )
  legend("topleft", legend=levels(as.factor(meuse_sf$Backend )),fill=palt )

}


getAvailableDataTypes <- function(){

  odf <- data.frame(knownFeatures, stringsAsFactors = F )
  colnames(odf) <- c('Value')
  return(odf)
 # return(as.list(knownFeatures))
}