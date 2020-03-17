
outpostOpts <- curlOptions(connecttimeout = globalTimeOut, ssl.verifypeer = FALSE)

generateSiteInfo_OutPost <- function(providerInfo, rootDir){

  urlData <- paste0(providerInfo$server, '/api/2.0/dataservice/mydata.aspx?userName=',  providerInfo$usr, '&password=', providerInfo$pwd, '&dateFrom=1/Dec/2017%2000:00:00&dateTo=1/Dec/2017%2001:00:00')

  #urlData <- 'https://www.outpostcentral.com/api/2.0/dataservice/mydata.aspx?userName=yoliver&password=export&dateFrom=1/Dec/2017%2000:00:00&dateTo=1/Dec/2017%2001:00:00'
  dataXML <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
  xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)
  #xml_view(dataXML)

  saveXML()
  # dataXML <- readLines('C:/Temp/outpost1.xml', encoding = 'UTF-8-BOM')
  # xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)

  doc <- xmlRoot(xmlObj)
  nsDefs <- xmlNamespaceDefinitions(doc)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))




  #saveXML(doc = doc, file='C:/Temp/outpost1.xml',)
  #sites <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)

  siteName <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)
  siteID <- xpathSApply(doc ,"//opdata:sites/opdata:site/id", xmlValue, ns)

  lat <- xpathSApply(doc ,"//opdata:sites/opdata:site/latitude", xmlValue, ns)
  lon <- xpathSApply(doc ,"//opdata:sites/opdata:site/longitude", xmlValue, ns)

  loggerID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", siteName,"']/parent::opdata:site/opdata:loggers/opdata:logger/id"), xmlValue, ns)

  locsDesc <- ''

  locs <- data.frame(loggerID, siteName,providerInfo$provider, providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd,  lon,lat, T, providerInfo$org, providerInfo$contact, providerInfo$orgURL, locsDesc, stringsAsFactors = F)
  colnames(locs) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Active', 'Owner', 'Contact', 'ProviderURL', 'Description')

  #outSiteDF <- locs[grepl(providerInfo$provider, locs$SiteID ),]
  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')

  write.csv(locs, outName, row.names = F, quote = F)
  cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  vc(outName)

}

generateSensorInfo_OutPost <- function(providerInfo, rootDir){

  urlData <- paste0(providerInfo$server, '/api/2.0/dataservice/mydata.aspx?userName=',  providerInfo$usr, '&password=', providerInfo$pwd, '&dateFrom=1/Dec/2017%2000:00:00&dateTo=1/Dec/2017%2001:00:00')
  dataXML <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
  xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)

  doc <- xmlRoot(xmlObj)
  nsDefs <- xmlNamespaceDefinitions(doc)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

  sites <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)

  pb <- pbCreate(length(sites), progress='text', style=3, label='Generating Sensor data.....',timer=TRUE)

  sensorDF <- getEmptySensorDF()


  for(i in 1:length(sites)){

    pbStep(pb, i)
    siteName <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)[i]
    siteID <- xpathSApply(doc ,"//opdata:sites/opdata:site/id", xmlValue, ns)[i]
    streams <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", siteName,"']/parent::opdata:site/opdata:inputs/opdata:input/name"), xmlValue, ns)
    streamID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", siteName,"']/parent::opdata:site/opdata:inputs/opdata:input/id"), xmlValue, ns)

    lat <- xpathSApply(doc ,"//opdata:sites/opdata:site/latitude", xmlValue, ns)[i]
    lon <- xpathSApply(doc ,"//opdata:sites/opdata:site/longitude", xmlValue, ns)[i]

    loggerID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", siteName,"']/parent::opdata:site/opdata:loggers/opdata:logger/id"), xmlValue, ns)


    df <- data.frame( loggerID[1], siteName[1], providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, providerInfo$server, lat, lon, streamID, streams, NA, NA, NA, NA, NA, F, '', stringsAsFactors = F)
    colnames(df) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'SeverName', 'Latitude', 'Longitude', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units')
    sensorDF <- rbind(sensorDF, df)
  }

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv')
  write.csv(sensorDF, outName, row.names = F, quote = F)
  pbClose(pb)
  cat(paste0('Sensor info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  cat('\n')
  cat('OK. Now manually curate this file to expose the data you want\n')
  cat("Don't forget to recompile the 'AllSensors.csv' & 'AllSites.csv' files afetr these changes\n")
  vc(outName)
}

getData_Outpost<- function(usr=usr, pwd=pwd, opID=opID, sensorID=sensorID, dateFrom=dateFrom,  dateTo=dateTo){

  # usr<- providerInfo$usr
  # pwd <- providerInfo$pwd
  #
  # opID <- 'op16299'
  # sensorID <- '386340'
  # dateFrom <- '1/Dec/2017%2000:00:00'
  # dateTo <- '1/Dec/2017%2001:00:00'

  url <- paste0(providerInfo$server, '/api/2.0/dataservice/mydata.aspx?userName=', usr, '&password=', pwd,
                    '&outpostID=', opID, '&inputID=', sensorID, '&dateFrom=', dateFrom, '&dateTo=', dateTo)

  dataXML <- getURL(url, .opts = outpostOpts)

}

getURLAsync_OutPost <- function(x){

  #response <- getURL(x)
  response <- getURL(x, .opts = myOpts , .encoding = 'UTF-8-BOM')

  ndf<- outpost_GenerateTimeSeries(response, retType = 'df')
  return(ndf)
}

outpost_GenerateTimeSeries <- function(response, retType = 'df'){

  # this is a massive hack to deal with a problem in the xml response on linux systems
  # on linux the xml has some characters att the start of the response which then stop it from
  # being recognised as propper xml by the R xml parsing functions
  # the code below chops off these characters in linux system
  # if I understood more about character sets etc in xml I could probably doa proper fix
  # sysinf <- Sys.info()
  # if(as.character(sysinf['sysname']) == "Linux"){
  #   response2 <- str_sub(response, 4, nchar(response))
  #   print("here")
  # }else{
  #   response2 <- response
  # }

  xmlObj=xmlParse(response, useInternalNodes = TRUE)

  doc <- xmlRoot(xmlObj)

  nsDefs <- xmlNamespaceDefinitions(doc)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

  rawDates <- xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/date", xmlValue, ns)

  if(length(rawDates) < 1 ){
    stop('No records were returned for the specified query')
  }

  dl <- str_replace(rawDates, 'T', ' ')
  vals <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value", xmlValue, ns))

  if(retType == 'xts'){
    tz <- xts(as.numeric(vals), order.by = dl)
    return (tz)
  }else if(retType == 'df'){
    ndf <- data.frame(dl, vals)
    colnames(ndf)<- c('theDate', 'Values')

    return(ndf)

  }else{
    stop(cat(retType, 'is an unkown data return type. Options are', paste(knownAdconReturnTypes, collapse=',' )), call. = F)
  }
}

