



generateSiteInfo_Cosmoz <- function(providerInfo, rootDir){

  stnsRaw <- getURL(paste0('http://cosmoz.csiro.au/rest/stations?count=100&offset=0'))
  stns <- gsub("\r?\n|\r", " ", stnsRaw)
  stnsJ <- fromJSON(stns)

  locs <- data.frame(paste0('Cosmoz_', stnsJ$stations$site_no), stnsJ$stations$site_name,providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, stnsJ$stations$latitude, stnsJ$stations$longitude, T, providerInfo$org, providerInfo$email, providerInfo$server, str_replace_all(stnsJ$stations$site_description, ',', ' ') , stringsAsFactors = F)
  colnames(locs) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Active', 'Owner', 'Contact', 'ProviderURL', 'Description')
  locs <- na.omit(locs)

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')
  write.csv(locs, outName, row.names = F, quote = F)
  cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  vc(outName)

}



generateSensorInfo_Cosmoz <- function( providerInfo, rootDir){

  stnsRaw <- getURL(paste0('http://cosmoz.csiro.au/rest/stations?count=100&offset=0'))
  stns <- gsub("\r?\n|\r", " ", stnsRaw)
  stnsJ <- fromJSON(stns)

  sensorDF <- getEmptySensorDF()

 # for(i in 1:nrow(stnsJ$stations)){

    dfRain <- data.frame( paste0('Cosmoz_', stnsJ$stations$site_no), stnsJ$stations$site_name, providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, providerInfo$server, stnsJ$stations$latitude, stnsJ$stations$longitude, paste0(providerInfo$backEnd, stnsJ$stations$site_no, '_Rainfall'), paste0('Cosmoz_', stnsJ$stations$site_no, '_Rainfall'), stnsJ$stations$installation_date, NA, 'Rainfall', NA, NA, T, 'mm', stringsAsFactors = F)
    colnames(dfRain) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'SeverName', 'Latitude', 'Longitude', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units')
    sensorDF <- rbind(sensorDF, dfRain)
    dfSM <- data.frame( paste0('Cosmoz_', stnsJ$stations$site_no), stnsJ$stations$site_name, providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, providerInfo$server, stnsJ$stations$latitude, stnsJ$stations$longitude, paste0(providerInfo$backEnd, stnsJ$stations$site_no, '_SoilMoisture'), paste0('Cosmoz_', stnsJ$stations$site_no, '_SoilMoisture'), stnsJ$stations$installation_date, NA, 'Soil-Moisture', 0, 300, T, 'percent', stringsAsFactors = F)
    colnames(dfSM) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'SeverName', 'Latitude', 'Longitude', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units')
    sensorDF <- rbind(sensorDF, dfSM)
  #}

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv')
  write.csv(sensorDF, outName, row.names = F, quote = F)
  cat(paste0('Sensor info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  cat('\n')
  cat('OK. Now manually curate this file to expose the data you want\n')
  cat("Don't forget to recompile the 'AllSensors.csv' & 'AllSites.csv' files afetr these changes\n")
  vc(outName)
}




getURLAsync_Cosmoz <- function(x){

  #response <- getURL(x)
  resp <- GET(x)
  response <- content(resp, "text")
  print(response)
  ndf<- cosmoz_GenerateTimeSeries(response, retType = 'df')
  return(ndf)
}


cosmoz_GenerateTimeSeries <- function(response, retType = 'df'){


  tsj <- fromJSON(response, flatten=T)
  print(str(tsj))
  if(tsj$meta$count == 0){
      (stop('No records were returned for the specified query'))
    }
  dts <- str_replace(tsj$observations[,1], 'T', ' ')
  dts2 <- str_replace(dts, 'Z', '')
  vals <- tsj$observations[,2]

  if(retType == 'xts'){
    tz <- xts(as.numeric(vals), order.by = dts2)
    return (tz)
  }else if(retType == 'df'){
    ndf <- data.frame(dts2, vals)
    colnames(ndf)<- c('theDate', 'Values')
    return(ndf)

  }else{
    stop(cat(retType, 'is an unkown data return type. Options are', paste(knownAdconReturnTypes, collapse=',' )), call. = F)
  }
}


