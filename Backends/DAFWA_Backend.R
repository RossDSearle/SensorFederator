
DAFWAapiKey <- 'CCB3F85A64008C6AC1789E4F.apikey'

myOpts <- curlOptions(connecttimeout = 200, ssl.verifypeer = FALSE)


generateSiteInfo_DAFWA <- function(providerInfo, rootDir){

  urlSts <- paste0('https://api.agric.wa.gov.au/v1/weatherstations.json?api_key=', DAFWAapiKey)
  stns <- getURL(urlSts, .opts = myOpts)
  stnsJ <- fromJSON(stns)

  locs <- data.frame(paste0('DAFWA_', stnsJ$result$station_id ), stnsJ$result$name,providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, stnsJ$result$latitude , stnsJ$result$longitude, T, providerInfo$org, providerInfo$email, providerInfo$server, 'Part of the DAFWA weather staion network' , stringsAsFactors = F)
  colnames(locs) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Active', 'Owner', 'Contact', 'ProviderURL', 'Description')
  locs <- na.omit(locs)

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')
  write.csv(locs, outName, row.names = F, quote = F)
  cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  vc(outName)

}



generateSensorInfo_DAFWA <- function( providerInfo, rootDir){

  urlSts <- paste0('https://api.agric.wa.gov.au/v1/weatherstations.json?api_key=', DAFWAapiKey)
  stns <- getURL(urlSts, .opts = myOpts)
  stnsJ <- fromJSON(stns)

  sensorDF <- getEmptySensorDF()

    dfRain <- data.frame( paste0('DAFWA_', stnsJ$result$station_id ), stnsJ$result$name,providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, providerInfo$server, stnsJ$result$latitude , stnsJ$result$longitude, paste0(providerInfo$backEnd, '_', stnsJ$result$station_id, '_Rainfall'), paste0('DAFWA_', stnsJ$result$station_id, '_Rainfall'), stnsJ$result$start_date, NA, 'Rainfall', NA, NA, T, 'mm', stringsAsFactors = F)
    colnames(dfRain) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'SeverName', 'Latitude', 'Longitude', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units')
    sensorDF <- rbind(sensorDF, dfRain)

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv')
  write.csv(sensorDF, outName, row.names = F, quote = F)
  cat(paste0('Sensor info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  cat('\n')
  cat('OK. Now manually curate this file to expose the data you want\n')
  cat("Don't forget to recompile the 'AllSensors.csv' & 'AllSites.csv' files afetr these changes\n")
  vc(outName)
}




getURLAsync_DAFWA <- function(x){



  response <- getURL(x)
  ndf<- DAFWA_GenerateTimeSeries(response, retType = 'df', variables=c('rain'))
  return(ndf)
}


DAFWA_GenerateTimeSeries <- function(response, retType = 'df', variables){

  tsj <- fromJSON(response, flatten=T)
  if(length(tsj$result) == 0){
    (stop('No records were returned for the specified query'))
  }

  dts <- paste0(tsj$result$record_date, ' 00:00:00')
  vals <- as.numeric(tsj$result$rain)

  if(retType == 'xts'){
    tz <- xts(as.numeric(vals), order.by = dts)
    return (tz)
  }else if(retType == 'df'){
    ndf <- data.frame(dts, vals)
    colnames(ndf)<- c('theDate', 'Values')
    return(ndf)

  }else{
    stop(cat(retType, 'is an unkown data return type. Options are', paste(knownAdconReturnTypes, collapse=',' )), call. = F)
  }
}
