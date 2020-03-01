library(RCurl)
library(XML)
library(xml2)


knownAdconReturnTypes <- c('df', 'xts')

timeOut <- 60 # seconds
mode <- 't' # or 'z'
slots <- 100

myOpts <- curlOptions(connecttimeout = 200, ssl.verifypeer = FALSE)

adconSensorTypes <- c('Soil moisture', 'Daily Rainfall' )
adconServerWA <- 'http://data.farmlinkrural.com'
#adconSensorTypes <- c('Soil moisture' )

# Login
adconLogin <- function(adconServer, usr = usr, pwd = pwd, timeOut=30, mode='t'){

    urlLogin <- paste0(adconServer, '/addUPI?function=login&user=', usr ,'&passwd=', pwd, '&timeout=', timeOut, '&mode=' , mode)

   # print(urlLogin)
    authXML <- getURL(urlLogin, .opts = myOpts)
    AuthID <- xml_text(xml_find_first(read_xml(authXML), '//response/result'))
    print(urlLogin)
    #write(authXML, 'C:/Users/sea084/Dropbox/RossRCode/Git/ProbeAPIs/AdconResponses/auth.xml')

    return(AuthID)
}

# Logout
adconLogout <- function(adconServer, AuthID = AuthID){

    urlLogout <- paste0(adconServer, '/addUPI?function=logout&session-id=', AuthID , '&mode=' , mode)
    logoutXML <- getURL(urlLogout, .opts = myOpts)
    print(urlLogout)
    return(logoutXML)
}

# get Config
adconConfig <- function(adconServer = NULL, usr=usr, pwd = pwd){

    authID <- adconLogin(adconServer=adconServer, usr=usr, pwd = pwd)
    urlConfig <- paste0(adconServer, '/addUPI?function=getconfig&session-id=', authID , '&mode=' , mode)
    print(urlConfig)
    configXML <- getURL(urlConfig, .opts = myOpts)
    adconLogout(adconServer=adconServer, AuthID = authID)

    return(configXML)
}

# get Template
adconTemplate<- function(usr=usr, pwd=pwd){

    authID <- adconLogin(usr=usr, pwd = pwd)
    urlTemplate <- paste0(adconServer, '/addUPI?function=gettemplate&session-id=', authID , '&mode=' , mode)
    templateXML <- getURL(urlTemplate, .opts = myOpts)
    adconLogout(AuthID = authID)

    return(templateXML)
}

# get Attribute
adconAttribute <- function(usr=usr, pwd=pwd, nodeID=nodeID){

    authID <- adconLogin(usr=usr, pwd = pwd)
    urlAttribute <- paste0(adconServer, '/addUPI?function=getattrib&session-id=', authID , '&id=', nodeID, '&mode=' , mode)
    attributeXML <- getURL(urlAttribute, .opts = myOpts)
    adconLogout(AuthID = authID)

    return(attributeXML)
}

# get Data With no existing Auth XML
adconGetData <- function(adconServer=server, usr=usr, pwd=pwd, nodeID, date, slots = 100){

    authID <- adconLogin(adconServer, usr=usr, pwd = pwd)

    urlData <- paste0(adconServer, '/addUPI?function=getdata&session-id=', authID , '&id=', nodeID, '&date=', date, '&slots=', slots , '&mode=' , mode)
   # print(urlData)
    dataXML <- getURL(urlData, .opts = myOpts)
    adconLogout(adconServer, AuthID = authID)

    return(dataXML)
}

# get Data With existing Auth XML
adconGetDataWithAuth <- function(usr=usr, pwd=pwd, authID=authID, nodeID, date, slots = 100000){

  urlData <- paste0(adconServer, '/addUPI?function=getdata&session-id=', authID , '&id=', nodeID, '&date=', date, '&slots=', slots , '&mode=' , mode)
  dataXML <- getURL(urlData, .opts = myOpts)

  return(dataXML)
}

# get Data using a date range
adconGetDataDateRange <- function(usr=usr, pwd = pwd, nodeID = NULL, startDate, endDate, deltaSecs=900){

  inter = 900
  sd <- as.POSIXct(startDate, format="%Y%m%dT%H:%M:%S")
  ed <- as.POSIXct(endDate, format="%Y%m%dT%H:%M:%S")

  deltaSecs <- as.numeric(ed-sd,units="secs")
  slots <- round(deltaSecs/inter) -1

  dataXML = adconGetData(usr,pwd, nodeID, startDate, slots)

  return(dataXML)
}


adconGetDataDateRangeWithAuth <- function(usr=usr, pwd = pwd, authID=authID, nodeID = NULL, startDate, endDate, deltaSecs=900){

  inter = 900
  sd <- as.POSIXct(startDate, format="%Y%m%dT%H:%M:%S")
  ed <- as.POSIXct(endDate, format="%Y%m%dT%H:%M:%S")

  deltaSecs <- as.numeric(ed-sd,units="secs")
  slots <- round(deltaSecs/inter) - 1

  dataXML = adconGetDataWithAuth(usr=usr, pwd=pwd, authID=authID, nodeID, date, slots = slots)

  return(dataXML)
}

# error Checking of response
noErrors <- function(xml){

  xmlErrTest=xmlParse(xml)
  xe <- xmlRoot(xmlErrTest)
  isErr <- xpathSApply (xe ,"//response/error", xmlGetAttr, 'code')
  length(isErr)

    if(length(isErr) > 0){
        return(F)
    }else{
       return(T)
    }
  }


adconGenerateTimeSeries<- function(xmlData, retType='df'){

  xmlObj=xmlParse(xmlData)
  r <- xmlRoot(xmlObj)
  vals  <- as.numeric(xpathSApply (r ,"//response/node/v",xmlValue))
  times  <- xpathSApply (r ,"//response/node/v", xmlGetAttr, 't')
  quals  <- as.numeric(unlist(xpathSApply (r ,"//response/node/v", xmlGetAttr, 's')))

  dl <- as.POSIXct(numeric(length(times)), origin='1970-01-01')

  dl[1] <- as.POSIXct(times[1], format="%Y%m%dT%H:%M:%S")
  for(i in 2:length(times)){
    sign <- str_sub(times[i], start = 1, end = 1)
    interv <- as.numeric(str_replace(times[i], paste0('[', sign, ']'), ""))

    dl[i] <- dl[i-1] + interv
  }


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





adcon_GetSiteMetadata <- function(usr=usr, pwd = pwd){

  xmlConfig <- adconConfig(usr=usr, pwd = pwd)
  #xml_view(xmlConfig)
  xmlObj=xmlParse(xmlConfig)
  r <- xmlRoot(xmlObj)

  DataSetNodeID  <- xpathSApply (r ,"//response/node", xmlGetAttr, 'id')
  DataSetName  <- xpathSApply (r ,"//response/node", xmlGetAttr, 'name')

  SiteNames  <- xpathSApply (r ,"//response/node/nodes/node", xmlGetAttr, 'name')
  SiteIDs  <- xpathSApply (r ,"//response/node/nodes/node", xmlGetAttr, 'id')

  outDF <- data.frame(DataSet=character(), DataSetGroupIDs=numeric(), SiteNames=character(), SiteIDs=numeric(),
                      altitude = numeric(), active = logical(), batteryVoltage = numeric(), date = numeric(), firstSlot = numeric(),
                      lastSlot = numeric(), latitude = numeric(), longitude = numeric(), manufacturer = character(),
                      timeZone = character() )

  pb <- txtProgressBar(min = 0, max = length(SiteNames), style = 3)
  for(j in 1:length(SiteNames)){

    site<- SiteNames[j]
    siteID <- SiteIDs[j]

    DeviceID  <- xpathSApply (r ,paste0("//response/node/nodes/node[@id='", siteID ,"']/nodes/node[@class='DEVICE']"), xmlGetAttr, 'id')

    atts <- adconAttribute(usr=usr, pwd = pwd, nodeID = DeviceID)


    if(noErrors(atts)){

      xmlObjAtts=xmlParse(atts)
      nodeAtts <- xmlRoot(xmlObjAtts)

      altitude  <- xpathSApply (nodeAtts ,"//response/attrib[@name='altitude']/double", xmlValue)[1]
      active  <- xpathSApply (nodeAtts ,"//response/attrib[@name='active']/boolean", xmlValue)
      batteryVoltage  <- xpathSApply (nodeAtts ,"//response/attrib[@name='batteryVoltage']/double", xmlValue)
      date  <- xpathSApply (nodeAtts ,"//response/attrib[@name='date']/date", xmlValue)
      firstSlot  <- xpathSApply (nodeAtts ,"//response/attrib[@name='firstSlot']/date", xmlValue)
      lastSlot  <- xpathSApply (nodeAtts ,"//response/attrib[@name='lastSlot']/date", xmlValue)
      latitude  <- xpathSApply (nodeAtts ,"//response/attrib[@name='latitude']/double", xmlValue)
      longitude  <- xpathSApply (nodeAtts ,"//response/attrib[@name='longitude']/double", xmlValue)
      manufacturer  <- xpathSApply (nodeAtts ,"//response/attrib[@name='manufacturer']/string", xmlValue)
      slotInterval  <- xpathSApply (nodeAtts ,"//response/attrib[@name='slotInterval']/int", xmlValue)
      timeZone  <- xpathSApply (nodeAtts ,"//response/attrib[@name='timeZone']/string", xmlValue)
      type  <- xpathSApply (nodeAtts ,"//response/attrib[@name='type']/string", xmlValue)

      recDF <- data.frame( DataSetName, DataSetNodeID, site, siteID, altitude, active, date, firstSlot, lastSlot, latitude, longitude, manufacturer, slotInterval, timeZone)
      outDF<- rbind(outDF, recDF)

    }




    setTxtProgressBar(pb, j)

  }
  close(pb)

  return (outDF)

}




adcon_GetSensorMetadata <- function(usr=usr, pwd = pwd){

 xmlConfig <- adconConfig(usr=usr, pwd = pwd)
 #xml_view(xmlConfig)
 xmlObj=xmlParse(xmlConfig)
 r <- xmlRoot(xmlObj)

 DataSetNodeID  <- xpathSApply (r ,"//response/node", xmlGetAttr, 'id')
 DataSetName  <- xpathSApply (r ,"//response/node", xmlGetAttr, 'name')

 SiteNames  <- xpathSApply (r ,"//response/node/nodes/node", xmlGetAttr, 'name')
 SiteIDs  <- xpathSApply (r ,"//response/node/nodes/node", xmlGetAttr, 'id')

 # DeviceNames  <- xpathSApply (r ,"//response/node/nodes/node/nodes/node", xmlGetAttr, 'name')
 # DeviceIDs  <- xpathSApply (r ,"//response/node/nodes/node/nodes/node", xmlGetAttr, 'id')

 outDF <- data.frame(DataSet=character(), DataSetGroupIDs=numeric(), SiteNames=character(), SiteIDs=numeric(), SensorNames=character(), sensorIDs = numeric(),
                     altitude = numeric(), active = logical(), batteryVoltage = numeric(), date = numeric(), firstSlot = numeric(),
                     lastSlot = numeric(), latitude = numeric(), longitude = numeric(), Depth = numeric(), manufacturer = character(), slotInterval = numeric(),
                     timeZone = character(), type = character() )

pb <- txtProgressBar(min = 0, max = length(SiteNames), style = 3)
 for(j in 1:length(SiteNames)){

   site<- SiteNames[j]
   siteID <- SiteIDs[j]

   #print(siteID)

   DeviceID  <- xpathSApply (r ,paste0("//response/node/nodes/node[@id='", siteID ,"']/nodes/node[@class='DEVICE']"), xmlGetAttr, 'id')

   atts <- adconAttribute(usr=usr, pwd = pwd, nodeID = DeviceID)

   if(noErrors(atts)){

    # xml_view(atts)
     xmlObjAtts=xmlParse(atts)
     nodeAtts <- xmlRoot(xmlObjAtts)

     altitude  <- xpathSApply (nodeAtts ,"//response/attrib[@name='altitude']/double", xmlValue)
     active  <- xpathSApply (nodeAtts ,"//response/attrib[@name='active']/boolean", xmlValue)
     batteryVoltage  <- xpathSApply (nodeAtts ,"//response/attrib[@name='batteryVoltage']/double", xmlValue)
     date  <- xpathSApply (nodeAtts ,"//response/attrib[@name='date']/date", xmlValue)
     firstSlot  <- xpathSApply (nodeAtts ,"//response/attrib[@name='firstSlot']/date", xmlValue)
     lastSlot  <- xpathSApply (nodeAtts ,"//response/attrib[@name='lastSlot']/date", xmlValue)
     latitude  <- xpathSApply (nodeAtts ,"//response/attrib[@name='latitude']/double", xmlValue)
     longitude  <- xpathSApply (nodeAtts ,"//response/attrib[@name='longitude']/double", xmlValue)
     manufacturer  <- xpathSApply (nodeAtts ,"//response/attrib[@name='manufacturer']/string", xmlValue)
     slotInterval  <- xpathSApply (nodeAtts ,"//response/attrib[@name='slotInterval']/int", xmlValue)
     timeZone  <- xpathSApply (nodeAtts ,"//response/attrib[@name='timeZone']/string", xmlValue)
     type  <- xpathSApply (nodeAtts ,"//response/attrib[@name='type']/string", xmlValue)


   }

     #  for(i in 1:length(DeviceNames)) {

   for(i in 1:length(adconSensorTypes)) {

     sensorType <- adconSensorTypes[i]

       #SiteNodesXML <-  xpathSApply (r ,paste0("//response/node/nodes/node[@id='", siteID ,"']"))
       sensorIDs <- xpathSApply (r ,paste0("//response/node/nodes/node[@id='", siteID ,"']/nodes/node[@name='", sensorType ,"']/nodes/node"), xmlGetAttr, 'id')
       sensorNames <- xpathSApply (r ,paste0("//response/node/nodes/node[@id='", siteID ,"']/nodes/node[@name='", sensorType ,"']/nodes/node"), xmlGetAttr, 'name')

       if(length(sensorNames) > 0){
           depths <- str_split(sensorNames, ' ')

           dn <- numeric(length(depths))
             for(xi in 1:length(depths))
             {
                d1 <- depths[xi][[1]]
                if(length(d1) == 4){
                  dn[xi] <-  as.numeric(str_replace(d1[4], 'cm', ''))
                }
             }

           recDF <- data.frame( DataSetName, DataSetNodeID, site, siteID, sensorNames, sensorIDs, altitude, active, date, firstSlot, lastSlot, latitude, longitude, dn, manufacturer, slotInterval, timeZone, type)
           outDF<- rbind(outDF, recDF)
       }



 }
   setTxtProgressBar(pb, j)

}
close(pb)

return (outDF)

}


getURLAsync_Adcon <- function(x){

  response <- getURL(x)
  #stop(response)
  ndf<- adconGenerateTimeSeries(response, retType = 'df')
  return(ndf)
}


generateSiteInfo_Adcon <- function(providerInfo, rootDir, getRaw){

  if(getRaw){
      md <- adcon_GetSiteMetadata(usr=usr, pwd = pwd)
      outNameRaw <- paste0(rootDir, '/SensorInfo/Adcon_', providerInfo$provider, '_Sites_Raw.csv')
      write.csv(md, outNameRaw, row.names = F, quote = F)
  }

  vc(outNameRaw)
  md <- read.csv(outNameRaw, stringsAsFactors = F)

  locs <- data.frame(md$siteID, md$site,providerInfo$provider, providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd,  md$longitude ,md$latitude, T, providerInfo$org, providerInfo$contact, providerInfo$orgURL, '', stringsAsFactors = F)
  colnames(locs) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Active', 'Owner', 'Contact', 'ProviderURL', 'Description')

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')
  write.csv(locs, outName, row.names = F, quote = F)
  cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  vc(outName)
}

generateSensorInfo_Adcon <- function(providerInfo, rootDir, getRaw){


  if(getRaw){
    md <- adcon_GetSensorMetadata(usr=usr, pwd = pwd)
    outNameRaw <- paste0(rootDir, '/SensorInfo/Adcon_', providerInfo$provider, '_Sensors_Raw.csv')
    write.csv(md, outNameRaw, row.names = F, quote = F)
  }


  md <- read.csv(outNameRaw, stringsAsFactors = F)

  df <- data.frame( md$siteID, md$siteID, providerInfo$provider,providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, providerInfo$server, md$latitude, md$longitude, md$sensorIDs, md$sensorNames, md$firstSlot, md$lastSlot, md$dn, md$dn, 'Soil-Moisture', F, 'Percent', stringsAsFactors = F)
  colnames(df) <- c('SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Usr', 'Pwd', 'SeverName', 'Latitude', 'Longitude', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units')



  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv')
  write.csv(df, outName, row.names = F, quote = F)
  pbClose(pb)
  cat(paste0('Sensor info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  cat('\n')
  cat('OK. Now manually curate this file to expose the data you want\n')
  cat("Don't forget to recompile the 'AllSensors.csv' & 'AllSites.csv' files after these changes\n")
  vc(outName)
}


pokeDuration_Adcon <- function(usr, pwd, nodeID, date, slots = 2){

  xmlData <- adconGetData(usr=usr, pwd = pwd, nodeID = nodeID, date = date, slots = slots)
  xmlObj=xmlParse(xmlData)
  r <- xmlRoot(xmlObj)

  err <- xpathSApply (r ,"//response/node/error",xmlAttrs)
 if( length(err) > 0){
   stop('No records were returned for the specified query')
 }

  vals  <- as.numeric(xpathSApply (r ,"//response/node/v",xmlValue))

  if(length(vals) < 2){
    stop('No records were returned for the specified query')
  }

  dstr  <- xpathSApply (r ,"//response/node/v", xmlGetAttr, 't')[[slots]]
  interv <- as.numeric(str_remove(dstr, '/+'))
  return(interv)
}









