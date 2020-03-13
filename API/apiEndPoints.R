machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'FANCY-DP'){
  senFedDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/API'
  server <- '127.0.0.1'
  portNum <- 8072
}else if (machineName == 'TERNSOILS') {
  senFedDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederatorWebAPI'
  server = '152.83.244.137'
  portNum <- 8070
}else if (machineName == 'soils-discovery') {
  #####  need to change these
  senFedDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'
  deployDir <-'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/API'
  portNum <- 8072
}


source(paste0(senFedDir, '/Backends/Backend_Config.R'))





apiChk = ''

#* @apiTitle Soil Sensor Federation Services
#* @apiDescription These services allow <b>unified</b> and <b>standardised</b> access to a range of disparate soil sensor systems.<br><br> More detail about the Sensor Federation Service can be found <a href='http://esoil.io/FederatedServices/FedeartedSesnsorsHelpPage.html' > here </a>




#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){



  logentry <- paste0(as.character(Sys.time()), ",",
       machineName, ",",
       req$REQUEST_METHOD, req$PATH_INFO, ",",
       str_replace_all( req$HTTP_USER_AGENT, ",", ""), ",",
       req$QUERY_STRING, ",",
       req$REMOTE_ADDR
      )

  dt <- format(Sys.time(), "%d-%m-%Y")

  logDir <- paste0(deployDir, "/Logs")
  if(!dir.exists(logDir)){
     dir.create(logDir)
    }

  logfile <- paste0(deployDir, "/Logs/SoilFederationAPI_logs_", dt, ".csv")
  try(writeLogEntry(logfile, logentry), silent = TRUE)

  plumber::forward()
}

writeLogEntry <- function(logfile, logentry){

  if(file.exists(logfile)){
    cat(logentry, '\n', file=logfile, append=T)
  }else{
    hdr <- paste0('System_time,Server,Request_method,HTTP_user_agent,QUERY_STRING,REMOTE_ADDR\n')
    cat(hdr, file=logfile, append=F)
    cat(logentry, '\n', file=logfile, append=T)
  }
}



#* Returns information about the locations of sensors

#* @param pwd (Optional) Password for logging into the system - if not supplied defaults to 'Public'
#* @param usr (Optional) User name for logging into the system - if not supplied defaults to 'Public'
#* @param format (Optional) fotmat of the response to return. Either json, csv, or xml. Default = json
#* @param numToReturn (Optional) The number of closest sensor locations to be returned. Default = 10
#* @param extendedSet (Optional) Return public BoM and Silo locations. Values - T or F. Default =F
#* @param radius_km (Optional) The radius around the supplied location in which to search.  Default = NULL ie  returns all records
#* @param latitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
#* @param longitude (Optional) If a location is entered  the sensor locations closest to this location will be returned. If not specified all sensor locations will be returned
#* @param bbox (Optional) The  rectangular bounding box of the area to be queried in  the form bl;ll;tl;tr ie bottom latitude, left longitude, top latitude, right longitude

#* @param sensorgroup (Optional) Filter on a specific sensor group.
#* @param backend (Optional) Filter on a specific sensor backend type - eg the sensor suppliers, eg Outpost, Adcon etc
#* @param owner (Optional) Filter on a specific sensor owners

#* @param sensortype (Optional) Filter on a specific type of sensor. Currently supported sensor types are 'Soil-Moisture' and 'Rainfall' - if not supplied all records are returned
#* @param siteid (Optional) Filter on a specific SiteID - if not supplied all records are returned


#* @get /SensorAPI/getSensorLocations
apiGetSensorLocations <- function(res, usr='Public', pwd='Public', siteid=NULL, sensortype=NULL, sensorgroup=NULL, backend=NULL, owner=NULL, longitude=NULL, latitude=NULL, extendedSet=F,  radius_km=NULL, bbox=NULL, numToReturn=NULL, format='json'){

  tryCatch({

    check_GetSensorLocations(siteid, sensortype, longitude, latitude, radius_km, bbox, numToReturn)


        DF <- getSensorLocations(usr=usr, pwd=pwd, siteID=siteid, sensorType=sensortype, sensorGroup=sensorgroup, backend=backend, owner=owner, longitude=longitude, latitude=latitude, extendedSet=as.logical(extendedSet), radius_km=radius_km, bbox=bbox, numToReturn=numToReturn)

        if(format == 'xml'){

          res$setHeader("Content-Type", "application/xml; charset=utf-8")
          xdoc=xml_new_root('SensorLocations')
          vars_xml <- lapply(purrr::transpose(DF),
                           function(x) {
                             as_xml_document(list(SensorLocation = lapply(x, as.list)))
                           })

          for(trial in vars_xml) xml_add_child(xdoc, trial)
          res$body <- as(xdoc, "character")
          return(res)

        }else if(format == 'csv'){
          res$setHeader("content-disposition", "attachment; filename=SensorLocations.csv");
          res$setHeader("Content-Type", "application/csv; charset=utf-8")
          res$body <- writecsv(DF)
          return(res)

        }else if(format == 'html'){
          res$setHeader("Content-Type", "text/html ; charset=utf-8")
          res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "Sensor Locations")
          return(res)

        }else{
          return(DF)
        }




  }, error = function(res)
  {
    print(geterrmessage())
    res$status <- 400
    list(error=jsonlite::unbox(geterrmessage()))
  })
}





#* Returns information about the available sensors

#* @param pwd (Optional) Password for logging into the system - if not supplied defaults to 'Public'
#* @param usr (Optional) User name for logging into the system - if not supplied defaults to 'Public'
#* @param sensortype (Optional) Filter on a specific type of sensor. Currently supported sensor types are 'Soil-Moisture' and 'Rainfall' - if not supplied all records are returned
#* @param siteid (Optional) Filter on a specific SiteID - if not supplied all records are returned
#* @get /SensorAPI/getSensorInfo

apiGetSensorInfo <- function(usr='Public', pwd='Public', siteid=NULL, sensortype=NULL){

  tryCatch({
    check_GetSensorInfo(siteid, sensorid, sensortype)
    DF <- getSensorInfo(usr=usr, pwd=pwd, siteID=siteid, sensorType=sensortype)


    return(DF)
  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))
  })
}






#* Returns the data stream from the specified sensor.

#* @param pwd (Optional) Password for logging into the system - if not supplied defaults to 'Public'.
#* @param usr (Optional) User name for logging into the system - if not supplied defaults to 'Public'.
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @param sensorid (Optional) Filter on a specific sensor. If not specified all sensors data streams at the site of the type specified will be returned.
#* @param aggperiod (Optional) Agreggate the time steps of the data stream returned. Options are 'none', 'minutes', 'hours', 'days', 'weeks', 'months', 'quarters','years'. If not supplied the default is 'none'
#* @param enddate (Optional) The ending date for the data stream to be returned. Format %Y-%m-%dT%H:%M:%S eg 2018-04-20T09:00:00. If not specified data will be returned until today.
#* @param startdate (Optional) The starting date for the data stream to be returned. Format %Y-%m-%dT%H:%M:%S eg 2018-04-20T09:00:00. If not specified data will be returned from 1 year previous.
#* @param sensortype (Required) Filter on a specific type of sensor. Currently supported sensor types are 'Soil-Moisture' and 'Rainfall'.
#* @param siteid (Required) Filter on a specific SiteID.
#* @get /SensorAPI/getSensorDataStreams
apiGetSensorDataStreams <- function(res, usr='Public', pwd='Public', siteid=NULL, sensortype=NULL, sensorid=NULL, startdate=NULL, enddate=NULL, aggperiod='none', format='json'){

  tryCatch({

    # siteid <- 'OzNet_m1'
    # sensortype <- 'Soil-Moisture'
    # startDate <- ''
    # endDate <- ''

    check_GetSensorDataStreams(siteid=siteid, sensorid=sensorid, sensortype=sensortype, startDate=startdate, endDate=enddate )
       #sd <- getSensorDataStreams(usr=usr, pwd=pwd, siteID=siteid, sensorType=sensortype, sensorID=sensorid, startDate=startdate, endDate=enddate, aggPeriod=aggperiod, outFormat ='DF' )
   # return(sd)

    dataStrm <- getSensorDataStreams(usr=usr, pwd=pwd, siteID=siteid, sensorType=sensortype, sensorID=sensorid, startDate=startdate, endDate=enddate, aggPeriod=aggperiod, outFormat = 'nestedTS' )


    if(format == 'xml'){
       xdoc <-  writeNestedXML(dataStrm)
       res$setHeader("Content-Type", "application/xml; charset=utf-8")
       res$body <- as(xdoc, "character")
      return(res)

    }else if(format == 'csv'){
      outfile <- paste0(siteid,'_', sensortype=sensortype, '.csv')
      res$setHeader("content-disposition", paste0("attachment; filename=", outfile, ""));
      res$setHeader("Content-Type", "application/csv; charset=utf-8")
      res$body <- writeNestedCSV(dataStrm)
      return(res)

    }else if(format == 'html'){
      res$setHeader("Content-Type", "text/html ; charset=utf-8")
      xdoc <- writeNestedHTML(dataStrm)
      res$body <- as(xdoc, "character")
      return(res)

    }else{
      return(dataStrm)
    }





  }, error = function(res)
  {
   res$status <- 400 # Bad request
   list(error=jsonlite::unbox(geterrmessage()))

  })
}



#* Returns a static HTML help page. You will only see the HTML in the Swagger UI. Not the actual web page.
#* @get /SensorAPI/help
#* @html
help <- function(){

  #fileName="C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederationWebAPI/FedeartedSesnsorsHelpPage.html"
  fileName=paste0(deployDir, "/FederatedSensorsHelpPage.html")
  con=file(fileName,open="r")
  html=readLines(con)
  #"<html><body><h1>plumber is alive!</h1></body></html>"
  return(html)
}



#* Returns Am image of all the sensor locations

#* @param pwd (Optional) Password for logging into the system - if not supplied defaults to 'Public'
#* @param usr (Optional) User name for logging into the system - if not supplied defaults to 'Public'
#* @param sensortype (Optional) Filter on a specific type of sensor. Currently supported sensor types are 'Soil-Moisture' and 'Rainfall' - if not supplied all records are returned

#* @png (width = 400, height = 500)
#* @get /SensorAPI/getSensorLocationsAsMap
apiGetSensorLocationsAsMap <- function(usr='Public', pwd='Public', sensortype=NULL){


  tryCatch({

    DF <- avail
    spp <- plotSensorLocationsImage(DF)
    return(plot(spp))
  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))

  })


}





#* Returns a list of the available sensor data types
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @get /SensorAPI/getDataTypes
apiGetDataTypes <- function(res, format='json'){

  tryCatch({

    DF <- getAvailableDataTypes()

   print(format)
    if(format == 'xml'){

      res$setHeader("Content-Type", "application/xml; charset=utf-8")
      xdoc=xml_new_root('DataTypes')
      vars_xml <- lapply(purrr::transpose(DF),
                         function(x) {
                           as_xml_document(list(DataType = lapply(x, as.list)))
                         })

      for(trial in vars_xml) xml_add_child(xdoc, trial)
      res$body <- as(xdoc, "character")
      return(res)

    }else if(format == 'csv'){
      res$setHeader("content-disposition", "attachment; filename=DataTypes.csv");
      res$setHeader("Content-Type", "application/csv; charset=utf-8")
      res$body <- writecsv(DF)
      return(res)

    }else if(format == 'html'){
      res$setHeader("Content-Type", "text/html ; charset=utf-8")
      res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "DataTypes")
      return(res)

    }else{
      return(DF)
    }


  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))

  })
}

#* Returns info about the SoilDataFederator development
#* @param format (Optional) format of the response to return. Either json, csv, or xml. Default = json
#* @get /SensorAPI/news
apiNew <- function(res, format='json'){

  tryCatch({

    DF <-getNews()

    print(format)
    if(format == 'xml'){

      res$setHeader("Content-Type", "application/xml; charset=utf-8")
      xdoc=xml_new_root('News')
      vars_xml <- lapply(purrr::transpose(DF),
                         function(x) {
                           as_xml_document(list(Item = lapply(x, as.list)))
                         })

      for(trial in vars_xml) xml_add_child(xdoc, trial)
      res$body <- as(xdoc, "character")
      return(res)

    }else if(format == 'csv'){
      res$setHeader("content-disposition", "attachment; filename=News.csv");
      res$setHeader("Content-Type", "application/csv; charset=utf-8")
      res$body <- writecsv(DF)
      return(res)

    }else if(format == 'html'){
      res$setHeader("Content-Type", "text/html ; charset=utf-8")
      res$body <- htmlTable(DF, align = "l", align.header = "l", caption = "News")
      return(res)

    }else{
      return(DF)
    }


  }, error = function(res)
  {
    res$status <- 400 # Bad request
    list(error=jsonlite::unbox(geterrmessage()))

  })
}


#######     Some utilities    ###########################


writecsv <- function(DF){

  s <- paste(colnames(DF), collapse = ", ")
  s <- paste(s, '\r\n')

  for(i in 1:nrow(DF)){

    for(j in 1:ncol(DF)){
      if(j==1){
        s <- paste0(s, as.character(DF[i,j]))
      }else{
        s <- paste0(s, ',', as.character(DF[i,j]))
      }
    }
    s <- paste(s, '\r\n')
  }
  return(s)
}



writeNestedCSV <- function(nDF){

  s <- paste0('SiteID = ', nDF$SiteID[1], '\n')
  s <- paste0(s, 'SiteName = ', nDF$SiteName[1], '\n')
  s <- paste0(s, 'Provider = ', nDF$Provider[1], '\n')
  s <- paste0(s, 'Backend = ', nDF$Backend[1], '\n')
  s <- paste0(s, 'Access = ', nDF$Access[1], '\n')
  s <- paste0(s, 'Longitude = ', nDF$Longitude[1], '\n')
  s <- paste0(s, 'Latitude = ', nDF$Latitude[1], '\n')

  s <- paste0(s, 'RequestStartDate = ', nDF$RequestStartDate, '\n')
  s <- paste0(s, 'RequestEndDate = ', nDF$RequestEndDate, '\n')
  s <- paste0(s, 'AggregationPeriod = ', nDF$AggregationPeriod, '\n')

  s <- paste0(s, 'DataType = ', nDF$DataType[1], '\n')
  s <- paste0(s, 'Units = ', nDF$Units[1], '\n')
  s <- paste0(s, 'Calibrated = ', nDF$Calibrated[1], '\n')

   colnms <- paste(nDF$DataType[1],'_', nDF$UpperDepthCm,'_', nDF$LowerDepthCm, collapse = ', ', sep='')
   s <- paste0(s,'DateTime,', colnms,  '\n')
   numrows <- nrow(nDF$DataStream[[1]])
   numcols <- length(nDF$DataStream)
   for(i in 1:numrows){
     s <- paste0(s, format(nDF$DataStream[[1]][i,1],"%Y-%m-%dT%H:%M:%S"))
     for(j in 1:numcols){
         s <- paste0(s, ', ', as.character(nDF$DataStream[[j]][i,2]))
     }
     s <- paste(s, '\n')
   }

  return(s)
}

#writeNestedHTML(nDF)

writeNestedHTML <- function(nDF){

  flds <- c( 'SiteID', 'SiteName', 'Provider', 'Backend', 'Access', 'Longitude', 'Latitude', 'RequestStartDate', 'RequestEndDate', 'AggregationPeriod', 'DataType', 'Units', 'Calibrated')

  htmln = newXMLNode("HTML")
  bodyn <- newXMLNode("BODY", parent = htmln)

  for(i in 1:length(flds)){

    fname <-flds[i]
    n0 <- newXMLNode("p")
    n1 <- newXMLNode("b", newXMLTextNode(paste0(fname, " : ")))
    n2 <- addChildren(n0, kids = c( n1, nDF[fname][[1]][1]))
    addChildren(bodyn, kids=c(n2))

  }

  tablen <- newXMLNode("TABLE", parent = bodyn, attrs = c(border = 1))

  tr <- newXMLNode("tr")
  n1 <- newXMLNode("th", newXMLTextNode(paste0( "DateTime")),parent = tr)
  for (i in 1:nrow(nDF)) {

    n1 <- newXMLNode("th", newXMLTextNode(paste0(nDF$DataType[1],'_', nDF$UpperDepthCm[i],'_', nDF$LowerDepthCm[i])),parent = tr)

  }
  addChildren(tablen,c(tr))


  numrows <- nrow(nDF$DataStream[[1]])
  numcols <- length(nDF$DataStream)
  for(i in 1:numrows){
    dt <- format(nDF$DataStream[[1]][i,1],"%Y-%m-%dT%H:%M:%S")

    tr <- newXMLNode("tr")
    n1 <- newXMLNode("td", newXMLTextNode(paste0( dt)),parent = tr)

    for(j in 1:numcols){
      v <- as.character(nDF$DataStream[[j]][i,2])
      n1 <- newXMLNode("td", newXMLTextNode(paste0( v )),parent = tr)
    }


    addChildren(tablen,c(tr))
  }

  return(htmln)
}




writeNestedXML <- function(nDF){

  topN = newXMLNode("DataStreams")

  for (i in 1:nrow(nDF)) {

    sensor = newXMLNode("Sensor")

    newXMLNode("SensorID", newXMLTextNode(nDF$SiteID[i]), parent = sensor)
    newXMLNode("SiteName", newXMLTextNode(nDF$SiteName[i]), parent = sensor)
    newXMLNode("Provider", newXMLTextNode(nDF$Provider[i]), parent = sensor)
    newXMLNode("Backend", newXMLTextNode(nDF$Backend[i]), parent = sensor)
    newXMLNode("Access", newXMLTextNode(nDF$Access[i]), parent = sensor)
    newXMLNode("Longitude", newXMLTextNode(nDF$Longitude[i]), parent = sensor)
    newXMLNode("Latitude", newXMLTextNode(nDF$Latitude[i]), parent = sensor)

    newXMLNode("SensorID", newXMLTextNode(nDF$SensorID[i]), parent = sensor)
    newXMLNode("SensorName", newXMLTextNode(nDF$SensorName[i]), parent = sensor)

    newXMLNode("UpperDepthCm", newXMLTextNode(nDF$UpperDepthCm [i]), parent = sensor)
    newXMLNode("LowerDepthCm", newXMLTextNode(nDF$LowerDepthCm[i]), parent = sensor)

    newXMLNode("RequestStartDate", newXMLTextNode(nDF$RequestStartDate [i]), parent = sensor)
    newXMLNode("RequestEndDate", newXMLTextNode(nDF$RequestEndDate[i]), parent = sensor)
    newXMLNode("AggregationPeriod", newXMLTextNode(nDF$AggregationPeriod[i]), parent = sensor)

    newXMLNode("DataType", newXMLTextNode(nDF$DataType[i]), parent = sensor)
    newXMLNode("Units", newXMLTextNode(nDF$Units[i]), parent = sensor)
    newXMLNode("Calibrated", newXMLTextNode(nDF$Calibrated[i]), parent = sensor)

    #addChildren(sensor, kids = kidss)

    ds = newXMLNode("DataStream")
    df <- nDF$DataStream[[i]]

    for(j in 1:nrow(df)){
      vals = newXMLNode("Record")
      vkids <- c(newXMLNode("DateTime", newXMLTextNode(format( df$t[j],"%Y-%m-%dT%H:%M:%S")  )),
                 newXMLNode("Value", newXMLTextNode(df$v[j])))
     suppressWarnings(addChildren(vals, kids = vkids))
     suppressWarnings(addChildren(ds, kids = c(vals)))
    }

    addChildren(sensor, c(ds))
    addChildren(topN, kids = c(sensor))
  }
  return(topN)
}




makeDFList <- function(DF) {

  l<- vector("list", ncol(DF)-1)
  for(i in 1:(ncol(DF)-1)){
    nd <-  cbind(DF[1], DF[i+1])
    names(nd) <- c('T', 'V')
    l[[i]] <-nd
  }
  names(l) <- names(DF)[-1]
  l

}