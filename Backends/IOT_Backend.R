library(RCurl)
library(jsonlite)
library(httr)
library(stringr)


# url <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams(20)/Observations/aggregate/day'
 usr <- 'Ross.Searle@csiro.au'
 pwd <- 'uT8tGtyZSUqL'

# rootDir = 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'

#
# resp <- GET(url,  authenticate(usr, pwd))
# response <- content(resp, "text")
# response
# fromJSON(response)
#
#
# x <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams(20)/Observations/aggregate/day|Ross.Searle@csiro.au|uT8tGtyZSUqL'
# x <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams(20)/Observations|Ross.Searle@csiro.au|uT8tGtyZSUqL'


# thingsURL <- 'https://services.cerdi.edu.au/sfs/v1.0/Things'
# thingsresp <- GET(thingsURL,  authenticate(usr, pwd))
# thingsresponse <- content(thingsresp, "text")
# thingsDf <- fromJSON(thingsresponse)
#
#
# for (i in 1:nrow(thingsDf)) {
#
#
#
# }



generateSiteInfo_IOT_SFS <- function(providerInfo, rootDir){

  url <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams?$filter=ObservedProperty/id%20eq%201&$expand=Sensor'
  url <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams?$top=5000'
  resp <- GET(url,  authenticate(usr, pwd))
  response <- content(resp, "text")
  resDf <- fromJSON(response)


n <- nrow(resDf$value)
siteIDl <- character(n)
sensIDl <- character(n)
latl  <- character(n)
lonl  <- character(n)
siteNamel <- character(n)
descl <- character(n)
datatypel <- character(n)
endl <- character(n)
startcl <- character(n)
Depthl <- character(n)


for (i in 700:n) {
  #for (i in 1:30) {
  Sys.sleep(3)

  print(i)

  rec <- resDf$value[i,]

  thingUrl <- rec$`Thing@iot.navigationLink`
  thingResp <- GET(thingUrl,  authenticate(usr, pwd))
  thingResponse <- content(thingResp, "text")
  thingDf <- fromJSON(thingResponse)

  opUrl <- rec$`ObservedProperty@iot.navigationLink`
  opResp <- GET(opUrl,  authenticate(usr, pwd))
  opResponse <- content(opResp, "text")
  opDf <- fromJSON(opResponse)


  sensorUrl <- rec$`Sensor@iot.navigationLink`
  sensorResp <- GET(sensorUrl,  authenticate(usr, pwd))
  sensorResponse <- content(sensorResp, "text")
  sensorDf <- fromJSON(sensorResponse)

  if(str_detect(sensorDf$name, 'SoilMoisture')){
    d <- str_remove(sensorDf$name, 'SoilMoisture')
    Depthl[i] <- d
  }else if(str_detect(sensorDf$name, 'SoilTemperature')){
    d <- str_remove(sensorDf$name, 'SoilTemperature')
    Depthl[i] <- d
  }else{
    Depthl[i] <- 0
  }

  #####  This way I was fetching the coords in the IOT_Backend.r generateSiteInfo_IOT_SFS function is wrong.
  #####  Eace fix is implemented in IOT_SFS_Admin.R file before committing to the database - should be in here next time

  locationsUrl <- thingDf$`Locations@iot.navigationLink`
  locationsResp <- GET(locationsUrl,  authenticate(usr, pwd))
  locationsResponse <- content(locationsResp, "text")
  locationsDf <- fromJSON(locationsResponse)

  locs <- locationsDf$value$location$geometry$coordinates
  lonl[i] <- locs[[1]][1]
  latl[i] <- locs[[1]][2]

  siteIDl[i] <- locationsDf$value$'@iot.id'
  siteNamel[i] <- thingDf$name
  descl[i] <-  thingDf$description

  sensIDl[i] <- rec$'@iot.id'
  datatypel[i] <- opDf$name

  sf <- rec$phenomenonTime
  bits <- str_split(sf, '/')
  startl[i] <- bits[[1]][1]
  endl[i] <- bits[[1]][2]
}


  AllInfo <- data.frame(siteIDl, sensIDl, siteNamel, datatypel, startl, endl, Depthl, Depthl, providerInfo$provider, providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, latl, lonl,  providerInfo$org, providerInfo$contact, providerInfo$orgURL, providerInfo$Description, providerInfo$server, stringsAsFactors = F)
  colnames(AllInfo) <- c('SiteID', 'SensorID', 'SiteName', 'DataType', 'StarDate', 'EndDate', 'UpperDepth', 'LowerDepth',  'SensorGroup', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Owner', 'Contact', 'ProviderURL', 'Description', 'ServerName')
  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_AllInfo.csv')

  write.csv(AllInfo, outName, row.names = F, quote = F)
  cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  vc(outName)

  return(locs)

}




getURLAsync_IOT <- function(x){

  bits <- str_split(x, '[|]')
  url <- bits[[1]][1]
  usr <- bits[[1]][2]
  pwd <- bits[[1]][3]
  sd <- bits[[1]][4]
  ed <- bits[[1]][5]
  dt <- bits[[1]][6]

  resp <- GET(url,  authenticate(usr, pwd), add_headers(content_type="text/json;charset=UTF-8"))
  response <- suppressMessages(content(resp, "text"))


  if(response=='' | response=='[]'){
    outList <-   vector("list")
    return(outList)
  }else{
    ndf <- IOT_GenerateTimeSeries(response, retType = 'df', sd, ed, dt)
    return(ndf)
  }
}



IOT_GenerateTimeSeries <- function(response, retType = 'df', startDate=NULL, endDate=NULL, dataType){

  ddf <- fromJSON(response)
  print(head(ddf))

  if(nrow(ddf) == 0){
    #outList <-   vector("list")
    return(data.frame())
  }

  dts <-  as.POSIXct(ddf$date, format = "%Y-%m-%dT%H:%M:%OS"  )
  outList <-   vector("list", 1 )

  if( dataType %in% c('Rainfall')){
    ndf <- data.frame(dt=dts, vals=as.numeric(ddf$sum))
  }else{
    ndf <- data.frame(dt=dts, vals=as.numeric(ddf$avg))
  }

  colnames(ndf)<- c('theDate', 'Values')
    ## IOT endpoint doesn't really allow selection of time period so we do it manually here
    if(!is.null(startDate) & !is.null(endDate)){

      # odf <- ndf[ndf$dt >= as.Date(startDate, format = "%Y-%m-%dT%H:%M:%S") & ndf$dt <= as.Date(endDate, format = "%Y-%m-%d"),]
       odf <- ndf[ndf$theDate >= as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%OS") & ndf$theDate <=  as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%OS"),]
       return(odf)
    #    if(nrow(odf) == 0){
    #      outList <-   vector("list")
    #      return(outList)
    #    }else{
    #      ndf <- data.frame(dts2, vals)
    #
    #      return(ndf)
    #    }
    # }else{
    #   ndfRange <- ndf
    # }
    }

    return(ndf)

}






