library(RCurl)
library(jsonlite)
library(httr)
library(stringr)
library(readr)

# url <- 'https://services.cerdi.edu.au/sfs/v1.0/Datastreams(20)/Observations/aggregate/day'
usr <- 'Ross.Searle@csiro.au'
pwd <- 'uT8tGtyZSUqL'

rootDir = 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'


thingsURL <- 'https://services.cerdi.edu.au/sfs/v1.0/Things?$expand=locations'
thingsresp <- GET(thingsURL,  authenticate(usr, pwd))
thingsresponse <- content(thingsresp, "text")
thingsDf <- fromJSON(thingsresponse)[[1]]


siteDF <- data.frame()
for (i in 1:nrow(thingsDf)) {
  
  rec <- thingsDf[i, ]
  print(i)
  
  sid <- rec$'@iot.id'
  
  loc <- rec$Locations
  lon <- loc[[1]]$location$geometry$coordinates[[1]][1]
  lat <- loc[[1]]$location$geometry$coordinates[[1]][2]
  
  recDF <- data.frame(SiteID=paste0('SFS_',rec$'@iot.id'), SiteName=rec$name, SensorGroup=providerInfo$provider, Backend=providerInfo$backEnd,
                      Access=providerInfo$access,Usr=providerInfo$usr,Pwd=providerInfo$pwd,Latitude=lat,Longitude=lon,
                      Owner=providerInfo$org,Contact=providerInfo$contact,ProviderURL=providerInfo$orgURL,NetworkInfoWebsite=providerInfo$orgURL,
                      Description=providerInfo$Description,ServerName=providerInfo$server
                      )
                      
                      siteDF <- rbind(siteDF, recDF)
                      Sys.sleep(1)
}

outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites2.csv')
write.csv(siteDF, outName, row.names = F, quote = F)



###### Extract sensor info 

sensorDF <- data.frame()

for (i in 46:nrow(thingsDf)) {
  
  rec <- thingsDf[i, ]
  print(i)
  
  sid <- rec$'@iot.id'
  siteName <- rec$name
  
  
  datastreamURL <- paste0('https://services.cerdi.edu.au/sfs/v1.0/Things(', sid,')/Datastreams')
  datastreamresp <- GET(datastreamURL,  authenticate(usr, pwd))
  datastreamresponse <- content(datastreamresp, "text")
  datastreamDf <- fromJSON(datastreamresponse)[[1]]
  
  if(length(datastreamDf) > 0){
  
  sensors <-  datastreamDf$`Sensor@iot.navigationLink`
  ids <- datastreamDf$'@iot.id'
  
  siteSensdf <- data.frame()
  for (j in 1:length(sensors)) {
      url <- sensors[j]
      sensorresp <- GET(url,  authenticate(usr, pwd))
      sensorresponse <- content(sensorresp, "text")
      sensorDf <- fromJSON(sensorresponse)
      
      sensID <- ids[j]
      
      dtraw <- suppressWarnings( parse_number(sensorDf$name ))
      
     if(is.na(dtraw) ){
       dt <- sensorDf$name
       depth <- 0
     }else{
       depth <- dtraw
       dt <- str_remove(sensorDf$name, as.character(depth))
     }
      
      units <-  datastreamDf$unitOfMeasurement$name[j]
      
      outSenDf <- data.frame(SiteID=paste0('SFS_', sid), SensorID=sensID, SensorName=paste0('SFS_', siteName, '_', dt, '_', depth),
                              DataType=dt, UpperDepth=depth, LowerDepth=depth,
                             Calibrated = 'TRUE', Units=units, IsActive='TRUE')
      siteSensdf <- rbind( siteSensdf , outSenDf)
  }
  
  sensorDF <- rbind(sensorDF, siteSensdf)
   
  Sys.sleep(20)
  }
}



outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorRaw.csv')

write.csv(sensorDF, outName, row.names = F, quote = F)

allDf <- read.csv(outName, stringsAsFactors = F)

sensorDF <- data.frame()


sensdf <- allDf

unique(sensdf$DataType)

sensdf <- sensdf[!sensdf$DataType == 'SoilMoistureSummed-1000', ]  # delete these records
sensdf <- sensdf[!sensdf$DataType == 'Battery', ]  # delete these records
sensdf <- sensdf[!sensdf$DataType == 'WindSpeedGust', ]  # delete these records
sensdf <- sensdf[!sensdf$DataType == 'InternalTemperature', ]


#sensdf$DataType[sensdf$UpperDepth !=0 & sensdf$DataType == 'Temperature'] <- "Soil-Temperature" # fix soil temperature records
sensdf$DataType[sensdf$DataType == 'Precipitation'] <- "Rainfall"
sensdf$DataType[sensdf$DataType == 'SoilMoisture'] <- 'Soil-Moisture'
sensdf$DataType[sensdf$DataType == 'WindSpeedAvg'] <- 'Wind-Speed'
sensdf$DataType[sensdf$DataType == 'RelativeHumidity'] <- 'Humidity'
sensdf$DataType[sensdf$DataType == 'SoilTemperature'] <- 'Soil-Temperature'
sensdf$DataType[sensdf$DataType == 'AirTemperature'] <- 'Temperature'
sensdf$DataType[sensdf$DataType == 'BarometricPressure'] <- 'Atmospheric-Pressure'


unique(sensdf$Units)
sensdf$Units[sensdf$Units == 'Celsius'] <- 'Degrees-Celcius'
sensdf$Units[sensdf$Units == 'Millimeter'] <- 'mm'
sensdf$Units[sensdf$Units == 'Percentage'] <- 'Percent'
sensdf$Units[sensdf$Units == 'Kilometres per hour'] <- 'km/hr'
sensdf$Units[sensdf$Units == 'Hectopascal'] <- 'hPa'

# unique(sensdf$DataType)
# units <- c('Soil-Moisture'='mm', Rainfall='mm' , 'Soil-Temperature'='Degrees-Celcius', Temperature='Degrees-Celcius','Wind-Speed'='km/hr',Humidity='Percent','Atmospheric-Pressure'='hPa')
# 
# outSenDf <- data.frame(SiteID=paste0(sensdf$SiteID), Active=1, SensorID=sensdf$SensorID, SensorName=paste0(sensdf$SiteName, '_', sensdf$DataType, '_', sensdf$UpperDepth),
#                        StartDate=sensdf$StarDate,EndDate=sensdf$EndDate, DataType=sensdf$DataType, UpperDepth=sensdf$UpperDepth, LowerDepth=sensdf$LowerDepth,
#                        Calibrated = 'TRUE', Units=as.character(units[sensdf$DataType], IsActive='TRUE')
# )

outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sensors2.csv')
write.csv(sensdf, outName, row.names = F, quote = F)

inName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sensors2.csv')
outSenDf <- read.csv(inName, stringsAsFactors = F)
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
dbWriteTable(con, 'Sensors', outSenDf, append = T)
dbDisconnect(con)




