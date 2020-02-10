library(DBI)
library(stringr)
library(RSQLite)

usrpwd <- paste0(providerInfo$usr, ':', providerInfo$pwd)
platforms <- getURL(paste0(providerInfo$server, "/platforms?&groupids=boorowa-soil&expand=true"), userpwd=usrpwd, httpauth = 1L)




locJ <- getURL(paste0("https://senaps.io/api/sensor/v2/locations?groupids=boorowa-soil&expand=true"), userpwd=usrpwd, httpauth = 1L)
locs <-  fromJSON(locJ)


streamsJ <- getURL(paste0("https://senaps.io/api/sensor/v2/streams?expand=true&recursive=false&groupids=boorowa-soil&locationid=hussat_terrasonde_148.689633_-34.468953"), userpwd=usrpwd, httpauth = 1L)
streams <-  fromJSON(streamsJ)
writeLines(streamsJ, 'c:/temp/streams.json')

obssJ <- getURL(paste0("https://senaps.io/api/sensor/v2/observations?streamid=hussat.boorowa-soil-moisture.terrasonde.135.rssi"), userpwd=usrpwd, httpauth = 1L)
obs <- fromJSON(obssJ)


plocs <- locs$`_embedded`$locations$geojson[1:36,]
as.data.frame(plocs$coordinates)

bits <- str_split(plocs$coordinates, ' ')
lats <- as.numeric(sapply(bits, function (x) x[2]))
lons <- as.numeric(sapply(bits, function (x) x[1]))

generateSiteInfo_SC <- function(providerInfo, rootDir){
  
  locJ <- getURL(paste0("https://senaps.io/api/sensor/v2/locations?groupids=boorowa-soil&expand=true"), userpwd=usrpwd, httpauth = 1L)
  locs <-  fromJSON(locJ)
  
    outDF <- data.frame()
    for (i in 1:36){
       print(i)
       loc <-  locs$`_embedded`$locations$geojson[i,]
       name <- locs$`_embedded`$locations$description[i]
       id <- locs$`_embedded`$locations$id[i]
       lat <- loc$coordinates[[1]][2]
       lon <- loc$coordinates[[1]][1]
       elev <- loc$coordinates[[1]][3]
       df <- data.frame(Name=name, ID=id,Latitude=lat, Longitude=lon, Elevation=elev)
       outDF <- rbind(outDF,df)
    }
    
    locs <- data.frame(outDF$ID, outDF$Name,providerInfo$provider, providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, outDF$Latitude,   outDF$Longitude, providerInfo$org, providerInfo$contact, providerInfo$orgURL, '', stringsAsFactors = F)
    colnames(locs) <- c('SiteID', 'SiteName', 'SensorGroup', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude',  'Owner', 'Contact', 'ProviderURL', 'Description')
    
    outName <- paste0(rootDir, '/', providerInfo$provider, '_Sites.csv')
    write.csv(locs, outName, row.names = F, quote = F)
    cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
    #vc(outName)

}


write.csv(outDF, 'C:/Projects/Booroowa/Senaps/locs.csv')
locsDF <- read.csv('C:/Projects/Booroowa/Senaps/locs.csv')

for (i in 1:nrow(locsDF)) {
  
  r <- locsDF[i,]
  streamsJ <- getURL(paste0("https://senaps.io/api/sensor/v2/streams?expand=true&recursive=false&groupids=boorowa-soil&locationid=", r$ID), userpwd=usrpwd, httpauth = 1L)
  streams <-  fromJSON(streamsJ)
  
  locStreams <- streams$`_embedded`$streams$id
  
}


generateSensorInfo_SC <- function(providerInfo, rootDir){
  
  sites <- read.csv(paste0(rootDir, '/', providerInfo$provider, '_Sites.csv'), stringsAsFactors = F)
  #pb <- pbCreate(nrow(sites), progress='text', style=3, label='Generating Sensor data.....',timer=TRUE)
  
  sensorDF <- data.frame()
  for(i in 1:nrow(sites)){
    print(i)
    
    id <- sites$SiteID[i]
    #pbStep(pb, i)
    streamsJ <- getURL(paste0("https://senaps.io/api/sensor/v2/streams?expand=true&recursive=false&groupids=boorowa-soil&locationid=", id), userpwd=usrpwd, httpauth = 1L)
    streams <-  fromJSON(streamsJ)
    
    locStreams <- streams$`_embedded`$streams$id
    
    df <- data.frame( sites$SiteID[i], 1,streams$`_embedded`$streams$id, '', '2019-09-01T:00:00:00','2020-02-09T:00:00:00' , '', '', '', 0, '', 1, NA,  NA, NA, NA, NA, NA, NA,  stringsAsFactors = F)
    colnames(df) <- c('SiteID', 'Active', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units',  'IsActive',  'NumGaps', 'Gapiness','MinimumValue', 'MaximumValue' , 'MeanValue' , 'StandardDeviation','HarvestDate'    )
    sensorDF <- rbind(sensorDF, df)
    
  }
 
  colnames(sensorDF)
  
  sensorDF$DataType[grepl('electrical_conductivity', sensorDF$SensorID )] <- 'EC'
  sensorDF$DataType[grepl('soil_temperature', sensorDF$SensorID )] <- 'Soil_Temperature'
  sensorDF$DataType[grepl('dielectric_constant', sensorDF$SensorID )] <- 'Dielectric_Constant'
  
  sensorDF <- sensorDF[!grepl('via_basestation', sensorDF$SensorID ),]
  sensorDF <- sensorDF[!grepl('seq_at_zero_sequence_set', sensorDF$SensorID ),]
  sensorDF <- sensorDF[!grepl('.seq', sensorDF$SensorID ),]
  sensorDF <- sensorDF[!grepl('.rssi', sensorDF$SensorID ),]
  sensorDF <- sensorDF[!grepl('.rssi_noise_floor', sensorDF$SensorID ),]
  
  
  sensorDF$UpperDepth[grepl('_0_', sensorDF$SensorID )] <- 300
  sensorDF$LowerDepth[grepl('_0_', sensorDF$SensorID )] <- 300
  sensorDF$UpperDepth[grepl('_1_', sensorDF$SensorID )] <- 400
  sensorDF$LowerDepth[grepl('_1_', sensorDF$SensorID )] <- 400
  sensorDF$UpperDepth[grepl('_2_', sensorDF$SensorID )] <- 500
  sensorDF$LowerDepth[grepl('_2_', sensorDF$SensorID )] <- 500
  sensorDF$UpperDepth[grepl('_3_', sensorDF$SensorID )] <- 600
  sensorDF$LowerDepth[grepl('_3_', sensorDF$SensorID )] <- 600
  sensorDF$UpperDepth[grepl('_4_', sensorDF$SensorID )] <- 700
  sensorDF$LowerDepth[grepl('_4_', sensorDF$SensorID )] <- 700
  sensorDF$UpperDepth[grepl('_5_', sensorDF$SensorID )] <- 800
  sensorDF$LowerDepth[grepl('_5_', sensorDF$SensorID )] <- 800
  sensorDF$UpperDepth[grepl('_6_', sensorDF$SensorID )] <- 900
  sensorDF$LowerDepth[grepl('_6_', sensorDF$SensorID )] <- 900
  sensorDF$UpperDepth[grepl('_7_', sensorDF$SensorID )] <- 1000
  sensorDF$LowerDepth[grepl('_7_', sensorDF$SensorID )] <- 1000
  
  sensorDF$SensorName <- paste0('Boowoora_', str_replace(sensorDF$SensorID, 'hussat.boorowa-soil-moisture.terrasonde.', ''))
  

  
  outName <- paste0(rootDir, '/', providerInfo$provider, '_SensorsAll.csv')
  write.csv(sensorDF, outName, row.names = F, quote = F)
  pbClose(pb)
  cat(paste0('Sensor info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  cat('\n')
  cat('OK. Now manually curate this file to expose the data you want\n')
  cat("Don't forget to recompile the 'AllSensors.csv' & 'AllSites.csv' files afetr these changes\n")
  vc(outName)
}




obssJ <- getURL(paste0("https://senaps.io/api/sensor/v2/observations?streamid=hussat.boorowa-soil-moisture.terrasonde.135.sensor_6_dielectric_constant"), userpwd=usrpwd, httpauth = 1L)
obs <- fromJSON(obssJ)




rootDir <- 'C:/Projects/Booroowa/Senaps'
providerInfo <- list(provider=c('Booroowa'), backEnd=c('Senaps'), server=c('https://senaps.io/api/sensor/v2'), org=c('CSIRO'),
                     usr=c('ross.searle@csiro.au'), pwd=c('QBP'),
                     access=c('Public'),
                     contact=c('Stuart.Brown@csiro.au'), orgURL=c('https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station'))



#providerInfo = list( provider= c('cosmoz'), backEnd=c('SensorCloud'), access = c('Public'), org=c('CSIRO'), usr=c(usr), pwd=c(pwd), contact=c('David.Mcjannet@csiro.au'), orgURL=c('http://cosmoz.csiro.au/'))
generateSiteInfo_SC(providerInfo, rootDir)
generateSensorInfo_SC(providerInfo, rootDir)


bSites <- read.csv('C:/Projects/Booroowa/Senaps/Booroowa_Sites.csv')
bSensors <- read.csv('C:/Projects/Booroowa/Senaps/Booroowa_SensorsAll.csv')

con <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite', flags = SQLITE_RW)
dbWriteTable(con, 'sites', bSites, append =T)
dbWriteTable(con, 'sensors', bSensors, append =T)
