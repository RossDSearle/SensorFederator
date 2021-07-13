
# This script is at the top level of the infrsstructure - it needs to be sourced for other stuff to work


library(RCurl)
library(jsonlite)
library(stringr)
library(zoo)
library(xts)
library(rgdal)
library(raster)
library(reshape)
library(urltools)
library(lubridate)
library(async)
library(xml2)
library(htmlTable)
library(DBI)
library(RSQLite)
library(RColorBrewer)
library(sf)
library(httr)
library(XML)
library(xml2)
library(httr)

debugMode <- F

machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'WALCOT-SL'){
  #rootDir <<- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/Shiny/SMIPS'
  sensorRootDir <<- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/Git/SensorFederator'
  functionsRootDir <<- 'C:/Users/sea084/OneDrive - CSIRO/RossRCode/myFunctions'
  senFedDbPath <- paste0('C:/Temp/ozNetDB.db')
  dbPath <- paste0("C:/Users/sea084/OneDrive - CSIRO/ProjectAdmin/SensorFederator/DB/SensorFederator.sqlite")
}else if (machineName == 'soils-discovery' & debugMode == T) {
  #rootDir <<- '/home/sea084/R/SMIPS'
  sensorRootDir <<- '/home/sea084/R/SensorBackends'
  functionsRootDir <<- '/home/sea084/R/myFunctions'
  senFedDbPath <- paste0('/mnt/data/SensorFedDB/ozNetDB.db')
  dbPath <- paste0( "/srv/DB/SensorFederator/SensorFederator.sqlite")
}else if (machineName == 'soils-discovery' & debugMode == F) {
  #rootDir <<- '/srv/shiny-server/SMIPS'
  sensorRootDir <<- '/srv/plumber/SensorFederator'
  functionsRootDir <<- '/srv/plumber/Functions'
  senFedDbPath <- paste0('/mnt/data/SensorFedDB/ozNetDB.db')
  dbPath <- paste0( "/srv/DB/SensorFederator/SensorFederator.sqlite")
}


timeAggMethods <- data.frame(mean='mean', sum='sum', min='min', max='max', none='none', stringsAsFactors = F)

knownBackends <- c('Senaps', 'Adcon', 'OutPost','Outpost2', 'Cosmoz', 'DAFWA', 'Mait', 'DataFarmer', 'SenFedStore', 'IOT_CERDI', 'BoM_Latest', 'SILO', 'EPARF')

knownFeatures <- c('Soil-Moisture', 'Soil-Temperature', 'Rainfall', 'Humidity', 'Temperature', 'Wind-Direction', 'Wind-Speed', 'Atmospheric-Pressure', 'Vapour-Pressure', 'Dew-Point', 'Delta T', 'Soil-Suction', 'Solar-Radiation', 'PAW', 'Salinity')
FeatureAggTypes <-c(timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$sum, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean, timeAggMethods$mean)
names(FeatureAggTypes) <- knownFeatures

timeSteps <- data.frame(none='none', minutes='minutes', hours='hours', days='days', weeks='weeks', months='months', quarters='quarters', years='years', stringsAsFactors = F)
timeStepDurations <- data.frame(none=0, minutes=60, hours=3600, days=86400, weeks=604800, months=2592000, quarters=7948800, years=31536000, stringsAsFactors = F)


apiFormats <- data.frame(simpleTS='simpleTS', nestedTS='nestedTS', stringsAsFactors = F)




defaultStartTime <- '09:00:00'
asyncThreadNum = 10
maxRecs = '1000000'
globalTimeOut = 200








#dbPath <- paste0(sensorRootDir, "/DB/SensorFederator.sqlite")







source(paste0(functionsRootDir,'/GeneralUtils.R'))
source(paste0(functionsRootDir,'/VectorUtils.R'))
source(paste0(sensorRootDir, '/Backends/RequestChecks.R'))
source(paste0(sensorRootDir, '/Backends/Backend_Utils.R'))
source(paste0(sensorRootDir, '/Backends/Adcon_Backend.R'))
source(paste0(sensorRootDir, '/Backends/Outpost_Backend.R'))
source(paste0(sensorRootDir, '/Backends/Outpost2_Backend.R'))
source(paste0(sensorRootDir, '/Backends/Senaps_Backend.R'))
source(paste0(sensorRootDir, '/Backends/Cosmoz_Backend.R'))
source(paste0(sensorRootDir, '/Backends/DAFWA_Backend.R'))
source(paste0(sensorRootDir, '/Backends/Mait_Backend.R'))
source(paste0(sensorRootDir, '/Backends/DataFarmer_Backend.R'))
source(paste0(sensorRootDir, '/Backends/SensFedStore_Backend.R'))
source(paste0(sensorRootDir, '/Backends/IOT_Backend.R'))
source(paste0(sensorRootDir, '/Backends/BoM-Latest_Backend.R'))
source(paste0(sensorRootDir, '/Backends/EPARF_Backend.R'))
source(paste0(sensorRootDir, '/Backends/Backends.R'))
source(paste0(sensorRootDir, '/Backends/Authorisation.R'))











