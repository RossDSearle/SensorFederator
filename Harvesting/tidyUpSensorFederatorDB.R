library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(DBI)
library(RSQLite)
library(stringr)


machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'FANCY-DP'){
  rootDir <-  'C:/Projects/SensorFederator'
  dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
}else {
  dbPath <- ""
}


source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/HarvestUtils.R')



n <- as.character(Sys.time(),format='%y-%m-%dT%H-%M-%S')
file.copy(dbFedPath, paste0(dirname(dbFedPath), '/SensorFederator_',n, '.sqlite' ))


conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
fk_On <- 'PRAGMA foreign_keys = ON;'
dbExecute(conFed, fk_On)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RO)



##########  Tidy up calibration column    ############################
sql <- 'Select * from sensors'
recs <- doQuery(conFed, sql)
nrow(recs)
unique(recs$Calibrated)
upSql <- "UPDATE sensors SET Calibrated = 'TRUE' where Calibrated = '1'"
upSql <- "UPDATE sensors SET Calibrated = 'FALSE' where Calibrated = '0'"
sendStatement(conFed, upSql)





sql <- "Select * from sensors where EndDate = 'Nono'"
recs <- doQuery(conFed, sql)
nrow(recs)
upSql <- "UPDATE sensors SET EndDate = 'None' where EndDate = 'Nono'"
sendStatement(conFed, upSql)

##########  Tidy up DataType column    ############################
knownFeatures <- c('Soil-Moisture', 'Soil-Temperature', 'Rainfall', 'Humidity', 'Temperature', 'Wind-Direction',
                   'Wind-Speed', 'Atmospheric-Pressure', 'Vapour-Pressure', 'Dew-Point', 'Delta T', 'Soil-Suction', 'Solar-Radiation')

sql <- "Select * from sensors"
recs <- doQuery(conFed, sql)
nrow(recs)
unique(recs$DataType)

sql <- "Select * from sensors where DataType = 'NA'"
recs <- doQuery(conFed, sql)
nrow(recs)

upSql <- "UPDATE sensors SET DataType = 'Solar-Radiation' where DataType = 'Global Solar Irradiance'"
sendStatement(conFed, upSql)


Soil-Suction

###########################   Fix up issues with Boowora Data  ###############################################

sql <- "Select * from sensors where SiteID LIKE '%hussat_terrasonde%' and DataType = 'EC'"
recs <- doQuery(conFed, sql)
nrow(recs)
delSql <- "DELETE from sensors where SiteID LIKE '%hussat_terrasonde%' and DataType = 'EC'"
sendStatement(conFed, delSql)
delSql <- "DELETE from sensors where SiteID LIKE '%hussat_terrasonde%' and DataType = 'Soil_Temperature'"
sendStatement(conFed, delSql)

upSql <- "UPDATE sensors SET Units = 'None' where SiteID LIKE '%hussat_terrasonde%' and DataType = 'Soil-Moisture'"
sendStatement(conFed, upSql)
upSql <- "UPDATE sensors SET Calibrated = 'FALSE' where SiteID LIKE '%hussat_terrasonde%' and DataType = 'Soil-Moisture'"
sendStatement(conFed, upSql)



#########################   Vic Ag Fixes  #################################################
sql <- "Select * from sensors where SiteID LIKE '%VicAg_%' and UpperDepth <> 0"
recs <- doQuery(conFed, sql)
nrow(recs)
upSql <- "UPDATE sensors SET UpperDepth = (UpperDepth * 10) where SiteID LIKE '%VicAg_%' and UpperDepth <> 0"
upSql <- "UPDATE sensors SET LowerDepth = (LowerDepth * 10) where SiteID LIKE '%VicAg_%' and UpperDepth <> 0"

upSql <- "UPDATE sensors SET UpperDepth = (UpperDepth / 10) where SiteID LIKE '%VicAg_%' and UpperDepth <> 0"
upSql <- "UPDATE sensors SET LowerDepth = (LowerDepth / 10) where SiteID LIKE '%VicAg_%' and UpperDepth <> 0"

sendStatement(conFed, upSql)

upSql <- "UPDATE sensors SET Calibrated = 'TRUE' where SiteID LIKE '%VicAg_%'"
sendStatement(conFed, upSql)


upSql <- "UPDATE sensors SET Units = 'Percent' where SiteID LIKE '%VicAg_%' and DataType = 'Soil-Moisture'"
upSql <- "UPDATE sensors SET Units = 'Degrees-Celcius' where SiteID LIKE '%VicAg_%' and DataType = 'Soil-Temperature'"
upSql <- "UPDATE sensors SET Units = 'Percent' where SiteID LIKE '%VicAg_%' and DataType = 'Humidity'"
upSql <- "UPDATE sensors SET Units = 'Degrees-Celcius' where SiteID LIKE '%VicAg_%' and DataType = 'Temperature'"
upSql <- "UPDATE sensors SET Units = 'Degrees' where SiteID LIKE '%VicAg_%' and DataType = 'Wind-Direction'"
upSql <- "UPDATE sensors SET Units = 'mm' where SiteID LIKE '%VicAg_%' and DataType = 'Rainfall'"
upSql <- "UPDATE sensors SET Units = 'km/hr' where SiteID LIKE '%VicAg_%' and DataType = 'Wind-Speed'"

sendStatement(conFed, upSql)


#########################   EPARF Fixes  #################################################

sql <- "Select * from sensors where SiteID LIKE '%opSID_%'"
recs <- doQuery(conFed, sql)
nrow(recs)
upSql <- "UPDATE sensors SET Calibrated = 'FALSE' where SiteID LIKE '%opSID_%'"
upSql <- "UPDATE sensors SET UpperDepth = (UpperDepth * 10) where SiteID LIKE '%opSID_%' and UpperDepth <> 0"
upSql <- "UPDATE sensors SET LowerDepth = (LowerDepth * 10) where SiteID LIKE '%opSID_%' and UpperDepth <> 0"

sendStatement(conFed, upSql)




#########################   Cosmoz Fixes  #################################################

sql <- "Select * from sensors where SiteID LIKE '%Cosmoz_%'"
recs <- doQuery(conFed, sql)
nrow(recs)
upSql <- "UPDATE sensors SET Calibrated = 'TRUE' where SiteID LIKE '%Cosmoz_%'"

sendStatement(conFed, upSql)


#########################   SFS Fixes  #################################################
sql <- "Select * from sensors where SiteID LIKE '%SFS_%'"
recs <- doQuery(conFed, sql)
nrow(recs)
upSql <- "UPDATE sensors SET Calibrated = 'TRUE' where SiteID LIKE '%SFS_%'"
upSql <- "UPDATE sensors SET Units = 'Percent' where SiteID LIKE '%SFS_%' and DataType = 'Soil-Moisture'"
sendStatement(conFed, upSql)

