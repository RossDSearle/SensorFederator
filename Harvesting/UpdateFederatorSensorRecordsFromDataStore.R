library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(DBI)
library(RSQLite)


dev = T
machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'FANCY-DP'){
  rootDir <-  'C:/Projects/SensorFederator'
  dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
}else {
  dbFedPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
}

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/HarvestUtils.R')
qualParams <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/TSQualityParameters.csv', stringsAsFactors = F)

daysForActive <- 30
dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")


conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)


n <- as.character(Sys.time(),format='%y-%m-%dT%H-%M-%S')
file.copy(dbFedPath, paste0(dirname(dbFedPath), '/SensorFederator_',n, '.sqlite' ))

conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
fk_On <- 'PRAGMA foreign_keys = ON;'
dbExecute(conFed, fk_On)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RO)


sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE (((Sites.SensorGroup)<>'BoM'));"
#sql <- "Select * from Sensors where DataType = 'Soil-Moisture' and HasData = 'TRUE'"
allSensors <- doQuery(conFed, sql)


pb <- pbCreate(nrow(allSensors), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

odf <- data.frame(stringsAsFactors = F)
for (i in 1:nrow(allSensors)){
  print(i)

  rec <- allSensors[i,]
  qP <- qualParams[qualParams$Dtype == rec$DataType, ]

  sqlSenNum <- paste0("select * from Sensors where SiteID='", rec$SiteID, "' and upperDepth = '", rec$UpperDepth,
                      "' and lowerDepth = '", rec$LowerDepth, "' and DataType = '", rec$DataType, "'")
  storeSens <- doQuery(conStore, sqlSenNum)
  if(nrow(storeSens)>0){
    sensorNum <- storeSens$sensorNum[1]

    d <- getSensorData(conStore, sensorNum)
    ts <- xts(d[,3], order.by=as.Date(d[,2]))
    q <- assessTSQuality(ts, verbose = T, maxVal=qP$maxVal ,  minVal=qP$minVal, minNumDays=qP$minNumDays, desiredNumDays=qP$desiredNumDays, numDaysForActive = qP$numDaysForActive)

    df <- data.frame(rec$SiteID, rec$DataType, rec$UpperDepth, rec$LowerDepth,
                     format(q$QualityIndex, digits=2), q$LowestIndexName, format(q$LowestIndexValue, digits=2),
                     format(q$IinRange, digits=2), format(q$IvalidProp, digits=2), format(q$InumGaps, digits=2),
                     format(q$ItotalGapsDays, digits=2), format(q$IdesiredNumDays, digits=2),
                     q$Start, q$End,
                     q$TotalNumDays, q$TotalValidDays, format(q$validProportion, digits=2), q$numGaps, q$numGapDays, q$numInRange,
                     q$StandardDeviation, q$MeanValue, q$IsActive, q$HarvestDate, q$HasData,
                     stringsAsFactors = F)

    odf<-rbind(odf, df)

    sqlUp <- getQualitySQL(q)
    sendStatement(conFed, sqlUp)

  }else{
    q <- getBlankQualityRecord()
    sqlUp <- getQualitySQL(q)
    sendStatement(conFed, sqlUp)
  }
  pbStep(pb, step=i)
}




# for (i in 1:nrow(allSensors)){
#   rec <- allSensors[i,]
#   sqlSenNum <- paste0("select * from Sensors where SiteID='", rec$SiteID, "' and upperDepth = '", rec$UpperDepth,"' and lowerDepth = '", rec$LowerDepth, "' and DataType = '", rec$DataType, "'")
#   storeSens <- doQuery(conStore, sqlSenNum)
#   if(nrow(storeSens) > 1){
#     #print(paste0(rec$SiteID, " ", rec$UpperDepth, " ", rec$LowerDepth, " ", rec$DataType))
#     print(i)
#     print(sqlSenNum)
#
#   }
# }



