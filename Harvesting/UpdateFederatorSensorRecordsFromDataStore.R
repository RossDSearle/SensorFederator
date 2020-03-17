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

daysForActive <- 30
dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")






n <- as.character(Sys.time(),format='%y-%m-%dT%H-%M-%S')
file.copy(dbFedPath, paste0(dirname(dbFedPath), '/SensorFederator_',n, '.sqlite' ))

conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
fk_On <- 'PRAGMA foreign_keys = ON;'
dbExecute(conFed, fk_On)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RO)


sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE (((Sites.SensorGroup)<>'BoM'));"
allSensors <- doQuery(conFed, sql)


pb <- pbCreate(nrow(allSensors), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

for (i in 1:nrow(allSensors)) {

  rec <- allSensors[i, ]

  sqlsens <- paste0("select * from Sensors where  SiteID='", rec$SiteID, "' and SensorID='", rec$SensorID, "' and upperDepth='", rec$UpperDepth, "' and lowerDepth='", rec$LowerDepth, "' and DataType='", rec$DataType, "'")
  existingSensorData <- doQuery(conStore, sqlsens)

  validTS <- T

  if(nrow(existingSensorData) > 0){

        sNum <- existingSensorData$sensorNum

        sql <-  paste0("SELECT * from sensorData where sensorNum = " , sNum  )
        dataRecs <- doQuery(conStore, sql)

        ts <- xts(dataRecs[,3], order.by=as.Date(dataRecs[,2]))

        if(all(is.na(ts))){
          validTS <- F
        }

        if(nrow(ts) < 4){
          validTS <- F
        }


        ped <- getPeriod(ts)
        tsStart <- paste0(ped$startDate, 'T00:00:00')
        tsEnd <- paste0(ped$endDate, 'T23:59:59')
        tsNumDays <- as.numeric(as.Date(ped$endDate) - as.Date(ped$startDate))
        tsActive <- sensorIsActive(ts, daysForActive)
        g <- gapiness(ts)
        tsNumGaps <- g$numGaps
        tsTotalGapDays <- g$numDays
        tsGapiness <- g$gapiness
        tsMin <- min(ts, na.rm = T)
        tsMax <- max(ts, na.rm = T)
        tsMean <- mean(ts, na.rm = T)
        tsSD <- sd(ts, na.rm = T)

        }else{
          validTS <- F
        }

        if(validTS){
        sqlUp <- paste0("UPDATE sensors
                          SET StartDate = '", tsStart, "', ",
                        "EndDate = '", tsEnd, "', ",
                        "TotalDays = ", tsNumDays, ", ",
                        "IsActive = '", tsActive, "', ",
                        "NumGaps = ", tsNumGaps, ", ",
                        "TotalGapDays = ", tsTotalGapDays, ", ",
                        "Gapiness = ", tsGapiness, ", ",
                        "MinimumValue = ", tsMin, ", ",
                        "MaximumValue = ", tsMax, ", ",
                        "MeanValue = ", tsMean, ", ",
                        "StandardDeviation = ", tsSD, ", ",
                        "HarvestDate = '", Sys.Date(), "', ",
                        "hasdata = 'TRUE'", " ",
                        "WHERE SiteID='", rec$SiteID, "' and SensorID='", rec$SensorID, "' and upperDepth='", rec$UpperDepth, "' and lowerDepth='", rec$LowerDepth, "' and DataType='", rec$DataType, "';")

        }else{

          ####   No data in the DataStore ##############
          sqlUp <- paste0("UPDATE sensors
                          SET StartDate = 'None', ",
                          "EndDate = 'None', ",
                          "TotalDays = -1, ",
                          "IsActive = 'FALSE', ",
                          "NumGaps = -1, ",
                          "TotalGapDays = -1, ",
                          "Gapiness = -1, ",
                          "MinimumValue = -1, ",
                          "MaximumValue = -1, ",
                          "MeanValue = -1, ",
                          "StandardDeviation = -1, ",
                          "HarvestDate = '", Sys.Date(), "', ",
                          "hasdata = 'FALSE'", " ",
                          "WHERE SiteID='", rec$SiteID, "' and SensorID='", rec$SensorID, "' and upperDepth='", rec$UpperDepth, "' and lowerDepth='", rec$LowerDepth, "' and DataType='", rec$DataType, "';")
        }

  sendStatement(conFed, sqlUp)

  pbStep(pb, step=i)

}


pbClose(pb)



























write.csv(outDF, paste0(rootDir, '/MeataDataHarvest_on_', Sys.Date(), '.csv'), row.names = F)



metaD <- read.csv(paste0(rootDir, '/MeataDataHarvest_on_', Sys.Date(), '.csv'), stringsAsFactors = F)
metaD[mapply(is.infinite, metaD)] <- 'NULL'
metaD[mapply(is.na, metaD)] <- 'NULL'
head(metaD)


con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
#dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"

pb <- pbCreate(nrow(metaD), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1
for (i in 1:nrow(metaD)) {

  id <- metaD$SiteID[i]

  sql <- paste0("UPDATE sensors
  SET StartDate = '", metaD$StartDate[i], "',
      EndDate = '", metaD$EndDate[i], "',
      TotalDays = ", metaD$TotalDays[i], ",
      IsActive = '", metaD$IsActive[i], "',
      NumGaps = ", metaD$NumGaps[i], ",
      TotalGapDays = ", metaD$TotalGapDays[i], ",
      Gapiness = ", metaD$Gapiness[i], ",
      MinimumValue = ", metaD$MinimumValue[i], ",
      MaximumValue = ", metaD$MaximumValue[i], ",
      MeanValue = ", metaD$MeanValue[i], ",
      StandardDeviation = ", metaD$StandardDeviation[i], ",
      HarvestDate = '", metaD$MetaDataHarvestDate[i], "T23:59:59'

  WHERE SiteID = '", metaD$SiteID[i], "' and SensorID = '", metaD$SensorID[i], "';")

  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)
  pbStep(pb, step=i)

}

pbClose(pb)




