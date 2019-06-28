library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(DBI)
library(RSQLite)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')

getPeriod <- function(ts){

  data.frame(startDate=start(ts), endDate=end(ts), stringsAsFactors = F)
}

gapiness <- function(ts){
  tz <- na.omit(ts)
  gap_ts_Start=index(tz[ which( diff(index(tz))>1 )])
  gap_ts_End=index(tz[ which( diff(index(tz))>1 ) +1 ])

  ngaps <- length(gap_ts_Start)
  numdays <- gap_ts_End - gap_ts_Start
  gapi <- as.numeric(sum(numdays) / length(ts)) * 100

  return(data.frame(numGaps=ngaps, numDays=as.numeric(sum(numdays)), gapiness=gapi, stringsAsFactors = F))
}

sensorIsActive <- function(ts, days){

  lastRec <- as.Date(end(ts))
  if(Sys.Date()-lastRec > days){
    return(F)
  }else{
    return(T)
  }

}

rootDir <-  'C:/Projects/SensorFederator'
daysForActive <- 30

fls <- list.files(paste0(rootDir, '/DailyTS'), '.csv', full.names = T)


outDF <- data.frame( stringsAsFactors = F)

pb <- pbCreate(length(fls), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

for (i in 1:length(fls)) {
  f <- fls[i]
  fname <- basename(f)
  bits <- str_split(str_replace(fname, '.csv', ''), '!')
  SiteID <- bits[[1]][1]
  SensorID <- bits[[1]][2]
  UpperDepth <- bits[[1]][3]
  LowerDepth <- bits[[1]][4]
  DataType <- bits[[1]][5]
  df <- read.csv(f, stringsAsFactors = F)
  ts <- xts(df[,2], order.by=as.Date(df[,1]))

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
  #periodicity(ts)

  recDf <- data.frame(SiteID = SiteID,
                      SensorID =  SensorID,
                      UpperDepth = UpperDepth,
                      LowerDepth = LowerDepth,
                      DataType = DataType,
                      StartDate = tsStart,
                      EndDate = tsEnd,
                      TotalDays = tsNumDays,
                      IsActive = tsActive,
                      NumGaps = tsNumGaps,
                      TotalGapDays = tsTotalGapDays,
                      Gapiness = tsGapiness,
                      MinimumValue = tsMin,
                      MaximumValue = tsMax,
                      MeanValue = tsMean,
                      StandardDeviation = tsSD,
                      MetaDataHarvestDate = Sys.Date(),
                      stringsAsFactors = F)

  outDF <- rbind(outDF, recDf)
  pbStep(pb, step=i)

}


pbClose(pb)

write.csv(outDF, paste0(rootDir, '/MeataDataHarvest_on_', Sys.Date(), '.csv'), row.names = F)



metaD <- read.csv(paste0(rootDir, '/MeataDataHarvest_on_', Sys.Date(), '.csv'), stringsAsFactors = F)
metaD[mapply(is.infinite, metaD)] <- 'NULL'
metaD[mapply(is.na, metaD)] <- 'NULL'
head(metaD)

dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
#dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"

pb <- pbCreate(nrow(metaD), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1
for (i in 1:nrow(metaD)) {

  id <- df$SiteID[i]

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




