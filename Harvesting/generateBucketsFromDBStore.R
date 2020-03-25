library(DBI)
library(RSQLite)
library(stringr)


rootDir <-  'C:/Projects/SensorFederator'
dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/HarvestUtils.R')

conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)


###### Analyse upper and lower soil moisture limits  #########


sql <- "Select * from Sensors where DataType = 'Soil-Moisture' and HasData = 'TRUE'"
sm <- doQuery(conFed, sql)

q1 <- 0.05
q2 <- 0.95

odf <- data.frame(stringsAsFactors = F, row.names = NULL)
for (i in 1:nrow(sm)){
  print(i)
  rec <- sm[i,]
  sqlSenNum <- paste0("select * from Sensors where SiteID='", rec$SiteID, "' and upperDepth = '", rec$UpperDepth,
                      "' and lowerDepth = '", rec$LowerDepth, "' and DataType = 'Soil-Moisture'")
  storeSens <- doQuery(conStore, sqlSenNum)
  if(nrow(storeSens)>0){
    sensorNum <- storeSens$sensorNum[1]

    if(length(sensorNum) > 1){
      print(rec$SiteID)
    }

    d <- getSensorData(conStore, sensorNum)

    tst <- getTidyTS(d)

    tst[tst<1] <- NA
    tst <- na.omit(tst)

    if(nrow(tst)> 10){
    q <- quantile(coredata(tst), c(q1, q2))
    ll <-  format(q[1], digits=5)
    ul <-  format(q[2], digits=5)
    }else{
      ll <- 0
      ul <- 0
    }

    df <- data.frame(rec$SiteID, rec$DataType, rec$UpperDepth, rec$LowerDepth,
                     ll,  ul,  stringsAsFactors = F, row.names = NULL)

    odf <- rbind(odf, df)
  }
}


colnames(odf) <- c('SiteID','DataType', 'UpperDepth', 'LowerDepth',  'LL', 'UL')

dbWriteTable(conFed, "SoilBucket", odf, overwrite = TRUE, row.names = FALSE)



