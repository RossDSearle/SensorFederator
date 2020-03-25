library(DBI)
library(RSQLite)
library(stringr)


rootDir <-  'C:/Projects/SensorFederator'
dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/TSUtils.R')

conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)


sql <- "Select * from sites where SensorGroup = 'EPARF'"
s <- doQuery(conFed, sql)

for (i in 1:nrow(s)) {

  rec<-s[i, ]
  sql <- paste0("DELETE from Sensors where SiteID = '", rec$SiteID, "'")
  r <- sendStatement(conStore, sql)
  print(r)
}

