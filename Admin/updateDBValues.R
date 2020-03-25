library(DBI)
library(RSQLite)

dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
#dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"

sqlUp <- paste0("UPDATE sensors SET Calibrated = 'Not Applicable'")
res <- dbSendStatement(con, sqlUp)
dbGetRowsAffected(res)
dbClearResult(res)


grp <- 'Cosmoz'
grp <- '1107'
grp <- '1099'
grp <- 'OzNet'
grp <- 'SFS'
grp <- 'Booroowa'
grp <- 'EPARF'
grp <- 'VicAg2_1345'
grp <- 'VicAg2_1070'
grp <- 'GRDCWASoilWaterProbes'

sql <- paste0("select * from sites where SensorGroup = '", grp, "'")
res <- dbSendQuery(con, sql)
sites <- dbFetch(res)
dbClearResult(res)

for (i in 1:nrow(sites)) {

  sql <- paste0("UPDATE sensors SET UpperDepth = (UpperDepth * 10) WHERE SiteID = '", sites$SiteID[i], "' and UpperDepth <> 0")
  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

  sql <- paste0("UPDATE sensors SET LowerDepth = (LowerDepth * 10) WHERE SiteID = '", sites$SiteID[i], "' and LowerDepth <> 0")
  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)
}


pb <- pbCreate(nrow(sites), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1
for (i in 1:nrow(sites)) {

  sql <- paste0("UPDATE sensors SET Calibrated = 'Yes' WHERE SiteID = '", sites$SiteID[i], "' and DataType = 'Soil-Moisture';")

  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)
  pbStep(pb, step=i)
}


sql <- paste0("select * from sensors WHERE SiteID = '", sites$SiteID[i], "' and DataType = 'Soil-Moisture';")


#### Multiply depths by 10 in the SensorFederator DB

pb <- pbCreate(nrow(sites), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1
for (i in 3:nrow(sites)) {
  print(i)
  sql <- paste0("UPDATE sensors SET UpperDepth = (UpperDepth * 10) WHERE SiteID = '", sites$SiteID[i], "' and UpperDepth <> 0")
  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

  sql <- paste0("UPDATE sensors SET LowerDepth = (LowerDepth * 10) WHERE SiteID = '", sites$SiteID[i], "' and LowerDepth <> 0")
  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

 # pbStep(pb, step=i)
}



#### Multiply depths by 10 in the Datastore

dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)

sql <- paste0("select * from Sensors where SiteID like 'OzNet%'")
res <- dbSendQuery(conStore, sql)
sites <- dbFetch(res)
dbClearResult(res)


for (i in 2:nrow(sites)) {
  print(i)
  sql <- paste0("UPDATE Sensors SET upperDepth = (upperDepth * 10) WHERE SiteID = '", sites$SiteID[i], "' and upperDepth <> 0")
  res <- dbSendStatement(conStore, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

  sql <- paste0("UPDATE Sensors SET lowerDepth = (lowerDepth * 10) WHERE SiteID = '", sites$SiteID[i], "' and lowerDepth <> 0")
  res <- dbSendStatement(conStore, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

}


for (i in 2:nrow(sites)) {
  print(i)
  sql <- paste0("UPDATE Sensors SET upperDepth = (upperDepth / 10000000) WHERE SiteID = '", sites$SiteID[i], "' and upperDepth <> 0")
  res <- dbSendStatement(conStore, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

  sql <- paste0("UPDATE Sensors SET lowerDepth = (lowerDepth/ 10000000) WHERE SiteID = '", sites$SiteID[i], "' and lowerDepth <> 0")
  res <- dbSendStatement(conStore, sql)
  dbGetRowsAffected(res)
  dbClearResult(res)

}
