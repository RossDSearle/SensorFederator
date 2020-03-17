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

grp <- 'GRDCWASoilWaterProbes'

sql <- paste0("select * from sites where SensorGroup = '", grp, "'")
res <- dbSendQuery(con, sql)
sites <- dbFetch(res)
dbClearResult(res)

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


