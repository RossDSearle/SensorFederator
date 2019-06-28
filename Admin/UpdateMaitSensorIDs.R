library(DBI)
library(RSQLite)

dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)



sql <- 'CREATE TABLE SensorsV3 AS SELECT * FROM Sensors;'
res <- dbSendStatement(con, sql)
dbGetRowsAffected(res)

sql <- "Select SiteID From sites Where Backend='Mait'"
res <- dbSendQuery(con, sql)
df <- dbFetch(res)
df


for (i in 1:nrow(df)) {
  print(i)
  id <- df$SiteID[i]

  sql <- paste0("UPDATE sensors
  SET SensorID = SensorID || '_' || SensorName
  WHERE SiteID = '", id, "';")

  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
}

