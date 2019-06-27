library(DBI)
library(RSQLite)

dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)


sql <- 'SELECT Sites.Backend, Sites.SiteID, Sites.SiteName, Sensors.SensorID, Sensors.SensorName, Sensors.StartDate, Sensors.EndDate
FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID
WHERE (((Sites.Backend)="MAIT"));'


sql <- "Select SiteID From sites Where Backend='Mait'"
res <- dbSendQuery(con, sql)
df <- dbFetch(res)
df


for (i in 1:nrow(df)) {

  id <- df$SiteID[i]

  sql <- paste0("UPDATE sensors
  SET SensorID = SensorID || '_' || SensorName
  WHERE SiteID = '", id, "';")

  res <- dbSendStatement(con, sql)
  dbGetRowsAffected(res)
}

