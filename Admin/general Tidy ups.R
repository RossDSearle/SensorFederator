dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
sql1 <- "UPDATE sensors SET LowerDepth = 0 WHERE LowerDepth IS NULL;"
res <- dbSendStatement(con, sql1)
dbGetRowsAffected(res)


sql <- "SELECT * FROM sensors where UpperDepth IS NULL"
res <- dbSendQuery(con, sql)
df <- dbFetch(res)
df
