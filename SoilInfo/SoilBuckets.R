

getBucket <- function(SiteID){

  conFed <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
  res <- dbSendQuery(conFed, paste0("Select * from SoilBucket where SiteID = '", SiteID, "' ORDER BY LowerDepth" ))
  rows <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(conFed)

  return(rows)

}