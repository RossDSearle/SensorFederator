library(RSQLite)
library(DBI)

dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"

siteid = 'op7777'
sitename='Test Insert Site'
sensorgroup = 'USQ-Outpost' 
backend = 'OutPost' 
access = 'Public' 
usr = 'DFreebairn' 
pwd = 'USQ' 
latitude = '-26.405248' 
longitude = '153.01262' 
owner = 'USQ' 
contact = 'Ross Searle' 
providerurl = 'http://outpostcentral.com/' 
networkinfowebsite = '' 
description = 'This is justa test site - it has no data' 
servername = 'https://www.outpostcentral.com' 

addLocation(SiteName=sitename)
addLocation(SiteID = siteid, SiteName=sitename, SensorGroup=sensorgroup, Backend=backend, Access=access, Usr=usr, Pwd=pwd,Latitude=latitude, Longitude=longitude, Owner=owner, Contact=contact, ProviderURL=providerurl, NetworkInfoWebsite=networkinfowebsite, Description=description, ServerName=servername)
  

addLocation <- function(SiteID, SiteName, SensorGroup, Backend, Access='Public', Usr=NULL, Pwd=NULL,Latitude, Longitude, Owner, Contact=NULL, ProviderURL=NULL, NetworkInfoWebsite=NULL, Description=NULL, ServerName){
  
  con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
  fk_On <- 'PRAGMA foreign_keys = ON;'
  dbExecute(con, fk_On)
  
  sqlqry<- paste0("select * from Sites where SiteID = '", SiteID, "'")
  
  res <- dbSendQuery(con, sqlqry)
  df <- dbFetch(res)
  
  if( nrow(df) == 0){
    
    sqlInsert <- paste0("Insert into Sites (SiteID, SiteName, SensorGroup, Backend, Access, Usr, Pwd,Latitude, Longitude, Owner, Contact, ProviderURL, NetworkInfoWebsite, Description, ServerName ) values ('", SiteID, "','", SiteName, "','", SensorGroup, "','", Backend, "','", Access, "','", Usr, "','",  Pwd, "',", Latitude, ",",Longitude, ",'", Owner, "','",  Contact, "','",  ProviderURL, "','",  NetworkInfoWebsite, "','",  Description, "','",  ServerName, "')")
    
     # rs <- dbSendStatement(con, "Update appUsers SET daysSince = :y, defaultSite = :z where  usr = :x")
    # dbBind(rs, param = list(x = str_to_lower(appAuth$currentUsr), y = defDaysSince, z = defSite))
    # dbGetRowsAffected(rs)
    # dbClearResult(rs)
    # dbDisconnect(conUpdate)
    # 
    # sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE sites.Access = 'Public'"
    res <- dbSendStatement(con, sqlInsert)
    
    dbClearResult(res)
    
   
  }else{
    
    stop(paste0("Site with name '", SiteName, "' already exists"))
    
    
  }
  dbDisconnect(con)
}