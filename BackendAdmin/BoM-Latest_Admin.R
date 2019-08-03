library(RCurl)
library(jsonlite)
library(httr)
library(stringr)
library(xml2)
library(DBI)
library(RSQLite)

rootDir = 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'
rootDir = '/srv/plumber/SensorFederator'
dbPath <- paste0(rootDir, "/DB/SensorFederator.sqlite")


stns <- read.csv('C:/Projects/GIS/National/BOM/stations.csv', stringsAsFactors = F)
sc <- stns[stns$WMO!='',]




urls <- c('http://www.bom.gov.au/nsw/observations/nswall.shtml',
'http://www.bom.gov.au/qld/observations/qldall.shtml',
'http://www.bom.gov.au/tas/observations/tasall.shtml',
'http://www.bom.gov.au/vic/observations/vicall.shtml',
'http://www.bom.gov.au/sa/observations/saall.shtml',
'http://www.bom.gov.au/wa/observations/waall.shtml',
'http://www.bom.gov.au/nt/observations/ntall.shtml')


outDF <- data.frame(stringsAsFactors = F)

for (i in 1:length(urls)) {

    url <- urls[i]

    resp <- GET(url)
    webPage <-  content(resp, "text")

    scraping<- read_html(url)
    nds <- html_nodes(scraping, xpath = '//body//div//div//div//div//div//table//tbody//tr//a')

    ourls <- xml_attr(nds, "href")
    names <- xml_text(nds)

    bits <- str_split(basename(ourls), '[.]')
    ids <- sapply(bits, function (x) x[1])
    wons <- sapply(bits, function (x) x[2])

    df <- data.frame(names, ourls, wons, ids, stringsAsFactors = F)
    outDF <- rbind(outDF, df)

    print(names)

}

nrow(outDF)
write.csv(outDF, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/BOM/bomLatestStations.csv')

lats <- numeric(nrow(outDF))
lons <- numeric(nrow(outDF))
sDate <- numeric(nrow(outDF))

for (i in 1:nrow(outDF)) {

  id <- outDF$wons[i]
  rec <- stns[stns$WMO == id,]

  if(nrow(rec) > 0){
  lats[i] <- rec$Lat
  lons[i] <- rec$Lon
  sDate[i] <- rec$Start
  }
}

allDF <- data.frame(outDF, lons, lats, sDate)

write.csv(allDF, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/BOM/bomLatestStations_ALLInfo.csv')

providerInfo = list( provider= c('BoM'), backEnd=c('BoM_Latest'), server=c('http://www.bom.gov.au/products'), org=c('Bureau of Meteorology'),
                     usr=c(''), pwd=c(''),
                     access = c('Public'),
                     contact=c('ross.searle@csiro.au'), orgURL=c('http://www.bom.gov.au'),
                     Description=c('Latest 72 hours weather observations for each station (30 min intervals).') )



allDF <- allDF[allDF$lons != 0,]

siteDF <- data.frame()
for (i in 1:nrow(allDF)) {

  rec <- allDF[i,]
  recDF <- data.frame(SiteID=paste0('BoMLatest_',rec$wons, '_', rec$ids), SiteName=rec$names, SensorGroup=providerInfo$provider, Backend=providerInfo$backEnd,
                      Access=providerInfo$access,Usr=providerInfo$usr,Pwd=providerInfo$pwd,Latitude=rec$lats,Longitude=rec$lons,
                      Owner=providerInfo$org,Contact=providerInfo$contact,ProviderURL=providerInfo$orgURL,NetworkInfoWebsite=providerInfo$orgURL,
                      Description=providerInfo$Description,ServerName=providerInfo$server, stringsAsFactors = F
  )
  siteDF <- rbind(siteDF, recDF)
}


outName <- paste0(rootDir, '/SensorInfo/', providerInfo$backEnd, '_Sites.csv')
which(duplicated(siteDF))
siteDF[duplicated(siteDF$SiteID),]
siteDFNoDups <- siteDF[!duplicated(siteDF$SiteID), ]
write.csv(siteDFNoDups, outName, row.names = F, quote = F)

#### Squirt to DB

inName <- paste0(rootDir, '/SensorInfo/', providerInfo$backEnd, '_Sites.csv')
siteDF <- read.csv(inName, stringsAsFactors = F)
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
dbWriteTable(con, 'sites', siteDF, append = T)
dbDisconnect(con)


ops <- c('air_temp', 'press','rain_trace','rel_hum','wind_spd_kmh')
dTypes <- c('air_temp'='Temperature', 'press'='Atmospheric-Pressure','rain_trace'='Rainfall','rel_hum'='Humidity','wind_spd_kmh'='Wind-Speed' )
units <- c( rain_trace='mm' , air_temp='Degrees-Celcius','wind_spd_kmh'='km/hr',rel_hum='Percent',press='hPa')


sensDF <- data.frame(stringsAsFactors = F)
for (i in 1:nrow(allDF)) {
  rec <- allDF[i,]

  for (j in 1:length(ops)) {

    outSenDf <- data.frame( SiteID=paste0('BoMLatest_' , rec$wons, '_', rec$ids), Active=1, SensorID=ops[j], SensorName=paste0( rec$wons, '_', dTypes[j]),
                            StartDate=paste0(rec$sDate, '-01-01T00:00:00'),EndDate='2019-07-23T00:00:00', DataType=dTypes[j], UpperDepth=0, LowerDepth=0,
                            Calibrated = 'TRUE', Units=as.character( units[ops[j]] ), IsActive='TRUE' )

    sensDF <- rbind(sensDF, outSenDf)
  }
}


  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$backEnd, '_Sensors.csv')
  write.csv(sensDF, outName, row.names = F, quote = F)

  inName <- paste0(rootDir, '/SensorInfo/', providerInfo$backEnd, '_Sensors.csv')
  outSenDf <- read.csv(inName, stringsAsFactors = F)
  con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
  dbWriteTable(con, 'Sensors', outSenDf, append = T)
  dbDisconnect(con)






