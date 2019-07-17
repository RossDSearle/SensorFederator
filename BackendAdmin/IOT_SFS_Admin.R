
rootDir = 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'


providerInfo = list( provider= c('SFS'), backEnd=c('IOT_CERDI'), server=c('https://services.cerdi.edu.au/sfs/v1.0'), org=c('Southern Farming Systems'),
                     usr=c('Ross.Searle@csiro.au'), pwd=c('uT8tGtyZSUqL'),
                     access = c('Restricted'),
                     contact=c('s.limmer@federation.edu.au'), orgURL=c('http://www.sfs.org.au/ProbeTrax_MoistureProbeNetwork'),
                     Description=c('A private soil moisture network maintained by Southern Farming Systems in Southern Victoria') )




generateSiteInfo_IOT_SFS(providerInfo = providerInfo, rootDir = sensorRootDir)

alldfName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_AllInfo.csv')
allDf <- read.csv(alldfName, stringsAsFactors = F)

sites <- unique(allDf$SiteName)

siteDF <- data.frame()

for(site in sites){
  print(site)

  rec <- allDf[allDf$SiteName == site, ]

  sid <- rec$SiteID[1]
  #####  The way I was fetching the coords in the IOT_Backend.r generateSiteInfo_IOT_SFS function was wrong. This is a fix
  locUrl <- paste0('https://services.cerdi.edu.au/sfs/v1.0/Locations(', sid, ')')
  locResp <- GET(locUrl,  authenticate(usr, pwd))
  locResponse <- content(locResp, "text")
  locDf <- fromJSON(locResponse)
  locs <- locDf$location$geometry$coordinates
  lon <- locs[1]
  lat <- locs[2]

  recDF <- data.frame(SiteID=rec$SiteID[1], SiteName=rec$SiteName[1], SensorGroup=providerInfo$provider, Backend=providerInfo$backEnd,
                       Access=providerInfo$access,Usr=providerInfo$usr,Pwd=providerInfo$pwd,Latitude=lat,Longitude=lon,
                       Owner=providerInfo$org,Contact=providerInfo$contact,ProviderURL=providerInfo$orgURL,NetworkInfoWebsite=providerInfo$orgURL,
                       Description=providerInfo$Description,ServerName=providerInfo$server
                       )
  siteDF <- rbind(siteDF, recDF)
  Sys.sleep(3)
}

outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')

write.csv(siteDF, outName, row.names = F, quote = F)

#### Squirt to DB

dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
dbWriteTable(con, 'sites', siteDF, append = T)
dbDisconnect(con)



sensorDF <- data.frame()

sensdf <- allDf
sensdf <- sensdf[!sensdf$UpperDepth == 'Summed300-1000', ]  # delete these records
sensdf <- sensdf[!sensdf$DataType == 'Battery Voltage', ]  # delete these records
sensdf <- sensdf[!sensdf$DataType == 'WindSpeed Gust', ]  # delete these records
sensdf$DataType[sensdf$UpperDepth !=0 & sensdf$DataType == 'Temperature'] <- "Soil-Temperature" # fix soil temperature records
sensdf$DataType[sensdf$DataType == 'Rain'] <- "Rainfall"
sensdf$DataType[sensdf$DataType == 'Soil Moisture'] <- 'Soil-Moisture'
sensdf$DataType[sensdf$DataType == 'WindSpeed'] <- 'Wind-Speed'
sensdf$DataType[sensdf$DataType == 'Relative Humidity'] <- 'Humidity'


unique(sensdf$DataType)
units <- c('Soil-Moisture'='mm', Rainfall='mm' , 'Soil-Temperature'='Degrees-Celcius', Temperature='Degrees-Celcius','Wind-Speed'='km/hr',Humidity='Percent','Atmospheric-Pressure'='hPa')

outSenDf <- data.frame(SiteID=sensdf$SiteID, Active=1, SensorID=sensdf$SensorID, SensorName=paste0(sensdf$SiteName, '_', sensdf$DataType, '_', sensdf$UpperDepth),
                       StartDate=sensdf$StarDate,EndDate=sensdf$EndDate, DataType=sensdf$DataType, UpperDepth=sensdf$UpperDepth, LowerDepth=sensdf$LowerDepth,
                       Calibrated = 'TRUE', Units=as.character(units[sensdf$DataType], IsActive='TRUE')
)

outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sensors.csv')
write.csv(outSenDf, outName, row.names = F, quote = F)

dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
dbWriteTable(con, 'Sensors', outSenDf, append = T)
dbDisconnect(con)














