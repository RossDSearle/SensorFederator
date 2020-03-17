library(htmltidy)
library(plotly)
library(dygraphs)
library(httr)
library(stringr)
library(DBI)
library(RSQLite)
library(jsonlite)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')

sensorInfo <- getAuthorisedSensors()




########  WA   ##################################

usr <- 'yoliver'
pwd <- 'export'
siteID <- 'op36086'
sensorID <- '456113'

sensors <- sensorInfo[sensorInfo$SiteID == siteID & sensorInfo$DataType == 'Rainfall', ]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2019-01-01T00:00:00'
endDate='2019-01-04T23:59:59'

providerInfo = list( provider= c('GRDCWASoilWaterProbes'), backEnd=c('Outpost2'), server=c('https://www.outpostcentral.com'), org=c('GRDC'),
                     usr=c('yoliver'), pwd=c('export'),
                     access = c('Public'),
                     contact=c('Yvette.Oliver@csiro.au'), orgURL=c('https://grdc.com.au/'),
                     Description=c('There are 25 soil water probes installed over 2013 to 2016 for GRDC projects which are now managed by the GRDC DAW002 subsoil constraints project') )



# urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd,
#                   '&dateFrom=1/Jan/2019%2000:00:00&dateTo=', '2/Jan/2019%2000:00:00', '&outpostID=', siteID, '&inputID=', sensorID)

urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd)
response <- GET(urlData)
stop_for_status(response) # stop if the response is an error
sensorData <- content(response, as="text", encoding = 'UTF-8')
sensorData

cat(sensorData, file='c:/temp/outpost.xml')

xmlObj=xmlParse(sensorData, useInternalNodes = TRUE)
doc <- xmlRoot(xmlObj)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))


#vals <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value", xmlValue, ns))
siteNames <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)
siteIDs <- xpathSApply(doc ,"//opdata:sites/opdata:site/id", xmlValue, ns)
lats <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/latitude", xmlValue, ns))
lons <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/longitude", xmlValue, ns))

locs <- data.frame(paste0('opSID_', siteIDs), siteNames, providerInfo$provider, providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, lats, lons,  providerInfo$org, providerInfo$contact, providerInfo$orgURL, '', providerInfo$Description, providerInfo$server, stringsAsFactors = F)
colnames(locs) <- c('SiteID', 'SiteName', 'SensorGroup', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Owner', 'Contact', 'ProviderURL', 'NetworkInfoWebsite', 'Description', 'ServerName')

write.csv(locs, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/WA/WA_GRDC_Sites.csv')


sensorDF <- getEmptySensorDF()

for(i in 1:nrow(locs)){
  print(i)
  streams <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", locs$SiteName[i],"']/parent::opdata:site/opdata:inputs/opdata:input/name"), xmlValue, ns)
  streamID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/name[text()='", locs$SiteName[i],"']/parent::opdata:site/opdata:inputs/opdata:input/id"), xmlValue, ns)

  if(!is.null(streams))
  {
      df <- data.frame(SiteID = siteIDs[i], SensorID = streamID,
                       SensorName = streams, StartDate = NA, EndDate = NA, DataType = NA,
                       UpperDepth = NA, LowerDepth = NA, Calibrated = FALSE, Units = NA,
                       TotalDays  = NA, IsActive = TRUE, NumGaps = NA, TotalGapDays = NA, Gapiness = NA,
                       MinimumValue = NA, MaximumValue = NA, MeanValue = NA, StandardDeviation = NA, HarvestDate = NA,
                       stringsAsFactors = F )

      sensorDF <- rbind(sensorDF, df)
  }
}

write.csv(sensorDF, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/WA/WA_GRDC_Sensors.csv')
### this file edited to remove obvious rubbish

d <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/WA/WA_GRDC_SensorsManuallyEdited.csv', stringsAsFactors = F)
write.csv(d, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/WA/WA_GRDC_SensorsManuallyEdited.csv', row.names = F)


ids <- which(grepl(paste0('Soil Moisture '), d$sensorName, ignore.case = T))
d[ids, ]
d$Units[ids] <- 'Percent'
d$DataType[ids] <-  'Soil-Moisture'

d$SiteID <-  paste0('opSID_', d$SiteID)

depths <- seq(10, 100, 10)
for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('Soil Moisture ', depths[i], ' cm'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'Soil-Moisture'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Ynits <- 'Percent'
}

depths <- c(10, 25, 35, 45, 55, 65, 75, 85, 95)
for (i in 1:length(depths)) {
  ids <- which(grepl(paste0(depths[i], 'cm PAW'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'PAW'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Units <- 'mm'
}

depths <- seq(10, 100, 10)
for (i in 1:length(depths)) {
print(ids)
  ids <- which(grepl(paste0('EnvPro_Moisture_', depths[i], 'cm'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'Soil-Moisture'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'Percent'
}


depths <- seq(10, 100, 10)
for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('EnvPro_Salinity_', depths[i], 'cm'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'Salinity'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'dS/cm'
}



depths <- seq(10, 100, 10)
for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('EnvPro_Temp_', depths[i], 'cm'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'Soil-Temperature'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'Degrees-Celcius'
}

ids <- which(grepl(paste0('Rain'), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Rainfall'
d$Units[ids] <- 'mm'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0

ids <- which(grepl(paste0('Humidity'), d$sensorName, ignore.case = T))

d$DataType[ids] <- 'Humidity'
d$Units[ids] <- 'Percent'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0


ids <- which(grepl(paste0("TBSHT01_Temp"), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Temperature'
d$Units[ids] <- 'Degrees-Celcius'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0

ids <- which(grepl(paste0("Wind Speed km/h"), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Wind-Speed'
d$Units[ids] <- 'km/hr'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0

ids <- which(grepl(paste0("Wind Dir"), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Wind-Direction'
d$Units[ids] <- 'Degrees'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0

ids <- which(grepl(paste0("Pressure hPA"), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Atmospheric-Pressure'
d$Units[ids] <- 'hPa'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0

ids <- which(grepl(paste0("Temp °C"), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Temperature'
d$Units[ids] <- 'Degrees-Celcius'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0

ids <- which(grepl(paste0("Rel Humidity %"), d$sensorName, ignore.case = T))
d$DataType[ids] <- 'Humidity'
d$Units[ids] <- 'Percent'
d$UpperDepth[ids] <- 0
d$LowerDepth[ids] <- 0




depths <- seq(10, 80, 10)
for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('Salinity ', depths[i], ' cm'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'Salinity'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'dS/cm'
}

depths <- seq(10, 80, 10)
for (i in 1:length(depths)) {
  ids <- which(grepl(paste0('Temperature ', depths[i], ' cm'), d$sensorName, ignore.case = T))
  d$DataType[ids] <- 'Temperature'
  d$UpperDepth[ids] <- depths[i]
  d$LowerDepth[ids] <- depths[i]
  d$Units[ids] <- 'Degrees-Celcius'
}




ids <- which(grepl(paste0("Soil-Moisture"), d$DataType, ignore.case = T))
d$Units[ids] <- 'Percent'




WASites <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/WA/WA_GRDC_Sites.csv', stringsAsFactors = F)
WASensors <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/WA/WA_GRDC_SensorsManuallyEdited.csv', stringsAsFactors = F)

DBCon <- dbConnect(RSQLite::SQLite(), 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite', flags = SQLITE_RW)
dbWriteTable(DBCon, "sites", WASites, append=T)
dbWriteTable(DBCon, "sensors", WASensors, append=T)

dbDisconnect(DBCon)


unique(WASensors$DataType)



for(i in 1:nrow(WASensors)){

  rec <- WASensors[i,]
  print(rec$SiteID)
  print(rec$DataType)
  dbWriteTable(DBCon, "sensors", rec, append=T)


}

sql <- "SELECT * FROM Sites INNER JOIN Sensors ON Sites.SiteID = Sensors.SiteID WHERE sites.Access = 'Public' and Sites.SensorGroup = 'GRDCWASoilWaterProbes'"
res <- dbSendQuery(DBCon, sql)
sensors <- dbFetch(res)
dbClearResult(res)
sensors
write.csv(sensors, 'c:/temp/wa.csv')





for(i in 1:nrow(WASites)){

  rec <- WASites[i,]
  print(rec$SiteID)
  url <- paste0("http://127.0.0.1:6740/SensorAPI/getSensorDataStreams?siteid=",rec$SiteID , "&sensortype=Soil-Moisture&aggperiod=days&startdate=2019-03-01T00:00:00&enddate=2020-03-02T23:59:59")
  resp <- GET(url)
  http_error(resp)

  response <- content(resp, "text", encoding = 'UTF-8')

  if(!grepl('error', response)){
  df <- convertJSONtoDF(response)
  print(head(df))
  }else{
    print(paste0('Problem with - ', rec$SiteID))
  }

}


source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')
s <- getAuthorisedSensors()

sens <- s[s$SiteID == 'opSID_15184' & s$DataType=='Soil-Moisture', ]

getSensorData(sens[4, ])
getSensorData(sens)
getSensorData(streams=sens[4, ] , startDate = '2020-03-01T09:00:00', endDate = '2019-03-03T00:00:00', aggPeriod=timeSteps$days, numrecs=maxRecs, outFormat='simpleTS')





