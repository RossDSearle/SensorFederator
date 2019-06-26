source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')
library(htmltidy)
library(plotly)
library(dygraphs)
# Sensor Cloud



start_time <- Sys.time()
s <- getAuthorisedSensors()
end_time <- Sys.time()
end_time - start_time

s <- getAuthorisedSensors(usr = 'Public', pwd = 'Public')

sensorInfo <- s


locs <- fromJSON('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Soil-Moisture')
nrow(locs)


site <- 'cerdi.sfs.5278.platform'


sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Soil-Moisture', ]
sensors <- sensors[order(sensors$UpperDepth),]

aggregSeconds=timeSteps$day
startDate='09-04-2017'
endDate='11-07-2017'

d <- getSensorData(streams=sensors,aggPeriod = timeSteps$weeks, startDate=startDate, endDate=endDate )
d <- getSensorData(streams=sensors, aggPeriod=timeSteps$quarters , startDate=startDate, endDate=endDate )
head(d)

sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Rainfall', ]
getSensorData(streams=sensors, aggPeriod=timeSteps$week, startDate='09-04-2015', endDate='11-05-2015' )


usrpwd  <- 'ross.searle@csiro.au:rossiscool' # need to pass these inas parameters eventually
x <- 'https://sensor-cloud.io/api/sensor/v2/observations?streamid=cosmoz.site.2.soil_moisture_filtered&limit=1000000&start=2016-02-20T00:00:00.000Z&end=2017-02-22T23:59:59.000Z'
x <- 'https://sensor-cloud.io/api/sensor/v2/observations?streamid=cerdi.sfs.5278.stream.5588.soil-moisture.1000mm&start=2015-04-09T00:00:00.000Z&end=2015-05-11T23:59:59.000Z&limit=1000000'
x <- 'https://sensor-cloud.io/api/sensor/v2/observations?streamid=cosmoz.site.2.soil_moisture_filtered&start=2016-02-22T00:00:00.000Z&end=2018-03-24T23:59:59.000Z&limit=1000000'

x <- 'https://sensor-cloud.io/api/sensor/v2/observations?streamid=cerdi.sfs.5278.stream.5588.soil-moisture.1000mm&start=2015-04-09T00:00:00.000Z&end=2015-05-11T23:59:59.000Z&limit=1000000'
x <- 'https://sensor-cloud.io/api/sensor/v2/observations?streamid=cerdi.sfs.5278.stream.5586.soil-moisture.500mm&start=2015-04-09T09:00:00.000Z&end=2015-05-11T09:00:00.000Z&limit=1000000'

response <- getURL(x, userpwd=usrpwd, httpauth = 1L)
response







#Adcon


usr <- 'csirogrdc'
pwd <- 'grdc'
site <-  '15175'

startDate <- '20140125T10:45:00'
endDate <- '20140126T10:45:00'
res <- adconGetDataDateRange(usr, pwd, 15849, startDate, endDate, deltaSecs )
xml_view(res)


xmlData <- adconGetData(usr=usr, pwd=pwd, nodeID=15283, date='20140125T10:43:44', slots = 5000)
d <- adconGenerateTimeSeries(xmlData, retType = 'df')
head(d)

write(xmlData, file = 'C:/Users/sea084/Dropbox/RossRCode/Git/ProbeAPIs/AdconResponses/sm.xml')
xml_view(xmlData)

sensorInfo <- getAuthorisedSensors()
sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Rainfall', ]
sensors <- sensorInfo[sensorInfo$SiteID == site, ]
sensors <- sensors[order(sensors$UpperDepth) & sensorInfo$SensorGroup == 'RAIN',]
vcd(sensorInfo)

sensors <- sensorInfo[sensorInfo$DataType == 'Rainfall', ]

streams=sensors
backEnd='Adcon'
aggregSeconds=timeSteps$day
startDate='01-01-2016'
endDate='05-01-2016'

startDate='27-05-2017'
endDate='29-05-2017'

sensors <- sensors[1,]
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate)
head(d)
tail(d)





# Outpost

site = 'op12251'
nodeID <- '15849'

sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Soil-Moisture', ]
sensors <- sensors[order(sensors$UpperDepth),]

streams=sensors[1:3,]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2018-01-01T00:00:00'
endDate='2018-01-04T23:59:59'


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'yoliver', '&password=', 'export',
                  '&dateFrom=1/Dec/2017%2000:00:00&dateTo=', '3/Dec/2017%2000:00:00', '&outpostID=', 'op12253', '&inputID=', '245004')
dataXML <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
cat(dataXML, file='c:/temp/outpost.xml')

xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)

# dataXML <- readLines('C:/Temp/outpost1.xml', encoding = 'UTF-8-BOM')
# xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)

doc <- xmlRoot(xmlObj)

d <- getSensorData(streams=streams, aggPeriod=timeSteps$none, startDate='09-04-2016', endDate='11-05-2016' )
head(d)
plot(d)


getSensorInfo(usr = 'Admin', pwd = 'ross')


dygraph(d , main = paste0('Tet'))  #%>%



urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'EPARF',
                  '&dateFrom=1/Dec/2017%2000:00:00&dateTo=', '3/Dec/2017%2000:00:00', '&outpostID=', 'op15376', '&inputID=', '456114')
dataXML <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
cat(dataXML, file='c:/temp/outpost.xml')

xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)




####   Outpost USQ

#usr <- 'David+Freebairn'
usr <- 'DFreebairn'
pwd <- 'USQ'
siteID <- 'op28962'
sensorID <- '694333'

sensors <- sensorInfo[sensorInfo$SiteID == siteID & sensorInfo$DataType == 'Rainfall', ]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2018-01-01T00:00:00'
endDate='2018-01-04T23:59:59'

sDate <- '1/Jan/2019%2000:00:00'
eDate='20/Mar/2019%2000:00:00'


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd,
                  '&dateFrom=', sDate, '&dateTo=', eDate, '&outpostID=', siteID, '&inputID=', sensorID)

response <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
#xml_view(dataXML)
cat(response, file='c:/temp/outpost.xml')

xmlObj=xmlParse(response, useInternalNodes = TRUE)
doc <- xmlRoot(xmlObj)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))


vals <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value", xmlValue, ns))
tail(vals, 10)
sum(vals)
dts <- xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/date", xmlValue, ns)


dfo <- data.frame(dts, vals, stringsAsFactors = F)
tail(dfo)


startDate <- '2019-01-01T00:00:00'
endDate='2019-03-20T23:59:59'

rawv <- getSensorData(streams=streams, aggPeriod=timeSteps$days, startDate=startDate, endDate=endDate )
head(rawv)
plot(rawv)
write.csv(to.DF(rawv), 'c:/temp/outpostRaw.csv')
sum(rawv$Rainfall_0)

daysv <- getSensorData(streams=streams, aggPeriod=timeSteps$days, startDate=startDate, endDate=endDate )
sum(daysv$Rainfall_0)
write.csv(to.DF(daysv), 'c:/temp/outpostDays.csv')
plot(daysv)




dygraph(daysv) %>% dyRangeSelector()





####   Outpost EPARF


usr <- 'EPARF'
pwd <- 'EPARF'
siteID <- 'op30345'
sensorID <- '774289'

sensors <- sensorInfo[sensorInfo$SiteID == siteID & sensorInfo$DataType == 'Rainfall', ]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2018-01-01T00:00:00'
endDate='2018-01-04T23:59:59'

startDate <- '2018-01-01T09:00:00'
endDate='2018-01-04T08:59:59'

urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd,
                  '&dateFrom=1/Jan/2019%2000:00:00&dateTo=', '2/Jan/2019%2000:00:00', '&outpostID=', siteID, '&inputID=', sensorID)
response <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
#xml_view(dataXML)
cat(response, file='c:/temp/outpost.xml')

xmlObj=xmlParse(response, useInternalNodes = TRUE)
doc <- xmlRoot(xmlObj)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))


vals <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value", xmlValue, ns))
tail(vals, 10)
sum(vals)






##### Cosmoz   ##############

x <- 'http://cosmoz.csiro.au/rest/station/21/records?processing_level=4&startdate=2010-01-01T00%3A00%3A00Z&enddate=2019-06-20T00%3A00%3A00Z&count=100000000000&offset=0'

#x <- 'http://cosmoz.csiro.au/rest/station/2/records?processing_level=4&startdate=2017-08-20T09:00:00Z&enddate=2018-08-20T09:00:00Z&property_filter=rainfall&count=1000000&offset=0'

#response <- getURL(x)
#response

forDayData <- fromJSON(x, flatten=TRUE)
df <- forDayData$observations
nrow(df)
head(df,1)
tail(df,1)


sensorInfo[sensorInfo$Backend=='Cosmoz',]
sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Rainfall', ]

sensors <- sensorInfo[sensorInfo$SiteID == 'Cosmoz_2' & sensorInfo$DataType == 'Rainfall' & sensorInfo$Backend=='Cosmoz', ]
sensors <- sensors[order(sensors$UpperDepth),]
vcd(sensors)

streams=sensors
backEnd='Cosmoz'
aggregSeconds=timeSteps$day
numrecs = 100
# startDate='01-01-2016'
# endDate='05-01-2016'

startDate='2015-05-27T09:00:00'
endDate='2017-05-29T19:00:00'


format(1810032000, scientific = FALSE)
print(1810032000)
sensors <- sensors[1,]
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000000)
write.csv(to.DF(d), 'c:/temp/ts.csv')
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=NULL, endDate=NULL, numrecs = 10000)
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000, outFormat='nestedTS')

head(d)
tail(d)

write.csv(to.DF(dts), 'c:/temp/tsTest.csv')
write.csv(to.DF(outts), 'c:/temp/outtsTest.csv')



#####   DAFWA   ###########################

rootDir <- sensorRootDir

urlClimate <- paste0('https://api.agric.wa.gov.au/v1/weatherstations/dailysummary.json?station_code=', 'BR', '&fromDate=2010-01-01&toDate=2016-09-29&api_key=CCB3F85A64008C6AC1789E4F.apikey')
climate <- getURL(urlClimate, .opts = myOpts)
climateData <- fromJSON(climate)[[1]]

yrs <- seq.Date(as.Date('2010-01-01'), Sys.Date(), by='year')
toNow <- c(yrs, Sys.Date())


outDFD <- data.frame(stringsAsFactors = F)
for (i in 1:(length(toNow)-1)) {

  print(i)
  urlClimate <- paste0('https://api.agric.wa.gov.au/v1/weatherstations/dailysummary.json?station_code=', 'BR', '&fromDate=', toNow[i], '&toDate=', toNow[i+1], '&api_key=CCB3F85A64008C6AC1789E4F.apikey')
  climate <- getURL(urlClimate, .opts = myOpts)
  climateData <- fromJSON(climate)[[1]]
  outDFD <- rbind(outDFD, climateData)
}

tail(outDFD)
seq.Date(as.Date('2010-01-01'), as.Date('2016-09-29'), by='year')

site <- 'DAFWA_BR'
site <- 'DAFWA_BJ'

sensorInfo[sensorInfo$Backend=='DAFWA',]
sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Rainfall', ]

sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Rainfall' & sensorInfo$Backend=='DAFWA', ]
sensors <- sensors[order(sensors$UpperDepth),]
vcd(sensors)


streams=sensors
backEnd='DAFWA'
aggregSeconds=timeSteps$day
numrecs = 10000000
# startDate='01-01-2016'
# endDate='05-01-2016'

startDate <- '2010-01-01T00:00:00'
endDate <- '2019-06-04T23:00:00'

sensors <- sensors[1,]
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000000)
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$weeks , startDate=startDate, endDate=endDate, numrecs = 10000000)

head(d)
tail(d)



######   Netatmo


ClientId <- '5b5aa99b11349f54f18bec78'

ClientSecret <- 'Sao6iqGgn12K0x36DfWN4o3XEwwkPDUBjP'




paste0('https://api.netatmo.com/oauth2/authorize?client_id=', ClientId, '&redirect_uri=[YOUR_REDIRECT_URI]&scope=[SCOPE_DOT_SEPARATED]&state=', 'Bob')


url="https://api.netatmo.com/oauth2/authorize?client_id=$app_id&scope=read_station&state=Bob"


library(httr)
myapp   <- oauth_app("SensorFederator",key="5b5aa99b11349f54f18bec78",secret="Sao6iqGgn12K0x36DfWN4o3XEwwkPDUBjP")
ep      <- oauth_endpoint(authorize = "http://api.netatmo.com/oauth2/authorize",access = "http://api.netatmo.net/oauth2/token")
sig_tok <- oauth2.0_token(ep,myapp, scope="read_station", NULL)
sig     <- config(token = sig_tok)




dataJsn <- getURL(url)


YOUR_LAT_NE <- -27
YOUR_LON_NE <- 153
YOUR_LAT_SW <- -26
YOUR_LON_SW <- 152


url <- paste0('http:/api.netatmo.com/api/getpublicdata?access_token=__TECHNICAL_GETPUBLICDATA_ENTRY_TABLE_VALUE_3_1&lat_ne=-27&lon_ne=153&lat_sw=-26&lon_sw=152&filter=FALSE')


paste0('https://api.netatmo.com/oauth2/authorize?xlient_id=', ClientId, '&scope=read_station,&state=')



library(httr)
params <- c(grant_type='password',client_id=ClientId, client_secret=ClientSecret, username='ross.searle@gmail.com', password='Rossiscool1!', scope='read_station')

url <- "https://api.netatmo.com/oauth2/token"
#body <- c("SensorFederator",key="5b5aa99b11349f54f18bec78",secret="Sao6iqGgn12K0x36DfWN4o3XEwwkPDUBjP")

# Form encoded
POST(url, body = params, encode = "form", write_disk("c:/temp/a.txt", overwrite=T))
readtext::readtext("c:/temp/a.txt")



url <- "https://api.netatmo.com/oauth2/token?grant_type=password,client_id=5b5aa99b11349f54f18bec78&client_secret=Sao6iqGgn12K0x36DfWN4o3XEwwkPDUBjP&username=ross.searle@gmail.com&password=Rossiscool1!&scope=read_station"

url <- "https://api.netatmo.com/oauth2/token?grant_type=password,client_id=5b5aa99b11349f54f18bec78&client_secret=Sao6iqGgn12K0x36DfWN4o3XEwwkPDUBjP&username=ross.searle@gmail.com&password=Rossiscool1!&scope=read_station"


getURL('https://api.netatmo.com/oauth2/authorize?client_id=5b5aa99b11349f54f18bec78&scope=read_station&state=abcdefghi')





########    MAIT   ###########


siteid <- 'VicAg_Bangerang'
sensortype <- 'Soil-Moisture'

startDate <- '2018-01-01T00:00:00'
endDate <- '2018-01-04T23:00:00'

sensorInfo <- getAuthorisedSensors()
sensors <- sensorInfo[sensorInfo$SiteID == siteid & sensorInfo$DataType == 'Soil-Moisture' & sensorInfo$Backend=='Mait', ]
sensors <- sensors[order(sensors$UpperDepth),]
sensors <- sensors[1:3,]

d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000000)
head(d)
plot(d)
write.csv(to.DF(d), 'c:/temp/ts.csv')

sensors <- sensorInfo[sensorInfo$SiteID == 'VicAg_Speed' & sensorInfo$DataType == 'Rainfall' & sensorInfo$Backend=='Mait', ]
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$days , startDate=startDate, endDate=endDate, numrecs = 10000000)
head(d)
plot(d)





####  Mait USyd

siteid <- 'Usyd_Dinyah'
sensortype <- 'Soil-Moisture'

startDate <- '2018-03-01T00:00:00'
endDate <- '2018-03-04T23:00:00'

network = '1107' # USyd



url <- paste0('http://intelliweb.mait.com.au/getdata?network=', network)
stations <- getURL(url, userpwd=paste0( "dpigetdata:dpigetdata"))
modules <- read.csv(text=stations, skip=1)
modules
url <- 'http://intelliweb.mait.com.au/getdata?network=1107&module=10&startdate=2018/03/01&enddate=2018/03/04'
dcsv <- getURL(url, userpwd=paste0( "dpigetdata:dpigetdata"))


sensorInfo <- getAuthorisedSensors()
sensors <- sensorInfo[sensorInfo$SiteID == siteid & sensorInfo$DataType == 'Soil-Moisture' & sensorInfo$Backend=='Mait', ]
sensors <- sensors[order(sensors$UpperDepth),]
sensors <- sensors[1:3,]

d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000000)
head(d)
plot(d)
write.csv(to.DF(d), 'c:/temp/ts.csv')


########    DataFarmer   ###########

startDate <- '2018-01-04T00:00:00'
endDate <- '2018-01-10T23:00:00'

sensors <- sensorInfo[sensorInfo$SiteID == 'BCG_Banyena' & sensorInfo$Backend=='DataFarmer' & sensorInfo$DataType == 'Temperature', ]
d <- getSensorData(streams=sensors,  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000000)
head(d)
plot(d)
write.csv(to.DF(d), 'c:/temp/ts.csv')






###############    SenFedDataStore   ########################################



dtype <- 'Soil-Moisture'
sd <- '2001-12-01T00:00:00'
ed <- '2002-12-29T04:00:00'
site <-'m1'


sensorInfo <- getAuthorisedSensors()
#sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Rainfall', ]
sensors <- sensorInfo[sensorInfo$SiteID == site & sensorInfo$DataType == 'Soil-Moisture', ]
#sensors <- sensorInfo[sensorInfo$SiteID == site, ]
streams <- sensors


getData_SenFedStore(sid=site, datatype = dtype, sdate = sd, edate = ed )

site <- 'OzNet_m1'
d <- getSensorData_SenFedStore(streams = sensors, startDate = sd, endDate = ed)
tail(d[[1]])












getSensorLocations(latitude=-27.5347, longitude=151.9316, radius_km=1000.0, usr='SoilWaterApp',pwd='rLR4ArUomkODEpAgaDae4Ak')
SoilWaterApp&pwd=rLR4ArUomkODEpAgaDae4Ak



https://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?latitude=-27.5347&longitude=151.9316&radius_km=1000.0&sensortype=Rainfall&usr=SoilWaterApp&pwd=rLR4ArUomkODEpAgaDae4Ak

