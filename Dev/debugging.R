
library(jsonlite)
library(jsonview)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')


end_time <- Sys.time()
end_time - start_time
s <- getAuthorisedSensors(usr = 'Public', pwd = 'Public')
#s <- getAuthorisedSensors(usr = 'ross.searle@csiro.au', pwd = 'S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL')
sensorInfo <- s

DF <- getSensorLocations(usr = 'Public', pwd = 'Public')


######## different date queries

locs <- fromJSON('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensortype=Rainfall')
nrow(locs)


dfall <- fromJSON('https://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=Cosmoz_21&sensortype=Rainfall&aggperiod=days&usr=SoilWaterApp&pwd=rLR4ArUomkODEpAgaDae4Ak&startdate=2018-09-11T09:00:00&enddate=2019-11-25T08:59:59')
dfall$DataStream

dfdts <- fromJSON('https://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=Cosmoz_21&sensortype=Rainfall&startdate=2018-09-11T09:00:00&aggperiod=days&enddate=2019-11-25T08:59:59&usr=SoilWaterApp&pwd=rLR4ArUomkODEpAgaDae4Ak')
dfdts$DataStream




#### Outpost issues
dfOP1 <- fromJSON('https://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=op31917&sensortype=Rainfall&startdate=2019-11-26T09:00:00&aggperiod=days&enddate=2019-12-04T08:59:59&usr=SoilWaterApp&pwd=rLR4ArUomkODEpAgaDae4Ak')
dfOP1
dfOP2 <- fromJSON('https://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=op31917&sensortype=Rainfall&startdate=2019-10-01T09:00:00&aggperiod=days&enddate=2019-12-04T08:59:59&usr=SoilWaterApp&pwd=rLR4ArUomkODEpAgaDae4Ak')


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'DFreebairn', '&password=', 'USQ',
                  '&dateFrom=26/Nov/2019%2000:00:00&dateTo=', '04/12/2019%2000:00:00', '&outpostID=', 'op31917', '&inputID=', '808521')
dataXML <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
cat(dataXML, file='c:/temp/outpost.xml')

xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)
doc <- xmlRoot(xmlObj)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
vals <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value", xmlValue, ns))
tail(vals, 10)
sum(vals)
dts <- xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/date", xmlValue, ns)
dfo <- data.frame(dts, vals, stringsAsFactors = F)



