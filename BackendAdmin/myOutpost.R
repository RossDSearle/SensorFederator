library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(XML)
library(xml2)

myOpts <- curlOptions(connecttimeout = 200, ssl.verifypeer = FALSE)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')

usr <- 'DFreebairn'
pwd <- 'USQ'

urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd)

response <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
cat(response, file='c:/temp/outpost.xml')


# Burrowa Sr
siteID <- 'op28956'
sensorID <- '694237'

# Tarranlea
siteID <- 'op45521'
#sensorID <- '1434379'
sensorID <- '715359'

# Daves Wilston
siteID <- 'op42563'
#sensorID <- '1434379'
sensorID <- '725458'



sDate <- '1/Jul/2019%2000:00:00'
eDate='16/Jul/2019%2000:00:00'


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd,
                  '&dateFrom=', sDate, '&dateTo=', eDate, '&outpostID=', siteID, '&inputID=', sensorID)

# urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd,
#                  '&outpostID=', siteID, '&inputID=', sensorID)

response <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
response
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
tail(dfo, 500)


sensorInfo <- getAuthorisedSensors(usr = 'Public', pwd = 'Public')

sensors <- sensorInfo[sensorInfo$SiteID == siteID & sensorInfo$DataType == 'Rainfall', ]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2018-01-01T00:00:00'
endDate='2018-01-04T23:59:59'

