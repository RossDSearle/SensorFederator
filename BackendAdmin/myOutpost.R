library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)
library(XML)
library(xml2)
library(rAmCharts)

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


sDate <- '1/Jul/2019%2000:00:00'
eDate='12/Jul/2019%2000:00:00'


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

rtot <- cumsum(dfo$vals)

sensorInfo <- getAuthorisedSensors(usr = 'Public', pwd = 'Public')

sensors <- sensorInfo[sensorInfo$SiteID == siteID & sensorInfo$DataType == 'Rainfall', ]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2018-01-01T00:00:00'
endDate='2018-01-04T23:59:59'


dfs <- data.frame(theDate=as.POSIXct( dfo$dts), theVals=as.numeric(rtot))
amTimeSeries(data=dfs, col_date = 'theDate', col_series = colnames(dfs)[-1], linetype=0, color = 'red', linewidth = 5, bullet = 'diamond')

amTimeSeries(data=dfs, col_date = 'theDate', col_series = colnames(dfs)[-1], maxSeries=100)
