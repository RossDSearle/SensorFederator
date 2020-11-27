source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')
#source('/srv/plumber/SensorFederator/Backends/Backend_Config.R')
library(htmltidy)
library(plotly)
library(dygraphs)
library(httr)
library(XML)

myOpts <- curlOptions(connecttimeout = 200, ssl.verifypeer = FALSE)


start_time <- Sys.time()
s <- getAuthorisedSensors()
end_time <- Sys.time()
end_time - start_time
s <- getAuthorisedSensors(usr = 'Public', pwd = 'Public')
#s <- getAuthorisedSensors(usr = 'ross.searle@csiro.au', pwd = 'S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL')
sensorInfo <- s


urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  'EPARF', '&password=', 'eparf',
                  '&dateFrom=1/Dec/2017%2000:00:00&dateTo=', '3/Dec/2017%2000:00:00', '&outpostID=', 'op15376', '&inputID=', '456114')
dataXML <- getURL(urlData, .opts = myOpts , .encoding = 'UTF-8-BOM')
cat(dataXML, file='c:/temp/outpost.xml')

xmlObj=xmlParse(dataXML, useInternalNodes = TRUE)




locs <- getSensorLocations()

usr='Public'
pwd='Public'
siteID='opSID_42229'
siteID='opSID_20610'
sensorType='Soil-Moisture'
sensorID='785600'


startDate='2018-04-20T00:00:00'
endDate='2018-04-25T00:00:00'
aggPeriod=timeSteps$days



getSensorInfo(usr='Public', pwd='Public', siteID=siteID, sensorType='Soil-Temperature', verbose=F, sensorGroup=NULL, backend=NULL, owner=NULL)

getSensorDataStreams(usr=usr, pwd=usr, siteID=siteID, sensorType=sensorType, sensorID=NULL, startDate=startDate, endDate=endDate, aggPeriod=aggPeriod, outFormat='simpleTS', verbose=F, tempCorrect = F )
