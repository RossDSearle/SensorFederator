source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')
library(htmltidy)
library(plotly)
library(dygraphs)



####   Outpost EPARF


usr <- 'EPARF'
pwd <- 'eparf'
siteID <- 'op15376'
sensorID <- '456113'

sensors <- sensorInfo[sensorInfo$SiteID == siteID & sensorInfo$DataType == 'Rainfall', ]
streams=sensors
backEnd='OutPost'
aggregSeconds=timeSteps$day
startDate <- '2019-01-01T00:00:00'
endDate='2019-01-04T23:59:59'



urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=',  usr, '&password=', pwd,
                  '&dateFrom=1/Jan/2019%2000:00:00&dateTo=', '2/Jan/2019%2000:00:00', '&outpostID=', siteID, '&inputID=', sensorID)

response <- GET(urlData)
stop_for_status(response) # stop if the response is an error
sensorData <- content(response, as="text")
ts <- convertJSONtoTS(sensorData)
RV$currentTS <- ts

response <- GET(urlData, .encoding = 'UTF-8-BOM')
#xml_view(dataXML)
cat(response, file='c:/temp/outpost.xml')

xmlObj=xmlParse(response, useInternalNodes = TRUE)
doc <- xmlRoot(xmlObj)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))


vals <- as.numeric(xpathSApply(doc ,"//opdata:sites/opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value", xmlValue, ns))
tail(vals, 10)
sum(vals)



sensorInfo <- getAuthorisedSensors()

startDate <- '2018-12-01T09:00:00'
endDate='2019-02-27T08:59:59'

att = 'Soil-Moisture'

#sensors <- sensorInfo[sensorInfo$SensorGroup == 'EPARF' & sensorInfo$DataType == att, ]
sensors <- sensorInfo[sensorInfo$SiteName == 'Mayfield S - Solo' & sensorInfo$SensorGroup == 'EPARF' & sensorInfo$DataType == att, ]
subSens <- sensors[1:3,]
vcd(subSens)




for (i in 1:nrow(sensors)) {

  possibleError <- tryCatch({
  print(sensors$SiteName[i])
  d <- getSensorData(streams=sensors[i,],  aggPeriod=timeSteps$days , startDate=startDate, endDate=endDate, numrecs = 10000)
  df <- data.frame( date=time(d), Vals=as.matrix(coredata(d)))
   write.csv(df, paste0('c:/temp/eparf/', att, ' - ', sensors$SiteName[i],' - ', sensors$SensorName[i] , '.csv'))
  p <- plot(d, main = paste0(att, ' - ',sensors$SiteName[i],' - ', sensors$SensorName[i] ))
  print(p)
  }
  ,
  error=function(e) {
    e
    print(paste("Oops! --> Error in Loop ",i,sep = ""))
  }
  )
  if(inherits(possibleError, "error")) next
}




d <- getSensorData(streams=sensors[3:12, ],  aggPeriod=timeSteps$none , startDate=startDate, endDate=endDate, numrecs = 10000)
df <- data.frame( date=time(d), Vals=as.matrix(coredata(d)))
dygraph(d , main = paste0('Tet'))  #%>%
plot(d)















