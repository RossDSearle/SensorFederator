library(XML)
library(xml2)
library(stringr)
library(RSQLite)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backends.R')
source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Config.R')



###########  Provider = EPARF  #########

# Had to code it this way as all lggers didn't have an Outpost ID there for we can't use
# the normal lapply approach as some records are null
# we have to check wich sites are not null for OPID then iterate through these


usr <- 'DFreebairn'
pwd <- 'USQ'
urlData <- paste0('https://www.outpostcentral.com', '/api/2.0/dataservice/mydata.aspx?userName=', usr, '&password=', pwd, '&dateFrom=1/Dec/2017%2000:00:00&dateTo=1/Dec/2017%2001:00:00')

docRaw <- read_xml(urlData)
write_xml(docRaw,'c:/temp/outpost.xml')

doc <- xmlInternalTreeParse(docRaw)
nsDefs <- xmlNamespaceDefinitions(doc)
ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

providerInfo = list( provider= c('USQ'), backEnd=c('OutPost'), server=c('https://www.outpostcentral.com'), org=c('USQ'),
                     usr=c(usr), pwd=c(pwd),
                     access = c('Public'),
                     contact=c('ross'), orgURL=c('https://eparf.com.au/'),
                     Description=c('') )

rootDir = 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator'
n=25

siteDF <- generateSiteInfo_OutPost_idIterated(providerInfo, rootDir)

outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')
siteDF <- read.csv(outName)

#dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederatorDevelopment.sqlite"
dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
dbWriteTable(con, 'sites', siteDF, append = T)

dbDisconnect(con)



locs <- read.csv(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv'), stringsAsFactors = F)

generateSensorInfo_OutPost_idIterated(locs, providerInfo, rootDir, doc=docRaw)



generateSiteInfo_OutPost_idIterated <- function(providerInfo, rootDir){

      #doc <- read_xml(urlData)
      #write_xml(doc, 'c:/temp/EPARF.xml')

      sites <- xml_find_all(docRaw, "//opdata:sites/opdata:site")
      n=length(sites)

      opidl <- character(n)
      sitel <- character(n)
      latl  <- character(n)
      lonl  <- character(n)

      for (i in 1:length(sites)) {

       id <- xml_find_first(sites[[i]], "./opdata:loggers/opdata:logger/id")

          if(length(id) > 0){

            opid <- xml_text(id)
            #siteName <- str_replace(xml_text( xml_find_first(sites[[i]], "./projectReference") ), ',', ' ')
            siteName <- str_replace(xml_text( xml_find_first(sites[[i]], "./name") ), ',', ' ')
            print(siteName)
            lat <- xml_text( xml_find_first(sites[[i]] ,"./latitude"))
            lon <- xml_text( xml_find_first(sites[[i]] ,"./longitude"))
          opidl[i] <- opid
          sitel[i] <- siteName
          latl[i] <- lat
          lonl[i] <- lon

          }
      }


      locs <- data.frame(opidl, sitel,providerInfo$provider, providerInfo$backEnd, providerInfo$access, providerInfo$usr, providerInfo$pwd, latl, lonl,  providerInfo$org, providerInfo$contact, providerInfo$orgURL, providerInfo$Description, providerInfo$server, stringsAsFactors = F)
      colnames(locs) <- c('SiteID', 'SiteName', 'SensorGroup', 'Backend', 'Access', 'Usr', 'Pwd', 'Latitude', 'Longitude', 'Owner', 'Contact', 'ProviderURL', 'Description', 'ServerName')

      #outSiteDF <- locs[grepl(providerInfo$provider, locs$SiteID ),]
      outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_Sites.csv')

      write.csv(locs, outName, row.names = F, quote = F)
      cat(paste0('Site info for ', providerInfo$provider, ' written to ',  outName, '\n'))
      vc(outName)

      return(locs)

}


generateSensorInfo_OutPost_idIterated <- function(locs, providerInfo, rootDir, doc){


  sites <- xml_find_all(doc, "//opdata:sites/opdata:site")

  loc <- xml_find_first(doc,   "//opdata:sites/opdata:site/opdata:inputs/opdata:input/id")

  pb <- pbCreate(n, progress='text', style=3, label='Generating Sensor data.....',timer=TRUE)

  sensorDF <- getEmptySensorDF()


  for(i in 1:nrow(locs)){

    pbStep(pb, i)

    siteName <-locs$SiteName[[i]]

    ### need to keep coding from here

    siteName <- xpathSApply(doc ,"//opdata:sites/opdata:site/name", xmlValue, ns)[i]
    siteID <- xpathSApply(doc ,"//opdata:sites/opdata:site/id", xmlValue, ns)[i]
    streams <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/projectReference[text()='", siteName,"']/parent::opdata:site/opdata:inputs/opdata:input/name"), xmlValue, ns)
    streamID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/projectReference[text()='", siteName,"']/parent::opdata:site/opdata:inputs/opdata:input/id"), xmlValue, ns)


    loggerID <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/projectReference[text()='", siteName,"']/parent::opdata:site/opdata:loggers/opdata:logger/id"), xmlValue, ns)


    df <- data.frame( loggerID[1], T, streamID, streams, NA, NA, NA, NA, NA, F, '', stringsAsFactors = F)
    colnames(df) <- c('SiteID', 'Active', 'SensorID', 'SensorName', 'StartDate', 'EndDate', 'DataType', 'UpperDepth', 'LowerDepth', 'Calibrated', 'Units')
    sensorDF <- rbind(sensorDF, df)
  }

  outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv')
  write.csv(sensorDF, outName, row.names = F, quote = F)
  pbClose(pb)
  cat(paste0('Sensor info for ', providerInfo$provider, ' written to ',  outName, '\n'))
  cat('\n')
  cat('OK. Now manually curate this file to expose the data you want\n')
  cat("Don't forget to recompile the 'AllSensors.csv' & 'AllSites.csv' files afetr these changes\n")
  vc(outName)
}




outName <- paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv')
sensorDF <- read.csv(outName)

#dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederatorDevelopment.sqlite"
dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
dbWriteTable(con, 'sensors', sensorDF, append = T)
dbDisconnect(con)
