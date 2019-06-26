library(httr)
library(jsonlite)
library(RCurl)
library(xts)
library(raster)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')

responseIsOK <- function(resp){
  o <- fromJSON(resp)
  return(is.null(o$error))
}

rootDir <-  'C:/Projects/SensorFederator'

sDate <- '2010-01-01T00:00:00'
eDate <- paste0(Sys.Date(), 'T23:59:59')

url <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations'
locs <- fromJSON(URLencode(url))
head(locs)

url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo')
ssiAll <- fromJSON(URLencode(url))


cat('', file =  paste0('C:/Projects/SensorFederator/NonExistantSensors.txt'))
pb <- pbCreate(nrow(ssiAll), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

for (i in 1:nrow(locs)) {

  sid = locs$SiteID[i]
  #print(sid)

  url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?siteid=', sid)
  ssi <- fromJSON(URLencode(url))


  for (j in 1:nrow(ssi)) {


    outFile <- paste0(rootDir, '/DailyTS/', ssi$SiteID[j], '!', ssi$SensorID[j], '!',  ssi$UpperDepth[j], '!', ssi$LowerDepth[j], '!', ssi$DataType[j], '.csv')

    cntr <- cntr + 1
    pbStep(pb, step=cntr)

    if(!file.exists(outFile)){

        url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensorid=', ssi$SensorID[j], '&sensortype=',ssi$DataType[j], '&startdate=', sDate ,'&enddate=', eDate ,'&aggperiod=days')
        jsn <- getURL(URLencode(url))

        if(responseIsOK(jsn)){

              df <-convertJSONtoDF(jsn)
              stream <- fromJSON(jsn)
              allts <- to.TS(df)
              tr <- na.trim(allts)
              #plot(tr)
              tr[tr==-99] <- NA
              plot(tr)
              #nrow(stream)

              siteID <- stream$SiteID[1]
              sensorID <- stream$SensorID[1]
              upperDepth <- stream$UpperDepthCm[1]
              lowerDepth <- stream$LowerDepthCm[1]
              prop <- stream$DataType[1]
              outDF <- data.frame(dt=index(tr), coredata(tr)[,1],row.names=NULL, stringsAsFactors = F)
              colnames(outDF)[2] <- paste0(prop, '_', upperDepth, '_', lowerDepth)
              write.csv(outDF, paste0('C:/Projects/SensorFederator/DailyTS/', siteID, '!',  sensorID, '!',  upperDepth, '!',  lowerDepth, '!', prop, '.csv' ), row.names = F)
              cntr <- cntr+1
              pbStep(pb, step=cntr, label='')
        }else{

          cat(paste0(locs$SiteName[i], ',', sid, ',',ssi$SensorID[j], '\n'), file =  paste0('C:/Projects/SensorFederator/NonExistantSensors.txt'), append = T)
          break
        }

    }
  }
}

pbClose(pb)

