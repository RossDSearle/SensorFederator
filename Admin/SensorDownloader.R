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
pb <- pbCreate(nrow(locs), progress='text', style=3, label='Progress',timer=TRUE)
cntr <- 1

for (i in 1:nrow(locs)) {

  sid = locs$SiteID[i]
  #print(sid)

  url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorInfo?siteid=', sid, '&sensortype=')
  ssi <- fromJSON(URLencode(url))

dTypes <- unique(ssi$DataType)

for (k in 1:length(dTypes)) {

  dtype <- dTypes[k]
  ssiF <- ssi[ssi$DataType == dtype,]

      for (j in 1:nrow(ssiF)) {

        outFile <- paste0(rootDir, '/DailyTS/', ssiF$SiteID[j], '!', ssiF$SensorID[j], '!',  ssiF$UpperDepth[j], '!', ssiF$LowerDepth[j], '!', ssiF$DataType[j], '.csv')

        cntr <- cntr + 1
        pbStep(pb, step=cntr)

        if(!file.exists(outFile)){

            url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', sid, '&sensorid=', ssiF$SensorID[j], '&sensortype=',ssiF$DataType[j], '&startdate=', sDate ,'&enddate=', eDate ,'&aggperiod=days')
            jsn <- getURL(URLencode(url))

            if(responseIsOK(jsn)){

              stream <- fromJSON(jsn)

              if(!length(stream$DataStream[[1]]) == 0){



                  df <-convertJSONtoDF(jsn)

                  allts <- to.TS(df)
                  tr <- na.trim(allts)
                  #plot(tr)
                  tr[tr==-99] <- NA
                  #plot(tr)
                  #nrow(stream)

                  siteID <- stream$SiteID[1]
                  sensorID <- stream$SensorID[1]
                  upperDepth <- stream$UpperDepthCm[1]
                  lowerDepth <- stream$LowerDepthCm[1]
                  prop <- stream$DataType[1]
                  outDF <- data.frame(dt=index(tr), coredata(tr)[,1],row.names=NULL, stringsAsFactors = F)
                  colnames(outDF)[2] <- paste0(prop, '_', upperDepth, '_', lowerDepth)
                  print(outFile)
                  write.csv(outDF, paste0(outFile), row.names = F)
                  cntr <- cntr+1
                  pbStep(pb, step=cntr, label='')
            }else{
              cat(paste0(locs$SiteName[i], ',', sid, ',',ssi$SensorID[j], '\n'), file =  paste0('C:/Projects/SensorFederator/NonExistantSensors.txt'), append = T)
              break
            }

            }else{

              cat(paste0(locs$SiteName[i], ',', sid, ',',ssi$SensorID[j], '\n'), file =  paste0('C:/Projects/SensorFederator/NonExistantSensors.txt'), append = T)
              break
            }

        }
      }
    }
}

pbClose(pb)

