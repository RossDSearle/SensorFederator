library(httr)
library(jsonlite)
library(stringr)
library(xts)

convertJSONtoDF <- function(resp){

  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))

  for (i in 1:nrow(xin)) {
    d <- xin[i,]
    dd <- d$DataStream
    outDF[, (i+1)] <- dd[[1]]$v

    if(is.null(d$UpperDepth[1])){
      suffix = paste0('_x', i)
    }else if(is.na(d$UpperDepth[1])){
      suffix = paste0('_x', i)
    }else if(d$UpperDepth == d$LowerDepth[1]){
      suffix = paste0('_', d$UpperDepth[1])
    }else{
      suffix =  paste0('_', d$UpperDepth[1], '_', d$LowerDepth[1])
    }
    cnames[i+1] <- c(paste0(d$DataType[1],suffix))
  }
  colnames(outDF) <- cnames
  return(outDF)
}

to.TS <- function(df){
  cns <-names(df)
  # change over of daylight saving causes this to throw an error if we don't get rid of resultant NAs in the index
  d <- as.POSIXct(str_trim(df$Date) , format = "%Y-%m-%d %H:%M:%S")
  dfdaylightsaving <- df[!is.na(d), ]

  ts <- xts(x=dfdaylightsaving[,-1],unique = FALSE, order.by=as.POSIXct(dfdaylightsaving[,1], format = "%Y-%m-%d %H:%M:%S"), tzone =  Sys.getenv("TZ"))
  #ts <- xts(x=df[,-1],unique = FALSE, order.by=as.Date(df[,1], format = "%Y-%m-%d %H:%M:%S"), tzone =  Sys.getenv("TZ"))

  colnames(ts) <- cns[-1]

  tformat(ts) <- "%Y-%m-%dT%H:%M:%S"

  return(ts)
}



siteID <- 'Usyd_Kurrajong'
siteID <- 'Cosmoz_1'
#siteID <- 'Cosmoz_10'
#siteID <- 'opSID_40104'

url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', siteID , '&sensortype=Soil-Moisture&startdate=2019-04-20T00%3A00%3A00&enddate=2020-03-20T00%3A00%3A00&aggperiod=days')

resp <- GET(url)
response <- content(resp, "text", encoding = 'UTF-8')

df <- convertJSONtoDF(response)
ts <- to.TS(df)

plot(ts)
addLegend("topright", on=1, legend.names = colnames(ts), lty=c(1, 1), lwd=c(2, 1), col=1:ncol(ts))
