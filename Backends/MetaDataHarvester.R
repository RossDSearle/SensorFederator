library(httr)
library(jsonlite)
library(RCurl)
library(xts)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')




head(ts)
tail(ts)

getPeriod(ts)

getPeriod <- function(ts){

  data.frame(startDate=start(tr), endDate=end(tr))
}

gapiness <- function(ts){
    tz <- na.omit(ts)
    gap_ts_Start=index(tz[ which( diff(index(tz))>1 )])
    gap_ts_End=index(tz[ which( diff(index(tz))>1 ) +1 ])

    ngaps <- length(gap_ts)
    numdays <- gap_ts_End - gap_ts_Start
    gapi <- as.numeric(sum(numdays) / length(ts)) * 100
}

sensorIsActive <- function(ts, days){

  lastRec <- as.Date(end(ts))
  if(Sys.Date()-lastRec > days){
    return(F)
  }else{
    return(T)
  }

}

