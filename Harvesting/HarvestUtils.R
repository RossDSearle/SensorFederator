library(xts)
library(DBI)
library(RSQLite)

getPeriod <- function(ts){

  data.frame(startDate=start(ts), endDate=end(ts), stringsAsFactors = F)
}

gapiness <- function(ts){
  tz <- na.omit(ts)
  gap_ts_Start=index(tz[ which( diff(index(tz))>1 )])
  gap_ts_End=index(tz[ which( diff(index(tz))>1 ) +1 ])

  ngaps <- length(gap_ts_Start)
  numdays <- gap_ts_End - gap_ts_Start
  gapi <- as.numeric(sum(numdays) / length(ts)) * 100

  return(data.frame(numGaps=ngaps, numDays=as.numeric(sum(numdays)), gapiness=gapi, stringsAsFactors = F))
}

sensorIsActive <- function(ts, days){

  lastRec <- as.Date(end(ts))
  if(Sys.Date()-lastRec > days){
    return(F)
  }else{
    return(T)
  }

}

doQuery <- function(con, sql){
  res <- dbSendQuery(con, sql)
  rows <- dbFetch(res)
  dbClearResult(res)
  return(rows)
}

sendStatement <- function(con, sql){
  rs <- dbSendStatement(con, sql)
  dbHasCompleted(rs)
  dbGetRowsAffected(rs)
  dbClearResult(rs)
}