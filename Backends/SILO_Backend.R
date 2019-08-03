library(RCurl)
library(jsonlite)
library(httr)
library(stringr)
library(xml2)



getURLAsync_SILO<- function(x){

  debugMode=T
  url <- x



  if(debugMode){
    response <- readLines('C:/Temp/silo.txt')
    #response <-  content(resp, "text")

  }else{
    resp <- GET(url)
    response <- suppressMessages(content(resp, "text"))
  }

  writeLines(response, 'c:/temp/bom2.json')

  if(response=='' | response=='[]'){
    outList <-   vector("list")
    return(outList)
  }else{
    ndf <- SILO_GenerateTimeSeries(response, retType = 'df')
    return(ndf)
  }
}



SILO_GenerateTimeSeries <- function(response, retType = 'df'){

  ddf2 <- fromJSON(response)
  ddf2 <- readRDS('C:/Temp/silo.rds')

  dts <- as.POSIXct(paste0(ddf2$date, 'T00:00:00'), format = "%Y-%m-%dT%H:%M:%OS")
  bits <- str_split(ddf2$variables, ',')
  vals <-  sapply(bits, function (x) x[2])

  df<- data.frame(stringsAsFactors = F)
  for (i in 1:length(silo$data$variables)) {
    df <- rbind(df, silo$data$variables[[i]])
  }

  if(nrow(df) == 0){
    return(data.frame())
  }

  odf <- data.frame(theDate=dts, Values=df$value)
  return(odf)

}

