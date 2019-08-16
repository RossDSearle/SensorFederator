library(RCurl)
library(jsonlite)
library(httr)
library(stringr)
library(xml2)



getURLAsync_SILO<- function(x){

  #debugMode=T
  #url <- x

  bits <- str_split(x, '[|]')
  url <- bits[[1]][1]
  dt <- bits[[1]][2]

  resp <- GET(url)
  response <- content(resp, "text")


  if(response=='' | response=='[]'){
    outList <-   vector("list")
    return(outList)
  }else{
    ndf <- SILO_GenerateTimeSeries(response, retType = 'df')
    return(ndf)
  }
}



SILO_GenerateTimeSeries <- function(response, retType = 'df'){

  sdf <-read.delim(textConnection(SiloData), header=F, sep="", strip.white=TRUE, skip = 37)
  sdf2 <- sdf[, c(1,2,3,5,7,9,11,13,15,16,17)]
  sc <- c('Date','Day','T.Max','T.Min','Rain','Evap','Radn','VP','RHminT','RHmaxT','Date2')
  colnames(sdf2) <- sc

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

