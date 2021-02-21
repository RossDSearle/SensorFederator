library(RCurl)
library(jsonlite)
library(httr)
library(stringr)
library(xml2)



getURLAsync_BoM_Latest <- function(x){

  bits <- str_split(x, '[|]')
  url <- bits[[1]][1]
  dt <- bits[[1]][2]

  resp <- GET(url, timeout(5))
  response <-  content(resp, "text", encoding = 'UTF-8')

 #   tryCatch({
 #   dataStreamsDF <- synchronise(async_map(urls, getURLAsync_BoM_Latest, .limit = asyncThreadNum))
 #   }, error = function(e)
 #   {
 #     return(NULL)
 # })



  if(response=='' | response=='[]'){
    outList <-   vector("list")
    return(outList)
  }else{
    ndf <- BoM_Latest_GenerateTimeSeries(response, retType = 'df', dataType=dt)
    return(ndf)
  }
}



BoM_Latest_GenerateTimeSeries <- function(response, retType = 'df', dataType){

  ddf2 <- fromJSON(response)
  ddf <- ddf2$observations$data

  if(nrow(ddf) == 0){
    return(data.frame())
  }


  odf <- data.frame(ddf$local_date_time_full, ddf[dataType])
  idxs <- which(grepl(pattern = '-', odf[dataType]))
  if(length(idxs) > 0){
    odf <- odf[-idxs,]
  }
  odf <- na.omit(odf)
  colnames(odf) <- c('dt', 'v')

  yyyy <- str_sub(odf$dt, 1,4)
  mm <- str_sub(odf$dt, 5,6)
  dd <- str_sub(odf$dt, 7,8)
  h <- str_sub(odf$dt, 9,10)
  m <- str_sub(odf$dt, 11,12)
  s <- str_sub(odf$dt, 13,14)

  dts <- as.POSIXct(paste0(yyyy, '-', mm, '-', dd, 'T', h, ':', m, ':',s), format = "%Y-%m-%dT%H:%M:%OS")

  # rd <- ddf[dataType]
  # #rd[is.na(rd), 1] <- 0
  # rd[rd=='-', 1] <- NA
  #
  # rdo <- as.numeric(na.omit(rd[,1]))
  #
  # if(dataType=='rain_trace'){
  #
  #   d2 <- diff(rdo)
  #   d <- c(rdo[1], d2)
  #
  # }else{
  #   d<-rdo
  # }

  vals <- as.numeric(odf$v)
  odf <- data.frame(theDate=dts, Values=vals)

  #odf
  return(odf)

}
