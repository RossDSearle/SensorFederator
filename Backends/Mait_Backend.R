



getURL_Mait <- function(url, streams, usr, pwd){


  print(url)

  resp <- GET(url,  authenticate(usr, pwd))
  response <- content(resp, "text")

  #maitDF <- read.csv(text=maitstr, skip=1, stringsAsFactors = F)
  ndf<- mait_GenerateTimeSeries(response, streams, retType = 'df')
print(head(ndf))
  # response <- getURL(url, userpwd=paste0(usr, ":", pwd))
 # print(response)
 # ndf<- mait_GenerateTimeSeries(response, streams, retType = 'df')
  return(ndf)
}

getURLAsync_Mait <- function(x){

  bits <- str_split(x, '[|]')
  url <- bits[[1]][1]
  dataCols <- bits[[1]][4]
  usr <- bits[[1]][2]
  pwd <- bits[[1]][3]

  resp <- GET(url,  authenticate(usr, pwd))
  response <- content(resp, "text")


  if(response==''){
    outList <-   vector("list")
    return(outList)
  }else{
    ndf<- mait_GenerateTimeSeries2(response, dataCols, retType = 'df')
    return(ndf)
  }
}



mait_GenerateTimeSeries2 <- function(response, dataCols, retType = 'df'){

  ddf <- read.csv(text=response, skip=1, check.names = F, stringsAsFactors = F )
  print(head(ddf))
  ddf <- ddf[-1,]

  feats <- str_split(dataCols, ';')[[1]]
  #feats <- streams$SensorName

  if(nrow(ddf) == 0){
    #(stop('No records were returned for the specified query'))
    outList <-   vector("list")

    return(outList)
  }


  dts <-  as.POSIXct(ddf$DateTime, format = "%d/%m/%Y %H:%M" )

  outList <-   vector("list", length(feats) )
  for(i in 1:length(feats)){
    ndf <- data.frame(dts, as.numeric(ddf[,feats[i]]))
    colnames(ndf)<- c('theDate', 'Values')
    outList[[i]] <- ndf
  }

  return (outList)

}

mait_GenerateTimeSeries <- function(response, streams, retType = 'df'){

  ddf <- read.csv(text=response, skip=1, check.names = F, stringsAsFactors = F )

  ddf <- ddf[-1,]
  feats <- streams$SensorName

  if(nrow(ddf) == 0){
    #(stop('No records were returned for the specified query'))
    return(data.frame())
  }


  dts <-  as.POSIXct(ddf$DateTime, format = "%d/%m/%Y %H:%M" )

  outList <-   vector("list", length(feats) )
  for(i in 1:length(feats)){
    print(i)
    ndf <- data.frame(dts, as.numeric(ddf[,feats[i]]))
    colnames(ndf)<- c('theDate', 'Values')
    outList[[i]] <- ndf
  }

  return (outList)

}













