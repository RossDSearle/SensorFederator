



getURL_Mait <- function(url, streams, usr, pwd){


  print(url)
  response <- getURL(url, userpwd=paste0(usr, ":", pwd))

 # print(response)
  ndf<- mait_GenerateTimeSeries(response, streams, retType = 'df')
  return(ndf)
}


mait_GenerateTimeSeries <- function(response, streams, retType = 'df'){

  ddf <- read.csv(text=response, skip=1, check.names = F, stringsAsFactors = F )

  ddf <- ddf[-1,]
  feats <- streams$SensorName
  #head(ddf)

  if(nrow(ddf) == 0){
    (stop('No records were returned for the specified query'))
  }

   #dts <-  as.POSIXct(ddf$DateTime)
  dts <-  as.POSIXct(ddf$DateTime, format = "%d/%m/%Y %H:%M" )

  outList <-   vector("list", length(feats) )
  for(i in 1:length(feats)){
    print(i)
    ndf <- data.frame(dts, as.numeric(ddf[,feats[i]]))
    colnames(ndf)<- c('theDate', 'Values')
    outList[[i]] <- ndf
  }

  #print(str(outList))
  return (outList)

}











