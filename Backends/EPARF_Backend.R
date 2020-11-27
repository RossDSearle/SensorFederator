
timeOutPeriod = 180


getData_Outpost<- function(usr=usr, pwd=pwd, opID=opID, sensorID=sensorID, dateFrom=dateFrom,  dateTo=dateTo){

  url <- paste0(providerInfo$server, '/api/2.0/dataservice/mydata.aspx?userName=', usr, '&password=', pwd,
                '&outpostID=', opID, '&inputID=', sensorID, '&dateFrom=', dateFrom, '&dateTo=', dateTo)
  dataXML <- getURL(url, .opts = outpostOpts)
}

getURLAsync_EPARF <- function(x){
  bits <- str_split(x, '[|]')
  url <- bits[[1]][1]
  opID <- bits[[1]][2]

  #response <- sendRequest(url)
  resp <- GET(url, timeout(300))
  if(http_error(resp)){
    return(NULL)
  }
  response <- content(resp, "text", encoding = 'UTF-8')

  ndf<- EPARF_GenerateTimeSeries(response, retType = 'df', opID=opID)
  return(ndf)
}


sendRequestEPARF <- function(url){
  tryCatch(
    expr = {
      resp <- GET(url, timeout(timeOutPeriod))
      response <- content(resp, "text", encoding = 'UTF-8')
      return(response)

    },
    error = function(e){
      message('Caught an error!')
      print(e)
      return(NULL)
    },
    warning = function(w){
      # message('Caught an warning!')
      print(w)
    },
    finally = {
      #message('All done, quitting.')
    }
  )
}




EPARF_GenerateTimeSeries <- function(response, retType = 'df', opID){
  xmlObj=xmlParse(response, useInternalNodes = TRUE)
  doc <- xmlRoot(xmlObj)
  nsDefs <- xmlNamespaceDefinitions(doc)
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))

  rawDates <- xpathSApply(doc ,paste0("//opdata:sites/opdata:site/id[text()='", opID,"']/parent::opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/date"), xmlValue, ns)

  if(length(rawDates) < 1 ){
    ndf <- data.frame(theDate=character(), Values=numeric())
    #colnames(ndf)<- c('theDate', 'Values')
    #print('HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH')
    return(NULL)
    #stop('No records were returned for the specified query')
  }

  dl <- str_replace(rawDates, 'T', ' ')
  vals <- as.numeric(xpathSApply(doc ,paste0("//opdata:sites/opdata:site/id[text()='", opID,"']/parent::opdata:site/opdata:inputs/opdata:input/opdata:records/opdata:record/value"), xmlValue, ns))

  if(retType == 'xts'){
    tz <- xts(as.numeric(vals), order.by = dl)
    return (tz)
  }else if(retType == 'df'){
    ndf <- data.frame(dl, vals)
    colnames(ndf)<- c('theDate', 'Values')
    return(ndf)
  }else{
    stop(cat(retType, 'is an unkown data return type. Options are', paste(knownAdconReturnTypes, collapse=',' )), call. = F)
  }
}