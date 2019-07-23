library(RCurl)
library(jsonlite)
library(httr)
library(stringr)
library(xml2)


library(httr)

urls <- c('http://www.bom.gov.au/nsw/observations/nswall.shtml',
'http://www.bom.gov.au/qld/observations/qldall.shtml',
'http://www.bom.gov.au/tas/observations/tasall.shtml',
'http://www.bom.gov.au/vic/observations/vicall.shtml',
'http://www.bom.gov.au/sa/observations/saall.shtml',
'http://www.bom.gov.au/wa/observations/waall.shtml',
'http://www.bom.gov.au/nt/observations/ntall.shtml')


for (i in 1:length(urls)) {

url <- urls[i]

resp <- GET(url)
webPage <-  content(resp, "text")

scraping<- read_html(url)
nds <- html_nodes(scraping, xpath = '//body//div//div//div//div//div//table//tbody//tr//a')

ourls <- xml_attr(nds, "href")
names <- xml_text(nds)

bits <- str_split(basename(ourls), '[.]')
ids <- sapply(bits, function (x) x[1])
wons <- sapply(bits, function (x) x[2])

print(names)

}













getURLAsync_IOT <- function(x){

  url  <- 'http://www.bom.gov.au/fwo/IDV60801/IDV60801.94827.json'
#
#   bits <- str_split(x, '[|]')
#   url <- bits[[1]][1]
#   usr <- bits[[1]][2]
#   pwd <- bits[[1]][3]
#   sd <- bits[[1]][4]
#   ed <- bits[[1]][5]
#   dt <- bits[[1]][6]

  resp <- GET(url,  authenticate(usr, pwd), add_headers(content_type="text/json;charset=UTF-8"))
  response <- suppressMessages(content(resp, "text"))





  if(response=='' | response=='[]'){
    outList <-   vector("list")
    return(outList)
  }else{
    ndf <- IOT_GenerateTimeSeries(response, retType = 'df', sd, ed, dt)
    return(ndf)
  }
}



IOT_GenerateTimeSeries <- function(response, retType = 'df', startDate=NULL, endDate=NULL, dataType){

  ddf <- fromJSON(response)
  print(head(ddf))

  if(nrow(ddf) == 0){
    #outList <-   vector("list")
    return(data.frame())
  }

  dts <-  as.POSIXct(ddf$date, format = "%Y-%m-%dT%H:%M:%OS"  )
  outList <-   vector("list", 1 )

  if( dataType %in% c('Rainfall')){
    ndf <- data.frame(dt=dts, vals=as.numeric(ddf$sum))
  }else{
    ndf <- data.frame(dt=dts, vals=as.numeric(ddf$avg))
  }

  colnames(ndf)<- c('theDate', 'Values')
  ## IOT endpoint doesn't really allow selection of time period so we do it manually here
  if(!is.null(startDate) & !is.null(endDate)){

    # odf <- ndf[ndf$dt >= as.Date(startDate, format = "%Y-%m-%dT%H:%M:%S") & ndf$dt <= as.Date(endDate, format = "%Y-%m-%d"),]
    odf <- ndf[ndf$theDate >= as.POSIXct(startDate, format = "%Y-%m-%dT%H:%M:%OS") & ndf$theDate <=  as.POSIXct(endDate, format = "%Y-%m-%dT%H:%M:%OS"),]
    return(odf)
    #    if(nrow(odf) == 0){
    #      outList <-   vector("list")
    #      return(outList)
    #    }else{
    #      ndf <- data.frame(dts2, vals)
    #
    #      return(ndf)
    #    }
    # }else{
    #   ndfRange <- ndf
    # }
  }

  return(ndf)

}
