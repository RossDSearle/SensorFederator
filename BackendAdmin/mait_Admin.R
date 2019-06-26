library(RCurl)


library(httr)

url <- "https://intelliweb.mait.com.au/getdata?network=1099"
url <-"https://intelliweb.mait.com.au/getdata?network=1099&module=6&startdate=2017/01/01&enddate=2017/12/31"

bob <- GET(url,  authenticate("dpigetdata", "dpigetdata"))
maitstr <- content(bob, "text")
maitDF <- read.csv(text=maitstr, skip=1, stringsAsFactors = F)
maitDF

#, userpwd="dpigetdata:dpigetdata"
#
#
sensors <- getURL("https://intelliweb.mait.com.au/getdata?network=1099", userpwd="dpigetdata:dpigetdata")
write.csv(sensors,'c:/temp/mait/sensors.csv')
sdf <- read.csv(text=sensors, skip=1, stringsAsFactors = F)

head(sdf)
#
#
# cat
#
#
# d <- getURL("http://intelliweb.mait.com.au/getdata?network=1099&module=6&startdate=2017/01/01&enddate=2017/12/31", userpwd="dpigetdata:dpigetdata")
# cat(d, file='c:/temp/mait/mait2.csv')
# ddf <- read.csv(text=d, skip=1)
#
# moist <- sdf[grepl('Moisture',  sdf$Description),]
#
# for (i in 1:nrow(moist)){
#   d <- getURL(paste0("http://intelliweb.mait.com.au/getdata?network=1099&module=", moist$Module[i],   "&startdate=2017/01/01&enddate=2017/01/10"), userpwd="dpigetdata:dpigetdata")
#   ddf <- read.csv(text=d, skip=1)
#   print(head(ddf))
# }


# for (i in 1:nrow(sdf)){
#   print(sdf$Description[i])
#   d <- getURL(paste0("http://intelliweb.mait.com.au/getdata?network=1099&module=", moist$Module[i],   "&startdate=2018/01/01&enddate=2018/01/2"), userpwd="dpigetdata:dpigetdata")
#   ddf <- read.csv(text=d, skip=1)
#   print(colnames(ddf))
# }
#
#
# library(httr)
# r <- GET(paste0("http://intelliweb.mait.com.au/getdata?network=1099&module=", moist$Module[i],   "&startdate=2018/01/01&enddate=2018/01/2"), userpwd="dpigetdata:dpigetdata")
# r$status_code
# r$content
# content(r, "text")
#
# head(sensors)
# str_c



network = '1107' # USyd
network = '1099' # Vic Ag

#url <- 'http://intelliweb.mait.com.au/getdata?network=1099&module=16&startdate=2018/03/01&enddate=2018/03/04'

url <- paste0('http://intelliweb.mait.com.au/getdata?network=', network)
stations <- getURL(url, userpwd=paste0( "dpigetdata:dpigetdata"))
modules <- read.csv(text=stations, skip=1)
modules

cat(file = 'c:/temp/maitnamesUSyd.csv', append = FALSE )
for (i in 1:nrow(modules)) {
  #for (i in 1:2) {
  url <- paste0('http://intelliweb.mait.com.au/getdata?network=', network, '&module=', modules$Module[i],'&startdate=2018/01/01&enddate=2018/01/02')

  #url <- paste0('http://intelliweb.mait.com.au/getdata?network=', network, '&module=5','&startdate=2018/01/01&enddate=2018/09/04')
  sensorscsv <- getURL(url, userpwd=paste0( "dpigetdata:dpigetdata"))
  if(sensorscsv != ''){
      sensors <- read.csv(text=sensorscsv, skip=1, check.names = F)
      print(colnames(sensors))
      #l <- paste0(colnames(sensors), collapse = ", ")

      for (j in 1:length(colnames(sensors))) {
       l <- paste0(modules$Description[i],',TRUE,VicAgSMM,Mait,Public,dpigetdata,dpigetdata,http://intelliweb.mait.com.au,,,',modules$Module[i],',',colnames(sensors)[j],',')
       cat(paste0(l, '\n'),  file = 'c:/temp/maitnamesUSyd.csv', append = T )
      }
  }
}


url <- 'http://intelliweb.mait.com.au/getdata?network=1107&module=20&startdate=2018/03/01&enddate=2018/03/04'

sensors <- getURL(url, userpwd=paste0( "dpigetdata:dpigetdata"))
df <- read.csv(text=sensors, skip=1)
write.csv(sensors,'c:/temp/mait/sensors.csv')
sdf <- read.csv(text=sensors, skip=1, stringsAsFactors = F)

head(sdf)
