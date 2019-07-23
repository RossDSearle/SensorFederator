


library(httr)
url <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?username=&password=&station=010905&start=&finish=&format=near&comment=&nameFrag=&radius=400&sortby=name'
SiloLocs <- getURL(url, .opts = myOpts)

resp <- GET(url)
SILOLocs<-read.delim(textConnection( content(resp, "text")),header=FALSE,sep="|",strip.white=TRUE)
colnames(SILOLocs) <- c('ID', 'StationID', 'Name', 'latitude', 'longitude', 'State', 'Num', 'stnType')
coordinates(SILOLocs) <- ~longitude+latitude
crs(SILOLocs) <- CRS("+proj=longlat +datum=WGS84")


#