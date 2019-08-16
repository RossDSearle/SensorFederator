library(httr)



url <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?username=&password=&station=010905&start=&finish=&format=near&comment=&nameFrag=&radius=400000&sortby=name'
#SiloLocs <- getURL(url, .opts = myOpts)

resp <- GET(url)
SILOLocs<-read.delim(textConnection( content(resp, "text")),header=FALSE,sep="|",strip.white=TRUE)
colnames(SILOLocs) <- c('ID', 'StationID', 'Name', 'latitude', 'longitude', 'State', 'Num', 'stnType')
write.csv(SILOLocs, 'c:/temp/silolocs.csv')
head(SILOLocs)
coordinates(SILOLocs) <- ~longitude+latitude
crs(SILOLocs) <- CRS("+proj=longlat +datum=WGS84")
plot(SILOLocs)
head(SILOLocs)



providerInfo = list( provider= c('SILO'), backEnd=c('SILO'), server=c('https://www.longpaddock.qld.gov.au/cgi-bin/silo'), org=c('Queensland Dept. of Environment and Science'),
                     usr=c('CSIROESB15'), pwd=c('DISKW8026'),
                     access = c('Public'),
                     contact=c(''), orgURL=c('https://www.longpaddock.qld.gov.au'),
                     Description=c('SILO Patched Point data.') )


recDF <- data.frame(SiteID=paste0('SILO_',SILOLocs$StationID, '_', rec$ids), SiteName=SILOLocs$V3, SensorGroup=providerInfo$provider, Backend=providerInfo$backEnd,
                    Access=providerInfo$access,Usr=providerInfo$usr,Pwd=providerInfo$pwd,Latitude=rec$lats,Longitude=rec$lons,
                    Owner=providerInfo$org,Contact=providerInfo$contact,ProviderURL=providerInfo$orgURL,NetworkInfoWebsite=providerInfo$orgURL,
                    Description=providerInfo$Description,ServerName=providerInfo$server, stringsAsFactors = F
)

head(SILOLocs)


