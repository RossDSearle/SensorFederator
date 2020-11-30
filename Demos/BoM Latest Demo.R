library(httr)
library(stringr)
library(urltools)


usr <-'ross.searle@csiro.au'
pwd <- 'S4QQBMk74zhnBnEpTcd6iLwlUredn6kekLkjFL'

locs <- fromJSON(paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations?sensorgroup=BoM&sensortype=Rainfall&extendedSet=T&usr=', usr, '&pwd=', pwd))

sensorTypes <- fromJSON(paste0("http://esoil.io/SensorFederationWebAPI/SensorAPI/getDataTypes"))
sensor <- sensorTypes[5,1]

outDF <- data.frame(siteID=character(), parameter=character(), Lon=numeric(), Lat=numeric(), dateTime=character(), Value=numeric())

startDate <- paste0(Sys.Date()-1, 'T00:00:00')
endDate <- paste0(Sys.Date(), 'T23:00:00')

for (i in 1:nrow(locs)) {

    print(paste0(i, ' of ', nrow(locs)))
    loc <- locs$SiteID[i]
    url <- paste0('http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=', loc, '&sensortype=', sensor, '&startdate=', startDate, '&endate=', endDate)

    test <- try( resp <- GET(url, timeout(30)),TRUE)
    if(isTRUE(class(test)=="try-error")){next} else {


        if(resp$status_code==200){

          response <-  content(resp, "text", encoding = 'UTF-8')
          df <- fromJSON(response)

          if(!grepl('error', response)){
            rec <- tail(df$DataStream[[1]], 1)
            val <- rec$v
            time <- rec$t
            outDF[i,] <- c(df$SiteID,df$DataType, as.numeric(df$Longitude), as.numeric(df$Latitude),time,as.numeric(val))
          }
        }
    }
  }


##########################
###  Make some maps ######
##########################

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


outDF$Value<-as.numeric(outDF$Value)
outDF$Lon <-as.numeric(outDF$Lon)
outDF$Lat <-as.numeric(outDF$Lat)
outDF <- outDF[outDF$Value!=0, ]
outDF<-na.omit(outDF)


world <- ne_countries(scale = "medium", returnclass = "sf")
aust <- st_crop(world, xmin=110, ymin=-43, xmax=154, ymax=-10)

sites <- st_as_sf(outDF, coords = c(  "Lon", "Lat"),  crs = 4326)

ggplot(data = aust) +
  geom_sf(data = aust) +
  geom_sf(data = sites , aes(color = Value)) +
  scale_colour_gradient(low = "blue", high = "red")





library(leaflet)
copedomain <- rev(range(sites$Value))
tpal <- colorNumeric(rev(brewer.pal(11,'Spectral')), domain = copedomain)
leaflet(sites) %>%
  addProviderTiles("Esri.OceanBasemap") %>%
  addCircleMarkers( radius = 5,
                    label = sites$Value,
                    color = 'grey80',
                    weight = 0.1,
                    fill = TRUE,
                    fillOpacity = 0.7, fillColor = ~tpal(Value)) %>%
  addLegend("topright", pal = tpal,
            values = copedomain,
            title = ,
            opacity = 1)


