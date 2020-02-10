
source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorBackends/Backends/SensorCloud_Backend.R')




###############################################################
#####        CosMoz                                     #######
###############################################################


rootDir <- 'c:/temp/Boorowa'
providerInfo <- list(provider=c('Booroowa'), backEnd=c('Senaps'), server=c('https://senaps.io/api/sensor/v2'), org=c('CSIRO'),
                     usr=c('ross.searle@csiro.au'), pwd=c('QBP'),
                     access=c('Public'),
                     contact=c(''), orgURL=c('https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station'))



#providerInfo = list( provider= c('cosmoz'), backEnd=c('SensorCloud'), access = c('Public'), org=c('CSIRO'), usr=c(usr), pwd=c(pwd), contact=c('David.Mcjannet@csiro.au'), orgURL=c('http://cosmoz.csiro.au/'))
generateSiteInfo_SC(providerInfo, rootDir)
generateSensorInfo_SC(providerInfo, rootDir)


# Manually curate the data to be displayed in the App from 'cosmoz'
sensorInfo <- read.csv(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv'))
vc(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv'))

SM <- sensorInfo[str_detect(sensorInfo$SensorID, pattern ='soil_moisture_filtered'),  ]
outSM <- SM
outSM$DataType <- 'Soil-Moisture'
outSM$Calibrated <- T
outSM$Units <- 'percent'
outSM$UpperDepth <- 0
outSM$LowerDepth <- 30
outSM$SensorName <- outSM$SensorID

rain <- sensorInfo[str_detect(sensorInfo$SensorID, pattern ='rainfall'),  ]
rain$DataType <- 'Rainfall'
rain$Calibrated <- T
rain$Units <- 'mm'
rain$UpperDepth <- 0
rain$LowerDepth <- 0
rain$SensorName <- outSM$SensorID

appDF <- rbind(outSM,rain)


write.csv(appDF, paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsToUse.csv'), row.names = F, quote = F)
vc(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsToUse.csv'))





###############################################################
#####        SFS                                     #######
###############################################################

providerInfo <- data.frame(provider=c('cerdi.sfs'), backEnd=c('SensorCloud'), server=c('https://sensor-cloud.io/api/sensor/v2'),org=c('Southern Farming Systems'),
                           usr=c('ross.searle@csiro.au'), pwd=c('rossiscool'),
                           access=c('Public'),
                           contact=c('jmidwood@sfs.org.au'), orgURL=c('http://www.sfs.org.au/ProbeTrax_MoistureProbeNetwork'))

#providerInfo = list( provider= c('cerdi.sfs'), backEnd=c('SensorCloud'), access = c('Public'), org=c('Southern Farming Systems'), usr=c(usr), pwd=c(pwd), contact=c('jmidwood@sfs.org.au'), orgURL=c('http://www.sfs.org.au/ProbeTrax_MoistureProbeNetwork'))
generateSiteInfo_SC(providerInfo, rootDir)
generateSensorInfo_SC(providerInfo, rootDir)



# a <- getSiteMetadata (SID='cerdi.sfs.5278.platform')
# getSensorMetadata(SensID='cerdi.sfs.5278.stream.5588.soil-moisture.1000mm')
# getData(streamID='cerdi.sfs.4935.stream.4983.soil-moisture.1000mm')



# Manually curate the data to be displayed in the App from 'cerdi.sfs'
#providerInfo = list( provider= c('cerdi.sfs'), backEnd=c('SensorCloud'), org=c('Southern Farming Systems'), usr=c(usr), pwd=c(pwd), contact=c('jmidwood@sfs.org.au'), orgURL=c('http://www.sfs.org.au/ProbeTrax_MoistureProbeNetwork'))
sensorInfo <- read.csv(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv'))
vc(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsAll.csv'))




# Add the soil moisture records and populate the depths

SM <- sensorInfo[str_detect(sensorInfo$SensorID, pattern ='soil-moisture'),  ]
bits <- str_split(SM$SensorID, '[.]')
props <- sapply(bits, function (x) x[6])
unique(props)

SM$DataType <- 'Soil-Moisture'
SM$Calibrated <- T


depths <- sapply(bits, function (x) x[7])
depths <- str_replace_all(depths, 'mm', '')

outSM <- SM
for(i in 1:nrow(SM)){

  dbits <- str_split(depths[i], pattern = '-')

  if(length(dbits[[1]]) == 1){
    outSM$UpperDepth[i] <- as.numeric(dbits[[1]][1])
    outSM$LowerDepth[i] <- as.numeric(dbits[[1]][1])

  }else{
    outSM$UpperDepth[i] <- as.numeric(dbits[[1]][1])
    outSM$LowerDepth[i] <- as.numeric(dbits[[1]][2])
  }
}
vcd(outSM)


outSM$DataType <- 'Soil-Moisture'
outSM$Calibrated <- T
outSM$Units <- 'percent'
outSM$SensorName <- outSM$SensorID


# Add the Rainfall records

rain <- sensorInfo[str_detect(sensorInfo$SensorID, pattern ='precipitation'),  ]

bits <- str_split(rain$SensorID, '[.]')
props <- sapply(bits, function (x) x[6])
unique(props)

rain$DataType <- 'Rainfall'
rain$Calibrated <- T
rain$Units <- 'mm'
rain$SensorName <- rain$SensorID


appDF <- rbind(outSM,rain)


write.csv(appDF, paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsToUse.csv'), row.names = F, quote = F)
vc(paste0(rootDir, '/SensorInfo/', providerInfo$provider, '_SensorsToUse.csv'))












