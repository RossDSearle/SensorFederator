library(httr)
library(dplyr)
library(DBI)
library(RSQLite)
library(forecast)
library(ggplot2)
library(reshape)
library(xts)
library(leaflet)
library(mapview)
library(png)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/HarvestUtils.R')

dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
dbStorePath <- paste0("C:/Projects/SensorFederator/DataStore/SensorFederatorDataStore.db")

conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)

product <- 'Openloop_Wetness_Index'

rootDir <- 'C:/Projects/SMIPS'
modelTSRoot <- paste0(rootDir, '/SMIPSAnalysis')

outdir <- paste0(rootDir, '/ProbeValidations/', product)
if(!dir.exists(outdir)){dir.create(outdir, recursive = T)}
if(!dir.exists(paste0(outdir, '/Maps'))){dir.create(paste0(outdir, '/Maps'), recursive = T)}

ps <- read.csv('C:/Projects/SMIPS/ProbeAnalysis/ProbeQualitySummary.csv', stringsAsFactors = F)
str(ps)
head(ps)



#outDF <- data.frame(stringsAsFactors = F)
odf <- data.frame(stringsAsFactors = F)

for (i in 1:nrow(ps)) {
  print(i)
  sid <- ps$SiteID[i]

  #smipsTSPath  <- paste0(modelTSRoot, '/ProbSitesSMIPSts/SMIPS_', sid, '.csv'  )
  smipsTSPath  <- paste0(modelTSRoot, '/ProbSites', product, '/', product, '_', sid, '.csv'  )
  awraTSPath  <- paste0(modelTSRoot, '/ProbSitesAWRA_sm_pct/sm_pct_', sid, '.csv'  )

  if(file.exists(smipsTSPath)){

  smipsDF <- read.csv(smipsTSPath, stringsAsFactors = F)
  smipsTS <- xts(smipsDF[,3], order.by=as.Date(smipsDF[,2]))
  tsSMIPS <- getTidyTS( smipsTS, removeNA=T, upperBound=1, lowerBound=0)

  awraDF <- read.csv(awraTSPath, stringsAsFactors = F)
  awraTS <- xts(awraDF[,3], order.by=as.Date(awraDF[,2]))
  tsawra <- getTidyTS( awraTS, removeNA=T, upperBound=1, lowerBound=0)

  sqlSenNum <- paste0("select * from Sensors where SiteID='", sid, "' and DataType = 'Soil-Moisture'")
  storeSensors <- doQuery(conStore, sqlSenNum)

  rec <- ps[i,]
  allTS <- vector('list', length = nrow(storeSensors))

  probeStats <- data.frame(stringsAsFactors = F )
  for (j in 1:nrow(storeSensors)) {

    if(storeSensors$upperDepth[j] != 'NA'){

      sensorNum <- storeSensors$sensorNum[j]

          cn <-  paste0('SM.',storeSensors$upperDepth[j])
          if(cn %in% colnames(rec)){
            qv <-as.numeric(rec[cn][1])

            if(qv==1){

              probeData <- na.omit(getSensorData(conStore, sensorNum))
              its <- xts(probeData$value, order.by=as.Date(as.POSIXlt(probeData$dt, tz = "Australia/Brisbane",  tryFormats = c("%Y-%m-%d %H:%M:%OS") )))
              colnames(its) <- 'value'

              probeTs <- getTidyTS(ts=its, removeNA = T, upperBound = 100, lowerBound = 3, flatCnt = 5, removeOutliers = T, removeStartDays = 10)

              if(!is.null(probeTs)){

                if(nrow(probeTs) > 0){

                  mts <- merge(tsSMIPS*100,probeTs, join = 'inner')
                  SMIPScompTS <- na.omit(mts)

                  mts2 <- merge(tsawra*100,probeTs, join = 'inner')
                  awracompTS <- na.omit(mts2)

                  if(nrow(SMIPScompTS) > 0){
                    colnames(SMIPScompTS) <- c('SMIPS', paste0( storeSensors$SiteID[j],'!', storeSensors$upperDepth[j] ))
                    colnames(awracompTS) <- c('AWRA', paste0( storeSensors$SiteID[j],'!', storeSensors$upperDepth[j] ))

                    dfcompSMIPS <- na.omit(as.data.frame(coredata(SMIPScompTS)))
                    dfcompAWRA <- na.omit(as.data.frame(coredata(awracompTS)))

                    corSMIPS <- cor(SMIPScompTS[,1], SMIPScompTS[,2])
                    corAWRA <- cor(awracompTS[,1], awracompTS[,2])

                    rdf <- data.frame(storeSensors[j, ], qv=qv, depth=storeSensors$lowerDepth[j], R2_SMIPS=as.numeric(corSMIPS),R2_AWRA=as.numeric(corAWRA))
                    odf<- rbind(odf, rdf)

                    colnames(probeTs) <- paste0('SM ',storeSensors$upperDepth[j])
                    allTS[[j]] <-  probeTs
                    hdf <- data.frame(depth=storeSensors$lowerDepth[j], smips=corSMIPS, awra=corAWRA)
                    colnames(hdf) <- c('Depth', 'SMIPS', 'AWRA')
                    probeStats <- rbind(probeStats, hdf)
                    colnames(probeStats) <- c('Depth', 'SMIPS', 'AWRA')
                  }
                }
              }

            }

          }
    }

  }

  if(!is.null(allTS[[1]])){
  tss <- do.call(merge, allTS)


 allts1 <- merge(tss,(smipsTS*100), join = 'inner')
 ALLTS <- merge(allts1,(awraTS*100), join = 'inner')
 colnames(ALLTS)[ncol(ALLTS)-1] <- 'SMIPS'
 colnames(ALLTS)[ncol(ALLTS)] <- 'AWRA'

  colrs <- c('red', 'seagreen4', 'mediumpurple2', 'tan4', 'yellow', 'dimgray', 'chocolate', 'peachpuff', 'turquoise3', 'plum1')
  ALLTSdf <-data.frame(date=index(ALLTS), coredata(ALLTS))


  rval <- mean(as.numeric(probeStats$SMIPS ))

  #png(filename = paste0('C:/Temp/probeValidations/', format(round(rval, 2), nsmall = 2), '_', sid, '.png'), width = 1000, height = 1000)
  png(filename = paste0(outdir, '/', format(round(rval, 2), nsmall = 2), '_', sid, '.png'), width = 1000, height = 1000)

  split.screen(c(4, 1))       # split display into two screens
  split.screen(c(1, 2), screen = 3)
  split.screen(c(1, 2), screen = 4)

  screen(n = 1, new = T)
  erase.screen(n=1)
  par(mar = c(2, 2, 2, 2))
  with(ALLTSdf, plot(ALLTSdf[, 1], ALLTSdf[, 2], type="l", xlab="", main=paste0("Probe Vs SMIPS - ", sid)  ,ylim=c(0, 100) ) )
  for (i in 2:(ncol(ALLTSdf)-2)) {
    lines(ALLTSdf[, 1], ALLTSdf[, i], cex=1, col=colrs[i-1])
  }
  lines(ALLTSdf[, 1], ALLTSdf[, ncol(ALLTSdf)-1], lwd=2, col='black')
  legend("topright", colnames(ALLTSdf)[2:(ncol(ALLTSdf)-2)], cex=0.5, pch = 15, col = colrs)

  screen(n = 2, new = T)
 # erase.screen(n-2)
  par(mar = c(2, 2, 2, 2))
  with(ALLTSdf, plot(ALLTSdf[, 1], ALLTSdf[, 2], type="l", xlab="", main=paste0("Probe Vs AWRA - ", sid)  ,ylim=c(0, 100) ), cex.main=0.7 )
  for (i in 2:(ncol(ALLTSdf)-2)){
    lines(ALLTSdf[, 1], ALLTSdf[, i], cex=1, col=colrs[i-1])
  }
  lines(ALLTSdf[, 1], ALLTSdf[, ncol(ALLTSdf)], lwd=2, col='black')
  legend("topright", colnames(ALLTSdf)[2:(ncol(ALLTSdf)-2)], cex=0.5, pch = 15, col = colrs)


  screen(n = 5, new = TRUE)
  erase.screen(5)
  par(mar = c(0.9, 0.9, 0.9, 0.9))
  plot(1, type="n", xlab=" Probe Value", ylab="Model Value",xlim=c(0, 100), ylim=c(0, 100), main = 'Probe V SMIPS', cex.main=0.7)
  for (i in 2:(ncol(ALLTSdf)-2)) {
    points(ALLTSdf[, i], ALLTSdf[, (ncol(ALLTSdf)-1)], pch=19, cex=0.2, col=colrs[i-1])
  }
  legend("topright", colnames(ALLTSdf)[2:(ncol(ALLTSdf)-2)], cex = 0.5,pch = 15, col = colrs)

  screen(n = 6, new = TRUE)
  par(mar = c(0.9, 0.9, 0.9, 0.9))
  plot(1, type="n", xlab=" Probe Value", ylab="Model Value",xlim=c(0, 100), ylim=c(0, 100), main = 'Probe V AWRA', cex.main=0.7)
  for (i in 2:(ncol(ALLTSdf)-2)) {
    points(ALLTSdf[, i], ALLTSdf[, (ncol(ALLTSdf))], pch=19, cex=0.2, col=colrs[i-1])
  }
  legend("topright", colnames(ALLTSdf)[2:(ncol(ALLTSdf)-2)], cex = 0.5,pch = 15, col = colrs)


 e <- as.numeric(rec$Longitude)
 n <- as.numeric(rec$Latitude)
 print(paste0(e, ' ', n))

  screen(n = 7)
  par(mar = c(0, 0, 0, 0))
  m <- leaflet() %>%
    clearMarkers() %>%
    addTiles(group = "Map") %>%
    addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
    setView(lng = e, lat = n, zoom = 14) %>%
    addCircleMarkers(   lng = e, lat = n,
                        stroke = FALSE,
                        fillOpacity = 1,
                        color = 'blue',
                        radius = 10)


  mapshot(x=m, file = paste0(outdir, '/Maps/', sid, '.png'), url=paste0(outdir, '/Maps/', sid, '.html'), remove_url=F)
  img <- readPNG(paste0(outdir, '/Maps/', sid, '.png'))
  plot(0, type='n', xlim=0:1, ylim=0:1,    main="", xaxt='n', ann=FALSE, yaxt='n')
  rasterImage(img, 0, 0, 1, 1)
  m=NULL
  img=NULL

  screen(n = 8)
  erase.screen(8)
  par(mar = c(0, 0, 0, 0))
  pdf <- probeStats

  pdf$SMIPS <-  format(round(pdf$SMIPS, 2), nsmall = 2)
  pdf$AWRA <-  format(round(pdf$AWRA, 2), nsmall = 2)

  plot(0, type='n', xlim=0:1, ylim=0:1,    main="", xaxt='n', ann=FALSE, yaxt='n')
  plotrix::addtable2plot(table=pdf, x=0.1, y=0.1)

  close.screen(all.screens = TRUE)
  dev.off()

   }

 }
}

#write.csv(outDF, 'C:/Projects/SMIPS/ProbeAnalysis/ProbeQualitySummaryPerRecord.csv', row.names = F)
write.csv(odf, paste0('C:/Projects/SMIPS/ProbeAnalysis/ProbeCorrelations_', product, '.csv'), row.names = F)



odf <- read.csv(paste0('C:/Projects/SMIPS/ProbeAnalysis/ProbeCorrelations_', product, '.csv'))

odff1 <- odf[!grepl('opSID_', odf$SiteID), ]
odff2 <- odff1[!grepl('hussat_', odff1$SiteID), ]
odff3 <- odff2
odff3 <- odf
odff3 <-  odff2[odff2$R2_SMIPS > 0, ]

mean(odff2$R2_SMIPS, na.rm=T)
mean(odff2$R2_AWRA , na.rm=T)

mean(odff3$R2_SMIPS, na.rm=T)
mean(odff3$R2_AWRA , na.rm=T)

print('R squared for SMIPS')
depths <- seq(100, 900, 100)
for(i in 1:length(depths)){
  vals <- odff3[odff3$depth==depths[i], ]$R2_SMIPS
  m <- mean(vals, na.rm=T )
  print(paste0('Depth = ', depths[i], ' : R2 = ', format(round(m, 2), nsmall = 2), ' : Count = ', length(vals) ))
}

print('R squared for AWRA')
depths <- seq(100, 900, 100)
for(i in 1:length(depths)){
  vals <- odff3[odff3$depth==depths[i], ]$R2_AWRA
  m <- mean(vals, na.rm=T )
  print(paste0('Depth = ', depths[i], ' : R2 = ', format(round(m, 2), nsmall = 2), ' : Count = ', length(vals) ))
}


con <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RO)
res <- dbSendQuery(con, 'select * from sites')
sites <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con)



qual <- read.csv('C:/Projects/SMIPS/ProbeAnalysis/ProbeQualitySummary.csv', stringsAsFactors = F)
qual1 <- qual[rowSums(qual == 1) >= 1, ]
nrow(qual1)
#write.csv(qual1, 'C:/Projects/SMIPS/ProbeAnalysis/sitesQual1.csv')
#edited to add land use

qual1 <- read.csv('C:/Projects/SMIPS/ProbeAnalysis/sitesQual1.csv')
ascR <- raster('C:/Projects/SMIPS/Ancillary/ASC_ORD_mosaic.tif')

ascPts <- extract(ascR, data.frame(qual1$Longitude, qual1$Latitude))
sites2 <- data.frame(qual1, ascPts)



allInfo <- merge(sites2, odff3, by='SiteID')
write.csv(allInfo, 'C:/Projects/SMIPS/ProbeAnalysis/ProbeCorrelationsWith_AWRA&Smips_SM_FullSiteData.csv')

colnames(allInfo)[16] <- 'SMIPS'
colnames(allInfo)[17] <- 'AWRA'

# boxplot per probe network
cts <- aggregate(SiteID ~ SensorGroup , allInfo, function(x) length(unique(x)))
xlabs <- paste(cts$SensorGroup,"\n(N=",cts$SiteID,")",sep="")

mdata <- melt(allInfo[,c(1,3,16,17)], id=c("SiteID","SensorGroup"))
p <- ggplot(mdata, aes(x=SensorGroup, y=value, fill=variable)) +
  geom_boxplot(position=position_dodge(1)) + ggtitle("SMIPS V Probe correlations by Probe Network") + labs(y="R Squared Value", x = "Probe Network") + labs(fill = "SM Model") + scale_x_discrete(labels=xlabs)
p


# boxplot per probe Depth
mdata <- melt(allInfo[,c(1,12,16,17)], id=c("SiteID","lowerDepth"))
level_order <- c('50', '100', '200', '300', '400', '500', '600', '700', '800', '900', '1000', '1100')
#mdata$lowerDepth <- as.numeric(mdata$lowerDepth)
p <- ggplot(mdata, aes(x=factor(mdata$lowerDepth, level = level_order), y=value, fill=variable)) +
  geom_boxplot(position=position_dodge(1)) + ggtitle("SMIPS V Probe correlations by Depth") + labs(y="R Squared Value", x = "Probe Depth (mm)") + labs(fill = "SM Model")
p

# boxplot per Soil Type
allInfo$ascPts[allInfo$ascPts==4] <- 'Chromosol'
allInfo$ascPts[allInfo$ascPts==1] <- 'Vertosol'
allInfo$ascPts[allInfo$ascPts==11] <- 'Rudosol'
allInfo$ascPts[allInfo$ascPts==12] <- 'Calcarosol'
allInfo$ascPts[allInfo$ascPts==2] <- 'Sodosol'
allInfo$ascPts[allInfo$ascPts==7] <- 'Tenosol'
allInfo$ascPts[allInfo$ascPts==8] <- 'Kandosol'
allInfo$ascPts[allInfo$ascPts==3] <- 'Dermosol'
allInfo <- na.omit(allInfo)
cts <- aggregate(SiteID ~ ascPts , allInfo, function(x) length(unique(x)))

xlabs <- paste(cts$ascPts,"\n(N=",cts$SiteID,")",sep="")

mdata <- melt(allInfo[,c(1,8,16,17)], id=c("SiteID","ascPts"))
p <- ggplot(mdata, aes(x=factor(ascPts), y=value, fill=variable)) +
  geom_boxplot(position=position_dodge(1)) + ggtitle("SMIPS V Probe correlations by Depth") + labs(y="R Squared Value", x = "Probe Depth (mm)") + labs(fill = "SM Model") + scale_x_discrete(labels=xlabs)

p




