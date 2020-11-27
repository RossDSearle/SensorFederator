library(httr)
library(dplyr)
library(DBI)
library(RSQLite)

source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/HarvestUtils.R')

dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
dbStorePath <- paste0("C:/Projects/SensorFederator/DataStore/SensorFederatorDataStore.db")

conFed <- dbConnect(RSQLite::SQLite(), dbFedPath, flags = SQLITE_RW)
conStore <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)

SMIPSRoot <- 'C:/Projects/SMIPS/SMIPSAnalysis/ProbSitesSMIPSts'

ps <- read.csv('C:/Projects/SMIPS/ProbeAnalysis/ProbeQualitySummary.csv', stringsAsFactors = F)
str(ps)
head(ps)

outDF <- data.frame(stringsAsFactors = F)
odf <- data.frame(stringsAsFactors = F)

for (i in 1:nrow(ps)) {
  print(i)
  sid <- ps$SiteID[i]
  
  f <- paste0(SMIPSRoot, '/SMIPS_', sid, '.csv'  )
  if(file.exists(f)){
  
  smipsDF <- read.csv(f, stringsAsFactors = F)
  smipsTS <- xts(smipsDF[,3], order.by=as.Date(smipsDF[,2]))
  tsSMIPS <- getTidyTS( smipsTS, removeNA=T, upperBound=1, lowerBound=0)
  
  sqlSenNum <- paste0("select * from Sensors where SiteID='", sid, "' and DataType = 'Soil-Moisture'")
  storeSensors <- doQuery(conStore, sqlSenNum)
  
  rec <- ps[i,]
  
  for (j in 1:nrow(storeSensors)) {
    
    if(storeSensors$upperDepth[j] != 'NA'){
      
      sensorNum <- storeSensors$sensorNum[j]
      
          cn <-  paste0('SM.',storeSensors$upperDepth[j])
          if(cn %in% colnames(rec)){
            qv <-as.numeric(rec[cn][1])
            df <- data.frame(storeSensors[j, ], qv=qv, stringsAsFactors = F)
            outDF <- rbind(outDF,df)
            
            if(qv==1){
              
              
              probeData <- na.omit(getSensorData(conStore, sensorNum))
              
              its <- xts(probeData$value, order.by=as.Date(as.POSIXlt(probeData$dt, tz = "Australia/Brisbane",  tryFormats = c("%Y-%m-%d %H:%M:%OS") )))
              colnames(its) <- 'value'

              originTs <- getTidyTS(ts=its, removeNA = F, upperBound = NULL, lowerBound = NULL, flatCnt = NULL, removeOutliers = NULL, removeStartDays = NULL)
              probeTs <- getTidyTS(ts=its, removeNA = T, upperBound = 100, lowerBound = 3, flatCnt = 10, removeOutliers = T, removeStartDays = 100)
              
              if(!is.null(probeTs)){
                
                if(nrow(probeTs) > 0){
                  
                  mts <- merge(tsSMIPS*100,probeTs, join = 'inner')
                  compTS <- na.omit(mts)
                  
                  omts <- merge(tsSMIPS*100,originTs)
                  origcompTS <- na.omit(omts)
                  
                  sm <-  ma(tsSMIPS*100, 10)
                  smXTS <- xts(sm, order.by=index(tsSMIPS) )
                  smoothTS <- merge(smXTS, probeTs, join = 'inner')
                  smoothTS2 <- na.trim(smoothTS)
                  
                  
                  if(nrow(compTS) > 0){
                    colnames(compTS) <- c('SMIPS', paste0( storeSensors$SiteID[j],'!', storeSensors$upperDepth[j] ))
                    
                    dfcomp <- na.omit(as.data.frame(coredata(compTS)))
                    dforiginal<- na.omit(as.data.frame(coredata(origcompTS)))
                    dfSmooth <- na.omit(as.data.frame(coredata(smoothTS)))

                    cr <- cor(dfcomp[,1], dfcomp[,2])
                    cr2 <- cor(dforiginal[,1], dforiginal[,2])
                    cr3 <- cor(dfSmooth[,1], dfSmooth[,2])
                    rdf <- data.frame(storeSensors[j, ], qv=qv,siteIDx=sid2, depth=storeSensors$lowerDepth[j], R2Clean=cr,R2orig=cr2, R2smth=cr3)
                    odf<- rbind(odf, rdf)
                  }
                }
              }
              
            }
            
          }
        }
    }
  }
}

write.csv(outDF, 'C:/Projects/SMIPS/ProbeAnalysis/ProbeQualitySummaryPerRecord.csv', row.names = F)
write.csv(odf, 'C:/Projects/SMIPS/ProbeAnalysis/ProbeCorrelations.csv', row.names = F)




odff1 <- odf[!grepl('opSID_', odf$SiteID), ]
odff2 <- odff1[!grepl('hussat_', odff1$SiteID), ]
odff3 <-  odff2[odff2$R2orig > 0, ]

mean(odff3$R2Clean, na.rm=T)
mean(odff3$R2orig, na.rm=T)

depths <- seq(100, 900, 100)
for(i in 1:length(depths)){
  
  vals <- odff3[odff3$depth==depths[i], ]$R2orig
  m <- mean(vals, na.rm=T )
  print(paste0('Depth = ', depths[i], ' : R2 = ', format(round(m, 2), nsmall = 2), ' : Count = ', length(vals) ))  
}

mean(odf[odf$depth==100, ]$R2, na.rm=T )
mean(odf[odf$depth==200, ]$R2, na.rm=T)
mean(odf[odf$depth==300, ]$R2, na.rm=T)
mean(odf[odf$depth==300, ]$R2smth, na.rm=T)
mean(odf[odf$depth==400, ]$R2, na.rm=T)
mean(odf[odf$depth==400, ]$R2smth, na.rm=T)
mean(odf[odf$depth==500, ]$R2, na.rm=T)
mean(odf[odf$depth==600, ]$R2, na.rm=T)
mean(odf[odf$depth==700, ]$R2, na.rm=T)
mean(odf[odf$depth==800, ]$R2, na.rm=T)
mean(odf[odf$depth==900, ]$R2, na.rm=T)




