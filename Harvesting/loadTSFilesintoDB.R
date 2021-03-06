library(DBI)
library(RSQLite)
library(stringr)


machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'FANCY-DP'){
  rootDir <-  'C:/Projects/SensorFederator'

  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Backends/Backend_Utils.R')
  source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/Harvesting/HarvestUtils.R')
}else {
  dbPath <- ""
}

rootDir <-  'C:/Projects/SensorFederator'
dbFedPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
dbStorePath <- paste0(rootDir, "/DataStore/SensorFederatorDataStore.db")




#inDir <- paste0(rootDir, '/DataDumps/SFS_All')
inDir <- paste0(rootDir, '/DataDumps/WA')
inDir <- paste0(rootDir, '/DataDumps/VicAg2_1345')
inDir <- paste0(rootDir, '/DataDumps/VicAg2_1070')
inDir <- paste0(rootDir, '/DataDumps/DailyTS')
inDir <- paste0(rootDir, '/DataDumps/EPARF')
inDir <- paste0(rootDir, '/DataDumps/Booroowa')

fls <- list.files(inDir, full.names = T)

fls <- fls[grepl('OzNet', fls)]
fls <- fls[grepl('VicAg_', fls)]
fls <- fls[grepl('Usyd_', fls)]

con <- dbConnect(RSQLite::SQLite(), dbStorePath, flags = SQLITE_RW)
fk_On <- 'PRAGMA foreign_keys = ON;'
dbExecute(con, fk_On)

for (i in 1:length(fls)) {

  #print(i)
  #########   Open the csv file   ############
  d <- read.csv(fls[i])
  ts <- xts(d[,2], order.by=as.Date(d[,1]))
  bits <- str_split(basename(fls[i]), '!')
  sname <- bits[[1]][1]
  sens <- bits[[1]][2]
  upd <- as.numeric(bits[[1]][3])
  lowd <- as.numeric(bits[[1]][4])

  dtype <- str_remove( bits[[1]][5], '.csv')


  print(paste0(i, ' of ', length(fls), ' : ', basename(fls[i])))
  ##########   Check if sensor exists   ###################################
  sqlsens <- paste0("SELECT * FROM Sensors where SiteID = '", sname, "' and
    SensorID = '", sens, "' and
    upperDepth = '", upd, "' and
    lowerDepth = '", lowd, "' and
    DataType   = '", dtype, "';")
  sensor <- doQuery(con, sqlsens)

  ###########   If sensor doesn't exist add it  #############################
  if(nrow(sensor)==0){
       sql <- paste0("INSERT INTO Sensors (SiteID, SensorID, upperDepth ,lowerDepth, datatype) VALUES('", sname, "', '", sens, "', '", upd, "', '", lowd, "', '",dtype, "');")
       sendStatement(con, sql)
  }

  ##########  Get the sensor number ##########################################

  sqlsens <- paste0("SELECT * FROM Sensors where SiteID = '", sname, "' and
    SensorID = '", sens, "' and
    upperDepth = '", upd, "' and
    lowerDepth = '", lowd, "' and
    DataType   = '", dtype, "';")
  sensor <- doQuery(con, sqlsens)
  sNum <- sensor$sensorNum

 ##########   Append the new TS data  ########################################

   sql <-  paste0("SELECT datetime(sensorData.dateTime) as dt from sensorData where sensorNum = " , sNum  )
   dts <- doQuery(con, sql)

   if(nrow(dts) > 0){

     tsMax <- max(as.Date(dts$dt))

     sqlDel <-  paste0("DELETE from sensorData where sensorNum = " , sNum, " and sensorData.dateTime = '" , tsMax, "'" )
     sendStatement(con, sqlDel)

     sdate <- tsMax
     edate <- paste0(Sys.Date())

     newData <- ts[paste0(sdate, "/", edate)]
     sdf <- data.frame( sensorNum=sNum, dateTime=as.character(index(newData)), value=coredata(newData))
     dbWriteTable(con, "sensorData", sdf, append = TRUE)

   }else{
     bits <- str_split(d[, 1], ' ')
     dys <- sapply(bits, function (x) x[1])
     sdf <- data.frame( sensorNum=sNum, dateTime=dys, value=d[, 2])
     dbWriteTable(con, "sensorData", sdf, append = TRUE)
     #dbReadTable(con, "sensorData")

   }
}

dbDisconnect(con)

