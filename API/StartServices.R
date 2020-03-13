library(plumber)
library(htmltidy)





cat("\n")
cat("\n")
cat("You are now running the Sensor Federating Web Services API\n")
cat("\n")
cat("API uri root is '/SensorAPI'\n")
cat("\n")
cat("Currently supported API endpoints are \n")
cat("    - getSensorLocations\n")
cat("    - getSensorInfo\n")
cat("    - getSensorDataStreams\n")
cat("\n")
cat("Some usage examples...\n")
cat("-----------------------------\n")
cat(server, ":", portNum, "/SensorAPI/getSensorLocations\n", sep = '')
cat(server, ":", portNum,"/SensorAPI/getSensorLocations?usr=Public&pwd=Public&siteID=cosmoz.site.13.plat&sensorType=Soil-Moisture\n", sep = '')
cat("\n", sep = '')
cat(server, ":", portNum, "/SensorAPI/getSensorInfo?usr=Public&pwd=Public&siteID=cosmoz.site.13.plat&sensorType=Soil-Moisture\n", sep = '')
cat("\n", sep = '')
cat(server, ":", portNum, "/SensorAPI/getSensorDataStreams?usr=Public&pwd=Public&siteID=cosmoz.site.13.plat&sensorType=Soil-Moisture&aggPeriod=days&startDate=01-01-2018&endDate=05-01-2018\n", sep = '')
cat("\n", sep = '')
cat("\n", sep = '')
cat("Running the swagger UI at http://",server,":", portNum, "/__swagger__/\n", sep = '')
cat("\n", sep = '')


#cat(paste0(sensorRootDir, '/SensorInfo/AllSites.csv'), "\n", sep = '')
#cat(paste0(sensorRootDir, '/SensorInfo/AllSensors.csv'), "\n", sep = '')

# to start the web service on soil-discovery
#sudo supervisorctl -c /etc/supervisor/supervisord.conf restart plumber_sensorfederationwebapi

r <- plumb(paste0(deployDir, "/apiEndPoints.R"))  # Where 'myfile.R' is the location of the file shown above
print(r)
#
#  if(machineName == 'FANCY-DP'){
#    if("package:htmltidy" %in% search() ){
    # viewer <- getOption("viewer")
    # viewer('http://127.0.0.1:8070/__swagger__/')
#    }
#  }
 r$run(host = server, port=portNum, swagger = TRUE)








