library(stringr)


AustXMax = 154.0;
AustXMin = 112.0;
AustYMax = -10.0;
AustYMin = -44.0;


check_GetSensorInfo <- function(siteid=NULL, sensorid=NULL, sensortype=NULL){

  if(!is.null(sensortype)){
    if(sensortype != ''){
      if(!sensortype %in% knownFeatures){stop(paste0("The requested Sensor Type is not currently supported. : parameter = sensortype : value = ", sensortype, ' The currently supported sensor types are : ', paste(knownFeatures, collapse = ', ')))}
    }
  }
}


check_GetSensorDataStreams <- function(siteid=NULL, sensorid=NULL, sensortype=NULL, startDate=NULL, endDate=NULL){

  if(!is.null(startDate)){
    dt <- strptime(startDate, "%Y-%m-%dT%H:%M:%S")
   if(is.na(dt)){stop(paste0("The specified 'startdate' is not valid or in the correct format. It needs to be in the format %Y-%m-%dT%H:%M:%S eg 2018-04-20T09:00:00"))}
  }

  if(!is.null(endDate)){
    dt <- strptime(endDate, "%Y-%m-%dT%H:%M:%S")
    if(is.na(dt)){stop(paste0("The specified 'startdate' is not valid or in the correct format. It needs to be in the format %Y-%m-%dT%H:%M:%S eg 2018-04-20T09:00:00"))}
  }

  if(!is.null(startDate) & !is.null(endDate)){
    d1 <- strptime(startDate, "%Y-%m-%dT%H:%M:%S")
    d2 <- strptime(endDate, "%Y-%m-%dT%H:%M:%S")
    dif <- d2 - d1
    if(dif<0){stop(paste0("Specified 'enddate' is before 'startdate'"))}
  }

  if(is.null(sensortype)){
    stop(paste0("You need to specify a 'sensortype'. ", 'The currently supported sensor types are : ', paste(knownFeatures, collapse = ', ')))
  }

  if(is.null(sensortype)){
    stop(paste0("You need to specify a 'siteid'." ))
  }

  if(!is.null(sensortype)){
    if(sensortype != ''){
      if(!sensortype %in% knownFeatures){stop(paste0("The requested Sensor Type is not currently supported. : parameter = sensortype : value = ", sensortype, ' The currently supported sensor types are : ', paste(knownFeatures, collapse = ', ')))}
    }
  }
}


check_GetSensorlocationsAsMap <- function( sensortype=NULL){

  if(!is.null(sensortype)){
    if(sensortype != ''){
      if(!tolower(sensortype) %in% tolower(knownFeatures)){stop(paste0("The requested Sensor Type is not currently supported. : parameter = sensortype : value = ", sensortype, ' The currently supported sensor types are : ', paste(knownFeatures, collapse = ', ')))}
    }
  }
}


check_GetSensorLocations <- function(siteid=NULL, sensortype=NULL, longitude=NULL, latitude=NULL, radius_km=NULL, bbox=NULL, numToReturn=NULL){

  if(!is.null(numToReturn)){
    if(numToReturn != ''){
      if(is.na(as.numeric(numToReturn))){stop(paste0("The number of records to return is not a valid numeric value. : parameter = numToReturn : value = ", numToReturn))}
    }
  }

  if(!is.null(sensortype)){
    if(sensortype != ''){
      if(!tolower(sensortype) %in% tolower(knownFeatures)){stop(paste0("The requested Sensor Type is not currently supported. : parameter = sensortype : value = ", sensortype, ' The currently supported sensor types are : ', paste(knownFeatures, collapse = ', ')))}
    }
  }


  ###  bbox request

  if(is.null(bbox) & is.null(longitude )& is.null(latitude)){
    return('OK')
  }


  if(!is.null(bbox)){
    isBboxReq=T
  }else if (!is.null(longitude) & !is.null(latitude)){
    isBboxReq=F
  }
  else{
    stop('Hmmm, not sure how you made this happen !!!')
  }

  if(isBboxReq){

    bits <- str_split(bbox, ';')
    print(length(bits[[1]]))
    if(length(bits[[1]]) != 4){stop(paste0("The 'bbox' parameter need to contain 4 valid geographic values in the form, bottom latitude, left longitude, top latitude, right latitude : parameter = bbox : value = ", bbox))}

    ymin <- as.numeric(bits[[1]][1])
    xmin <- as.numeric(bits[[1]][2])
    ymax <- as.numeric(bits[[1]][3])
    xmax <- as.numeric(bits[[1]][4])

    if(!all(!is.na(as.numeric(bits[[1]])))){stop(paste0("The 'bbox' parameter contains one or more non numeric values"))}

    if (xmin >= AustXMin & xmax <= AustXMax & ymin >= AustYMin & ymax <= AustYMax){
      return('OK')
    }else{
      stop(paste0("Supplied values for the bounding box are not in Australia : parameter = bbox : value = ", bbox))
    }



  }else{

    if(!is.null(longitude) & is.null(latitude) & is.null(bbox)){stop(paste0("You need to either specify a valid latitude for this point query. : parameter = latitude : value = NULL"))}
    if(!is.null(latitude) & is.null(longitude) & is.null(bbox)){stop(paste0("You need to either specify a valid longitude for this point query. : parameter = longitude : value = NULL"))}

    if(is.na(as.numeric(latitude))){stop(paste0("The specified latitude = '", latitude, "' is not a valid numeric value. : parameter = latitude : value = ", latitude))}
    if(is.na(as.numeric(longitude))){stop(paste0("The specified longitude = '", longitude, "' is not a valid numeric value. : parameter = longitude : value = ", longitude))}

    if(!is.null(radius_km)){
       if(radius_km != ''){
          if(is.na(as.numeric(radius_km))){stop(paste0("The specified search radius = '", radius_km, "' is not a valid numeric value. : parameter = radius_km : value = ", radius_km))}
       }
    }

    X<- as.numeric(longitude)
    Y <- as.numeric(latitude)

    if (X >= AustXMin & X <= AustXMax & Y >= AustYMin & Y <= AustYMax){
      return('OK')
    }else{
      stop(paste0("Supplied values for the location is not in Australia. : parameters = longitude, latitude : values = ", longitude, ", ", latitude))
    }


  }




  #if()

  return('OK')

}