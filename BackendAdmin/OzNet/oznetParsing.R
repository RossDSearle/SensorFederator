library(xml2)

rootUrl <- 'http://www.oznet.org.au/'

paths <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/OzNet/paths.csv')

siteName<- character(nrow(paths))
lats <- numeric(nrow(paths))
lons <- numeric(nrow(paths))
elevs <- numeric(nrow(paths))
code <- character(nrow(paths))

for(j in 1:nrow(paths)){

      url <- paste0(rootUrl, paths[j,2], '.html')
      #print(url)

    docRaw <- read_html(url)
    #doc <- xmlInternalTreeParse(docRaw)
    #doc.html = htmlTreeParse(url, useInternal = TRUE)
    #xpathSApply(doc.html, "/html/body/table/tr/td/font")

    nds <- xml_find_all(docRaw,"//html/body/table/tr/td/table/tr/td/font")

    for (i in 1:length(nds)) {
      t <- as.character(nds[i])

      if(grepl( 'Latitude', t)){
        bits <- str_split(t, '<b>')

        la1 <- str_remove(bits[[1]][2], "Latitude:</b>")
        la2 <- str_remove(la1, ",")
        la3 <- str_trim(la2)
        print(la3)
        lats[j] <- as.numeric(la3)

        la1 <- str_remove(bits[[1]][3], "Longitude:</b>")
        la2 <- str_remove(la1, "</font>")
        la3 <- str_trim(la2)
        lons[j] <- as.numeric(la3)

        siteName[j] <- as.character(paths$Site[j])
        code[j] <- as.character(paths$code[j])
      }

      if(grepl( 'Elevation', t)){
        bits <- str_split(t, '<b>')
        la1 <- str_remove(bits[[1]][2], "Elevation:</b>")
        la2 <- str_remove(la1, "</font>")
        la3 <- str_remove(la2, "m")
        la4 <- str_trim(la3)
        elevs[j] <- as.numeric(la4)
      }
    }
}



outDF <- data.frame(siteName, code, lons, lats, elevs, stringsAsFactors = F)

write.csv(outDF, 'C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/OzNet/sites.csv' )


sitesDF <- read.csv('C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/BackendAdmin/OzNet/sites.csv', stringsAsFactors = F)


rootOutDir <- 'c:/temp/ozNet'
urlRoot <- 'http://www.oznet.org.au/data/processed/webData/'

a <- seq(1,19,1)
yrs <- paste("", formatC(a, width=2, flag="0"), sep="")
seasons <- c('su', 'au', 'wi', 'sp')

for (i in 1:nrow(sitesDF)) {

  siteName <- sitesDF$siteName[i]
  region <- str_to_lower( str_split(siteName, ' ')[[1]][1])
  code <- str_to_lower(sitesDF$code[i])

  for (j in 1:length(yrs)) {

    for (k in 1:length(seasons)) {

      outDir <- paste0(rootOutDir, '/', siteName)
      if(!dir.exists(outDir)){
        dir.create(outDir, recursive = T)
      }

      fname <- paste0(code, '_', yrs[j], '_', seasons[k], '_sm.xls')
      url <- paste0(urlRoot, region, '/', code, '/', fname)
      fpath <- paste0(outDir, '/', fname)

      if(url.exists(url)){
        print(paste0('Downloading - ', url))
        download.file(url, fpath, mode='wb',  quiet=T )

      }
    }
  }
}




