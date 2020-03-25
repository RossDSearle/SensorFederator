source('C:/Users/sea084/Dropbox/RossRCode/Git/SensorBackends/Backends/Backends.R')
library(htmltidy)
server <- 'http://data.farmlinkrural.com'

property <- 'Soil moisture'

usr <- 'csbp'
pwd <- 'csbp12'

usr <- 'csirogrdc'
pwd <- 'grdc'



nodeID = 9824
nodeID = 9782

nodeID = 9825

authID <- adconLogin(adconServer = adconServer, usr=usr, pwd = pwd)

adconLogout(adconServer = adconServer, AuthID = authID)




conf <- adconConfig(adconServer = adconServer, usr=usr, pwd = pwd)
write(conf, file = 'C:/Users/sea084/Dropbox/RossRCode/Git/ProbeAPIs/AdconResponses/conf.xml')
xml_view(conf)

atts <- adconAttribute(usr=usr, pwd = pwd, nodeID = 15118)
xml_view(atts)
write(atts, file = 'C:/Users/sea084/Dropbox/RossRCode/Git/ProbeAPIs/AdconResponses/atts.xml')

startDate <- '20140125T10:45:00'
endDate <- '20140126T10:45:00'
res <- adconGetDataDateRange(usr, pwd, 15784, startDate, endDate, deltaSecs )
xml_view(res)

nodeID = 15849
nodeID = 15862

xmlData <- adconGetData(usr=usr, pwd = pwd, nodeID = nodeID, date = '20140125T10:43:44', slots =10000)
write(xmlData, file = 'C:/Users/sea084/Dropbox/RossRCode/Git/ProbeAPIs/AdconResponses/sm.xml')
xml_view(xmlData)

tz <- adconGenerateTimeSeries(xmlData)
plot(tz)



usr=usr
pwd = pwd
property = property


vcd(md)







providerInfo = list( provider= c('RAIN'), backEnd=c('Adcon'), server=c('http://data.farmlinkrural.com'), org=c('CSIRO'),
                     usr=c('csirogrdc'), pwd=c('grdc'),
                     access = c('Public'),
                     contact=c('Yvette Oliver'), orgURL=c('http://data.farmlinkrural.com'))

generateSiteInfo_Adcon(providerInfo, rootDir, getRaw = T)
generateSensorInfo_Adcon(providerInfo, rootDir, getRaw = T)




usr='samdb'
pwd='demo'
server <- 'http://aqualab-data.dyndns.info'

conf <- adconConfig(adconServer=server, usr=usr, pwd = pwd)
write(conf, file = 'C:/Users/sea084/Dropbox/RossRCode/Git/ProbeAPIs/AdconResponses/conf.xml')
xml_view(conf)


adconGetData(adconServer=server, usr,pwd,'562', date='20140125T10:43:44', slots = 5000)

#SA
'http://aqualab-data.dyndns.info/'
