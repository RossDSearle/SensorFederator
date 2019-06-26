library(RSQLite)
library(DBI)
library(odbc)

dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)



makeRandomString <- function(n=1)
{
  lenght = sample(c(20:40))
  randomString <- c(1:n)
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),lenght, replace=TRUE),collapse="")
  }
  return(randomString)
}


dbListTables(con)
dbListFields(con, "AuthUsers")
df <- dbReadTable(con, "AuthUsers")
head(df)

dbReadTable(con, "AuthGroups")
dbReadTable(con, "AuthAccess")


makeUser(user = 'DataFarmer', firstname = 'Stephen', surname = 'van Rees', group = "DataFarmerGroup")
makeUser(user = 'AWRA-L', firstname = 'David', surname = 'Wright', group = "BoMGroup", emailAddr = "David.PeterWright@bom.gov.au" )

makeUser <- function(user, firstname, surname, group, emailAddr){
  
  pwd <- makeRandomString(1)
  
  sqlInsert <- paste0("Insert into AuthUsers ( usrID, FirstName, Surname,GroupName, Pwd, Email ) values ('", user, "','", firstname, "','",  surname, "','",  group, "','",  pwd, "','", emailAddr, "' )")
  
  print(sqlInsert)
  res <- dbSendStatement(con, sqlInsert)
  dbGetRowsAffected(res)
  
  sqlqry<- paste0("select * from AuthGroups where GroupName = '", group, "'")
  
  res <- dbSendQuery(con, sqlqry)
  df <- dbFetch(res)

   if( nrow(df) == 0){
     
     sqlInsert <- paste0("Insert into AuthGroups ( GroupName ) values ('", group, "')")
     
     print(sqlInsert)
     res <- dbSendStatement(con, sqlInsert)
     dbGetRowsAffected(res)
     
     sqlInsert <- paste0("Insert into AuthAccess ( GroupName, access ) values ('", group, "', 'None')")
     
     print(sqlInsert)
     res <- dbSendStatement(con, sqlInsert)
     dbGetRowsAffected(res)
   }
  
  sqlqry<- paste0("select * from AuthUsers where usrID = '", user, "'")
  
  res <- dbSendQuery(con, sqlqry)
  df <- dbFetch(res)
  print(df)
}





