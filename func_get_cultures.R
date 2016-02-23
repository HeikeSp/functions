

library(RMySQL)
library(yaml)
login = yaml.load_file("../functions/login.yaml")
phenotyper = dbConnect(MySQL(), user=login$user, password=login$passwd, dbname=login$db, host=login$host)  


# solution: http://r.789695.n4.nabble.com/Insert-variable-in-RMySQL-Statement-td3296870.html

func_get_cultures <- function(experiment_id = c('48656', '51790', '44443', '56726') )
{
  res <- dbGetQuery(mydb,
                    paste("SELECT * FROM cultures WHERE cultures.id IN (",
                          paste(experiment_id, collapse=",") , ")" , sep=""))
  return(res)

}

func_get_cultures()  



res <- dbGetQuery(mydb,paste("SELECT * FROM cultures WHERE cultures.id IN (",paste(experiment_id, collapse=",") ,")" ,sep=""));
print(res); 


