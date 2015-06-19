library(RMySQL)
library(yaml)
login = yaml.load_file("../libpurzel/login.yaml")
phenotyper = dbConnect(MySQL(), user=login$user, password=login$passwd, dbname=login$db, host=login$host)  

func_get_yield_data <- function(){

  yield_query <- paste("select
  pl.id as plant_id,
  pl.culture_id as culture,
  pl.subspecies_id AS cultivar_id,
  sub.cultivar AS cultivar,
  ph1.value_id as treatment,
  ph2.date as measurement_date,
  ph2.time as measurement_time,
  ph2.entity_id as entity_id,
  en.name as entity_name,
  ph2.value_id as value_id,
  v.attribute,
  v.value as unit,
  ph2.number as number
  from plants pl
  join phenotype_plants ph_pl1 on pl.id = ph_pl1.plant_id
  join phenotypes ph1 on ph1.id = ph_pl1.phenotype_id and ph1.value_id IN (169,170,171,172)
  join phenotype_plants ph_pl2 on pl.id = ph_pl2.plant_id
  join phenotypes ph2 on ph2.id = ph_pl2.phenotype_id and ph2.value_id in (53,55,188,69,189,211,190,191,130)
  join trost_prod.values v on ph2.value_id = v.id
  join entities en on ph2.entity_id = en.id
  join subspecies sub on sub.id = pl.subspecies_id")

  yield_query_send <- dbSendQuery(phenotyper, yield_query)
  
  yield_query_result <-  fetch(yield_query_send, n=-1)
  
  return(yield_query_result)

}