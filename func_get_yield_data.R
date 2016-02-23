library(RMySQL)
library(yaml)
login = yaml.load_file("../functions/login.yaml")
phenotyper = dbConnect(MySQL(), user=login$user, password=login$passwd, dbname=login$db, host=login$host)  

func_get_yield_data <- function(project = "trost"){
  
  if(project == "trost"){
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
    JOIN phenotype_plants ph_pl1 on pl.id = ph_pl1.plant_id
    JOIN phenotypes ph1 on ph1.id = ph_pl1.phenotype_id and ph1.value_id IN (169,170,171,172)
    JOIN phenotype_plants ph_pl2 on pl.id = ph_pl2.plant_id
    JOIN phenotypes ph2 on ph2.id = ph_pl2.phenotype_id and ph2.value_id in (53,55,188,69,189,211,190,191,130)
    JOIN trost_prod.values v on ph2.value_id = v.id
    JOIN entities en on ph2.entity_id = en.id
    JOIN subspecies sub on sub.id = pl.subspecies_id")
  }
  else if(project == "valdis"){
    yield_query <- paste("SELECT
    pl.id AS plant_id,
    pl.culture_id AS culture,
    pl.lineid AS line_id,
    plantlines.line_alias AS alias,
    plantlines.name AS name,
    ph1.value_id AS treatment,
    ph2.date AS measurement_date,
    ph2.time AS measurement_time,
    ph2.entity_id AS entity_id,
    en.name AS entity_name,
    ph2.value_id AS value_id,
    v.attribute,
    v.value AS unit,
    ph2.number AS number
    FROM plants AS pl
    JOIN phenotype_plants ph_pl1 on pl.id = ph_pl1.plant_id
    JOIN phenotypes ph1 on ph1.id = ph_pl1.phenotype_id and ph1.value_id IN (169,170,171,172)
    JOIN phenotype_plants ph_pl2 on pl.id = ph_pl2.plant_id
    JOIN phenotypes ph2 on ph2.id = ph_pl2.phenotype_id and ph2.value_id IN (188,189,190,191) and ph2.date > '2014-01-01' and ph2.program_id != 147
    JOIN trost_prod.values v on ph2.value_id = v.id
    JOIN entities en on ph2.entity_id = en.id
    JOIN plantlines on plantlines.id = pl.lineid")
  } 
  else {
    print("unknown project")
  }

  yield_query_send <- dbSendQuery(phenotyper, yield_query)
  
  yield_query_result <-  fetch(yield_query_send, n=-1)
  
  return(yield_query_result)

}

func_get_plant_number <- function(project = "valdis"){
  plant_number_query <- paste("SELECT
  pl.id AS plant_id,
  pl.culture_id AS culture,
  pl.lineid AS line_id,
  ph1.number AS plant_number
  FROM plants AS pl
  JOIN phenotype_plants ph_pl1 on pl.id = ph_pl1.plant_id and pl.culture_id IN (72247, 72275, 72292, 72396, 72482)
  JOIN phenotypes ph1 on ph1.id = ph_pl1.phenotype_id and ph1.value_id = 308 and ph1.entity_id = 21")
  
  plant_number_query_send <- dbSendQuery(phenotyper, plant_number_query)
  
  plant_number_result <-  fetch(plant_number_query_send, n=-1)
  
  return(plant_number_result)
}
