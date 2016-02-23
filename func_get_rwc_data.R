#library(RODBC)
#phenotyper <- odbcConnect("Phenotyper") # Phenoyter is the DSN of the connection

library(RMySQL)
library(yaml)
login = yaml.load_file("../functions/login.yaml")
phenotyper = dbConnect(MySQL(), user=login$user, password=login$passwd, dbname=login$db, host=login$host)  

# dbClearResult(dbListResults(phenotyper)[[1]])
# 
# rs = dbSendQuery(phenotyper, "select * from cultures")
# data = fetch(rs, n=-1)

func_get_rwc_data <- function(){
  

  rwc_query <- paste("SELECT
                     Base.plant_id,
                     Base.experiment_id,
                     Base.experiment_name,
                     Base.treatment,
                     Base.sample_id,
                     Base.sample_date,
                     SUB.id AS cultivar_id,
                     SUB.cultivar as cultivar,
                     ST.description,
                     P1.number AS FW,
                     P2.number AS DW,
                     P3.number AS SW,
                     (P1.number - P2.number) / (P3.number - P2.number) AS RWC FROM (
                     SELECT DISTINCT
                     PL.id AS plant_id,
                     C.id AS experiment_id,
                     PL.subspecies_id,
                     C.description AS experiment_name,
                     V.value AS treatment,
                     ASA.sample_id,
                     A.sample_date
                     FROM aliquots A
                     INNER JOIN aliquot_plants AP ON AP.aliquot_id = A.id
                     INNER JOIN plants PL ON PL.id = AP.plant_id 
                          AND PL.culture_id IN (48656, 51790, 56575, 58243, 60319)
                     INNER JOIN cultures C ON C.id = PL.culture_id
                     LEFT JOIN phenotype_plants PP ON PP.plant_id = PL.id
                     LEFT JOIN phenotypes P ON P.id = PP.phenotype_id
                     LEFT JOIN `values` V ON V.id = P.value_id
                     LEFT JOIN aliquot_samples AS ASA ON ASA.aliquot_id = A.id
                     WHERE V.id IN (169,170,171,172)
                     ) AS Base
                     LEFT JOIN subspecies AS SUB ON SUB.id = Base.subspecies_id 
                     LEFT JOIN sample_times ST on ST.culture_id = Base.experiment_id 
                          AND ST.sample_date = Base.sample_date 
                     LEFT JOIN phenotype_plants PP1 ON PP1.plant_id = Base.plant_id 
                     LEFT JOIN phenotypes P1 ON P1.id = PP1.phenotype_id 
                          AND Base.sample_date = P1.date 
                          AND P1.value_id = 55 AND P1.entity_id = 366 
                     LEFT JOIN phenotype_plants PP2 ON PP2.plant_id = Base.plant_id 
                     LEFT JOIN phenotypes P2 ON P2.id = PP2.phenotype_id 
                          AND P2.value_id = 69 AND P2.entity_id = 366 
                          AND ((P2.date <= (P1.date + INTERVAL 7 DAY)) 
                          AND (P2.date >= (P1.date + INTERVAL 1 DAY))) 
                     LEFT JOIN phenotype_plants PP3 ON PP3.plant_id = Base.plant_id 
                     LEFT JOIN phenotypes P3 ON P3.id = PP3.phenotype_id 
                          AND P3.value_id = 156 AND P3.entity_id = 366 
                          AND ((P3.date <= (P1.date + INTERVAL 5 DAY)) 
                          AND (P3.date >= (P1.date + INTERVAL 1 DAY))) 
                     WHERE P1.number IS NOT NULL AND P1.invalid IS NULL 
                          AND P2.number IS NOT NULL 
                          AND P2.invalid IS NULL 
                          AND P3.number IS NOT NULL 
                          AND P3.invalid IS NULL
                     ")

#rwc_query_result <- sqlQuery(phenotyper, rwc_query)

rwc_query_send <- dbSendQuery(phenotyper, rwc_query)

rwc_query_result <-  fetch(rwc_query_send, n=-1)
}
