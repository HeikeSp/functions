
library(RMySQL)
library(yaml)
login = yaml.load_file("../functions/login.yaml")
phenotyper = dbConnect(MySQL(), user=login$user, password=login$passwd, dbname=login$db, host=login$host)  



# function for MPI greenhouse trials: 48656, 51790 
# original query: fw_dw_mpi_greenhouse_query-2015-06-15.sql in "repos/database_scripts" 
# NETTO FW: 366/55
# NETTO DW: 366/69
# connection: phenotype_plants


func_get_fw_dw_mpi_greenhouse_data <- function(){
  
  fw_dw_mpi_greenhouse_query <- paste("SELECT
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
                                      (P1.number / P2.number) AS FW_DW FROM (
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
                                           INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (48656, 51790)
                                           INNER JOIN cultures C ON C.id = PL.culture_id
                                           LEFT JOIN phenotype_plants PP ON PP.plant_id = PL.id
                                           LEFT JOIN phenotypes P ON P.id = PP.phenotype_id
                                           LEFT JOIN `values` V ON V.id = P.value_id
                                           LEFT JOIN aliquot_samples AS ASA ON ASA.aliquot_id = A.id
                                           WHERE V.id IN (169,170,171,172)
                                      ) AS Base
                                      LEFT JOIN subspecies AS SUB ON SUB.id = Base.subspecies_id 
                                      LEFT JOIN sample_times ST on ST.culture_id = Base.experiment_id and ST.sample_date = Base.sample_date 
                                      LEFT JOIN phenotype_plants PP1 ON PP1.plant_id = Base.plant_id 
                                      LEFT JOIN phenotypes P1 ON P1.id = PP1.phenotype_id 
                                        AND Base.sample_date = P1.date AND P1.value_id = 55 AND P1.entity_id = 366 
                                      LEFT JOIN phenotype_plants PP2 ON PP2.plant_id = Base.plant_id 
                                      LEFT JOIN phenotypes P2 ON P2.id = PP2.phenotype_id 
                                        AND P2.value_id = 69 AND P2.entity_id = 366 AND ((P2.date <= (P1.date + INTERVAL 7 DAY)) 
                                        AND (P2.date >= (P1.date + INTERVAL 1 DAY))) 
                                      WHERE P1.number IS NOT NULL AND P1.invalid IS NULL AND P2.number IS NOT NULL AND P2.invalid IS NULL
                                 ")
  
  fw_dw_query_send <- dbSendQuery(phenotyper, fw_dw_mpi_greenhouse_query)
  
  fw_dw_query_result <-  fetch(fw_dw_query_send, n=-1)

}




# function for MPI field trials: 44443, 56726 
# original query: fw_dw_mpi_field_query-2015-06-15.sql in "repos/database_scripts" 
# BRUTTO FW: 366/164
# BRUTTO DW: 366/227
# TARA: 803/163
# connection: phenotype_samples

func_get_fw_dw_mpi_field_data <- function(){
  
  fw_dw_mpi_field_query <- dbGetQuery(phenotyper, paste("SELECT
                                      Base.plant_id,
                                      Base.experiment_id,
                                      Base.experiment_name,
                                      Base.treatment,
                                      Base.sample_id,
                                      Base.sample_date,
                                      SUB.id AS cultivar_id,
                                      SUB.cultivar as cultivar,
                                      ST.description,
                                      (P2.number - P1.number) AS FW,
                                      (P3.number - P1.number) AS DW,
                                      ( (P2.number - P1.number) / (P3.number - P1.number) ) AS FW_DW 
                                      FROM (
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
                                      INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id in (44443, 56726)
                                      INNER JOIN cultures C ON C.id = PL.culture_id
                                      LEFT JOIN phenotype_plants PP ON PP.plant_id = PL.id
                                      LEFT JOIN phenotypes P ON P.id = PP.phenotype_id
                                      LEFT JOIN `values` V ON V.id = P.value_id
                                      LEFT JOIN aliquot_samples AS ASA ON ASA.aliquot_id = A.id
                                      WHERE V.id IN (169,170,171,172)
                                      ) AS Base
                                      LEFT JOIN subspecies AS SUB ON SUB.id = Base.subspecies_id 
                                      LEFT JOIN sample_times ST on ST.culture_id = Base.experiment_id and ST.sample_date = Base.sample_date
                                      LEFT JOIN phenotype_samples PS1 ON PS1.sample_id = Base.sample_id -- for Tara
                                      LEFT JOIN phenotypes P1 ON P1.id = PS1.phenotype_id AND P1.value_id = 163 AND P1.entity_id = 803
                                      LEFT JOIN phenotype_samples PS2 ON PS2.sample_id = Base.sample_id -- for FW
                                      LEFT JOIN phenotypes P2 ON P2.id = PS2.phenotype_id AND P2.value_id = 164 AND P2.entity_id = 366
                                      LEFT JOIN phenotype_samples PS3 ON PS3.sample_id = Base.sample_id -- for DW
                                      LEFT JOIN phenotypes P3 ON P3.id = PS3.phenotype_id AND P3.value_id = 227 AND P2.entity_id = 366
                                      WHERE P1.number IS NOT NULL AND P1.invalid IS NULL 
                                      AND P2.number IS NOT NULL AND P2.invalid IS NULL 
                                      AND P3.number IS NOT NULL AND P3.invalid IS NULL;
                                      "))

fw_dw_query_send <- dbSendQuery(phenotyper, fw_dw_mpi_field_query)

fw_dw_query_result <-  fetch(fw_dw_query_send, n=-1)
}



# function for JKI field trial 2012: 56875 
# original query: fw_dw_jki_field_query-2015-06-05.sql in "repos/database_scripts"
# NETTO FW: 366/55
# NETTO DW: 366/69
# connection: phenotype_samples

func_get_fw_dw_jki_field_data <- function(){

fw_dw_jki_field_query <- dbGetQuery(phenotyper, paste("SELECT
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
                               (P1.number / P2.number) AS FW_DW 
                               FROM (
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
                               INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id = 56875
                               INNER JOIN cultures C ON C.id = PL.culture_id
                               LEFT JOIN phenotype_plants PP ON PP.plant_id = PL.id
                               LEFT JOIN phenotypes P ON P.id = PP.phenotype_id
                               LEFT JOIN `values` V ON V.id = P.value_id
                               LEFT JOIN aliquot_samples AS ASA ON ASA.aliquot_id = A.id
                               WHERE V.id IN (169,170,171,172)
                               ) AS Base
                               LEFT JOIN subspecies AS SUB ON SUB.id = Base.subspecies_id 
                               LEFT JOIN sample_times ST on ST.culture_id = Base.experiment_id and ST.sample_date = Base.sample_date
                               LEFT JOIN phenotype_samples PS1 ON PS1.sample_id = Base.sample_id -- for FW
                               LEFT JOIN phenotypes P1 ON P1.id = PS1.phenotype_id AND P1.value_id = 55 AND P1.entity_id = 366
                               LEFT JOIN phenotype_samples PS2 ON PS2.sample_id = Base.sample_id -- for DW
                               LEFT JOIN phenotypes P2 ON P2.id = PS2.phenotype_id AND P2.value_id = 69 AND P2.entity_id = 366
                               WHERE P1.number IS NOT NULL AND P1.invalid IS NULL AND P2.number IS NOT NULL AND P2.invalid IS NULL 
                            "))

fw_dw_query_send <- dbSendQuery(phenotyper, fw_dw_jki_field_query)

fw_dw_query_result <-  fetch(fw_dw_query_send, n=-1)
}


# function for JKI greenhouse trial 1: 45985 
# original query: fw_dw_jki_greenhouse_query-2015-06-05.sql in "repos/database_scripts" 
# BRUTTO FW: 366/164
# BRUTTO DW: 366/227
# TARA: 803/163
# connection: phenotype_samples

func_get_fw_dw_jki_greenhouse_data <- function(){
  
  fw_dw_jki_greenhouse_query <- dbGetQuery(phenotyper, paste("SELECT
                                      Base.plant_id,
                                      Base.experiment_id,
                                      Base.experiment_name,
                                      Base.treatment,
                                      Base.sample_id,
                                      Base.sample_date,
                                      SUB.id AS cultivar_id,
                                      SUB.cultivar as cultivar,
                                      ST.description,
                                      (P2.number - P1.number) AS FW,
                                      (P3.number - P1.number) AS DW,
                                      ( (P2.number - P1.number) / (P3.number - P1.number) ) AS FW_DW 
                                      FROM (
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
                                      INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id = 45985
                                      INNER JOIN cultures C ON C.id = PL.culture_id
                                      LEFT JOIN phenotype_plants PP ON PP.plant_id = PL.id
                                      LEFT JOIN phenotypes P ON P.id = PP.phenotype_id
                                      LEFT JOIN `values` V ON V.id = P.value_id
                                      LEFT JOIN aliquot_samples AS ASA ON ASA.aliquot_id = A.id
                                      WHERE V.id IN (169,170,171,172)
                                      ) AS Base
                                      LEFT JOIN subspecies AS SUB ON SUB.id = Base.subspecies_id 
                                      LEFT JOIN sample_times ST on ST.culture_id = Base.experiment_id and ST.sample_date = Base.sample_date
                                      LEFT JOIN phenotype_samples PS1 ON PS1.sample_id = Base.sample_id -- for Tara
                                      LEFT JOIN phenotypes P1 ON P1.id = PS1.phenotype_id AND P1.value_id = 163 AND P1.entity_id = 803
                                      LEFT JOIN phenotype_samples PS2 ON PS2.sample_id = Base.sample_id -- for FW
                                      LEFT JOIN phenotypes P2 ON P2.id = PS2.phenotype_id AND P2.value_id = 164 AND P2.entity_id = 366
                                      LEFT JOIN phenotype_samples PS3 ON PS3.sample_id = Base.sample_id -- for DW
                                      LEFT JOIN phenotypes P3 ON P3.id = PS3.phenotype_id AND P3.value_id = 227 AND P2.entity_id = 366
                                      WHERE P1.number IS NOT NULL AND P1.invalid IS NULL 
                                      AND P2.number IS NOT NULL AND P2.invalid IS NULL 
                                      AND P3.number IS NOT NULL AND P3.invalid IS NULL
                                      "))
fw_dw_query_send <- dbSendQuery(phenotyper, fw_dw_jki_greenhouse_query)

fw_dw_query_result <-  fetch(fw_dw_query_send, n=-1)

}