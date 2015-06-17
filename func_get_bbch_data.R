
##################################################
# new version: can handle several experiment_ids #
##################################################


# based on RMySQL, parameter/variables 
# solution: http://r.789695.n4.nabble.com/Insert-variable-in-RMySQL-Statement-td3296870.html

func_get_bbch_data <- function(experiment_id = c('48656', '51790', '44443', '56726') ){
  
  library(RMySQL)
  library(yaml)
  login = yaml.load_file("login.yml")
  phenotyper = dbConnect(MySQL(), user=login$user, password=login$passwd, dbname=login$db, host=login$host)  
  
  data <- dbGetQuery(phenotyper, paste("SELECT
                                 Base.plant_id,
                                 Base.experiment_id,
                                 Base.experiment_name,
                                 Base.treatment,
                                 Base.sample_id,
                                 Base.sample_date,
                                 SUB.id AS cultivar_id,
                                 SUB.cultivar as cultivar,
                                 ST.description as description,
                                 BB.id AS bbch,
                                 P1.date as Phenotype_date
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
                                 INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (",
                               
                               paste(experiment_id, collapse=",") ,")", 
                               
                               "INNER JOIN cultures C ON C.id = PL.culture_id
                               LEFT JOIN phenotype_plants PP ON PP.plant_id = PL.id
                               LEFT JOIN phenotypes P ON P.id = PP.phenotype_id
                               LEFT JOIN `values` V ON V.id = P.value_id
                               LEFT JOIN aliquot_samples AS ASA ON ASA.aliquot_id = A.id
                               WHERE V.id IN (169,170,171,172)
                                 ) AS Base
                               LEFT JOIN subspecies AS SUB ON SUB.id = Base.subspecies_id 
                               LEFT JOIN sample_times ST on ST.culture_id = Base.experiment_id and ST.sample_date = Base.sample_date 
                               LEFT JOIN phenotype_plants PP1 ON PP1.plant_id = Base.plant_id 
                               INNER JOIN phenotypes P1 ON P1.id = PP1.phenotype_id AND P1.value_id = 221 AND P1.entity_id = 21
                               INNER JOIN phenotype_bbches PHBB ON PHBB.phenotype_id = P1.id
                               INNER JOIN bbches BB ON BB.id = PHBB.bbch_id
                               AND ((P1.date <= (Base.sample_date + INTERVAL 5 DAY)) AND (P1.date >= (Base.sample_date - INTERVAL 5 DAY)))",
                               sep="" ) )
  return(data)
  dbDisconnect(phenotyper) 
}




# solution for parametrization using RODBCext
# http://cran.r-project.org/web/packages/RODBCext/vignettes/Parameterized_SQL_queries.html

func_get_bbch_data_RODBCext <- function(experiment_id = c('48656', '51790', '44443', '56726') ){
  
  library(RODBCext)
  connHandle <- odbcConnect("Phenotyper")

  filterData <- data.frame(PL.culture_id = experiment_id)
  
  data <- sqlExecute(connHandle, 
                     "SELECT
                      Base.plant_id,
                      Base.experiment_id,
                      Base.experiment_name,
                      Base.treatment,
                      Base.sample_id,
                      Base.sample_date,
                      SUB.id AS cultivar_id,
                      SUB.cultivar as cultivar,
                      ST.description as description,
                      BB.id AS bbch,
                      P1.date as Phenotype_date
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
                      INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id = ?
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
                      INNER JOIN phenotypes P1 ON P1.id = PP1.phenotype_id AND P1.value_id = 221 AND P1.entity_id = 21
                      INNER JOIN phenotype_bbches PHBB ON PHBB.phenotype_id = P1.id
                      INNER JOIN bbches BB ON BB.id = PHBB.bbch_id
                      AND ((P1.date <= (Base.sample_date + INTERVAL 5 DAY)) AND (P1.date >= (Base.sample_date - INTERVAL 5 DAY))) ",
                     filterData, fetch = TRUE)
  
  odbcClose(connHandle)
  
  return(data)
  dbDisconnect(phenotyper) 
}


##################################################
# 2nd version: for VALDIS trials, no sample date #
##################################################

# SQL script: D:\work\repos\database_scripts\bbch_query_valdis-2015-05-07.sql


func_get_bbch_data2 <- function(experiment_id = c('48656', '51790', '44443', '56726') ){
  
  library(RMySQL)
  phenotyper = dbConnect(MySQL(), user='sprenger', password='heike*rules', dbname='trost_prod', host='cosmos.mpimp-golm.mpg.de')  
  
  data <- dbGetQuery(phenotyper, paste("SELECT 
                      Base.plant_id,
                      Base.plant_name,
                      Base.culture_id,
                      Base.culture_desc,
                      Base.line_id,
                      Base.line_name,
                      Base.line_alias,
                      Base.treatment,
                      ROUND(PH1.number,0) as bbch,
                      PH1.date
                      FROM
                      ( SELECT DISTINCT
                      P.id as plant_id,
                      P.name as plant_name,
                      P.culture_id as culture_id,
                      C.description as culture_desc,
                      P.lineid as line_id,
                      PL.name as line_name,
                      PL.line_alias as line_alias,
                      V.value AS treatment
                      FROM trost_prod.plants P
                      JOIN plantlines PL ON PL.id = P.lineid
                      JOIN cultures C ON C.id = P.culture_id
                      LEFT JOIN phenotype_plants PP ON PP.plant_id = P.id
                      LEFT JOIN phenotypes PH ON PH.id = PP.phenotype_id
                      LEFT JOIN `values` V ON V.id = PH.value_id
                      WHERE culture_id IN (",
                               
                      paste(experiment_id, collapse=",") ,")", 
                               
                      "AND PH.value_id IN (169,170,171,172)
                      ) AS Base 
                      LEFT JOIN phenotype_plants PP1 ON PP1.plant_id = Base.plant_id 
                      INNER JOIN phenotypes PH1 ON PH1.id = PP1.phenotype_id AND PH1.value_id = 221 AND PH1.entity_id = 19;",
                      sep="" ) )
  return(data)
  dbDisconnect(phenotyper) 
}


# solution for parametrization using RODBCext
func_get_bbch_data2_RODBCext <- function(experiment_id = '47199' ){
  
  library(RODBCext)
  connHandle <- odbcConnect("Phenotyper")
  
  filterData <- data.frame(PL.culture_id = experiment_id)
  
  data <- sqlExecute(connHandle, 
                     "SELECT 
                      Base.plant_id,
                      Base.plant_name,
                      Base.culture_id,
                      Base.culture_desc,
                      Base.line_id,
                      Base.line_name,
                      Base.line_alias,
                      Base.treatment,
                      ROUND(PH1.number,0) as bbch,
                      PH1.date
                      FROM
                      ( SELECT DISTINCT
                      P.id as plant_id,
                      P.name as plant_name,
                      P.culture_id as culture_id,
                      C.description as culture_desc,
                      P.lineid as line_id,
                      PL.name as line_name,
                      PL.line_alias as line_alias,
                      V.value AS treatment
                      FROM trost_prod.plants P
                      JOIN plantlines PL ON PL.id = P.lineid
                      JOIN cultures C ON C.id = P.culture_id
                      LEFT JOIN phenotype_plants PP ON PP.plant_id = P.id
                      LEFT JOIN phenotypes PH ON PH.id = PP.phenotype_id
                      LEFT JOIN `values` V ON V.id = PH.value_id
                      WHERE culture_id = ? 
                      AND PH.value_id IN (169,170,171,172)
                      ) AS Base 
                      LEFT JOIN phenotype_plants PP1 ON PP1.plant_id = Base.plant_id 
                      INNER JOIN phenotypes PH1 ON PH1.id = PP1.phenotype_id AND PH1.value_id = 221 AND PH1.entity_id = 19;",
                     filterData, fetch = TRUE)
  
  odbcClose(connHandle)
  
  return(data)
  dbDisconnect(phenotyper) 
}

##################################################
# old version: can only handle one experiment_id #
##################################################

library(RODBC)
phenotyper <- odbcConnect("Phenotyper")

func_get_bbch_data_old <- function(experiment_id){
  
  bbch_query <- paste("SELECT
                      Base.plant_id,
                      Base.experiment_id,
                      Base.experiment_name,
                      Base.treatment,
                      Base.sample_id,
                      Base.sample_date,
                      SUB.id AS cultivar_id,
                      SUB.cultivar as cultivar,
                      ST.description as description,
                      BB.id AS bbch,
                      P1.date as Phenotype_date
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
                      INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (", "\'", experiment_id, "\'", ")
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
  INNER JOIN phenotypes P1 ON P1.id = PP1.phenotype_id AND P1.value_id = 221 AND P1.entity_id = 21
  INNER JOIN phenotype_bbches PHBB ON PHBB.phenotype_id = P1.id
  INNER JOIN bbches BB ON BB.id = PHBB.bbch_id
  AND ((P1.date <= (Base.sample_date + INTERVAL 5 DAY)) AND (P1.date >= (Base.sample_date - INTERVAL 5 DAY))) 
  ")
  
  bbch_query_result <- sqlQuery(phenotyper, bbch_query)
  
  return(bbch_query_result)
}
