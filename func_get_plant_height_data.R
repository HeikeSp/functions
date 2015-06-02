library(RODBC)
phenotyper <- odbcConnect("Phenotyper") # Phenoyter is the DSN of the connection

func_get_plant_height_data <- function(){
  

plant_height_query <- paste("SELECT
Base.plant_id,
Base.experiment_id,
                            Base.experiment_name,
                            Base.treatment,
                            Base.sample_id,
                            Base.sample_date,
                            SUB.id AS cultivar_id,
                            SUB.cultivar as cultivar,
                            ST.description as description,
                            P1.number AS height,
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
                            INNER JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (48656, 51790, 44443, 56726)
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
                            LEFT JOIN phenotypes P1 ON P1.id = PP1.phenotype_id AND P1.value_id = 53 AND P1.entity_id IN (19,21)
                            AND ((P1.date <= (Base.sample_date + INTERVAL 5 DAY)) AND (P1.date >= (Base.sample_date - INTERVAL 5 DAY))) 
                            WHERE P1.number IS NOT NULL 
                            ")

plant_height_query_result <- sqlQuery(phenotyper, plant_height_query)
}