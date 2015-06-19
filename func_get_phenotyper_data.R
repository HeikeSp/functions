#===============================================================================
# Name   : Get Phenotyter data 
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
# Aim    : Get Phenotyper data by master_query and meta_query
#===============================================================================

# master query
func_use_master_query <- function(){
  master_query_result <- sqlQuery(phenotyper, paste("
-- Field Query: MPIFeld2011-2
    SELECT 
      AQ.gmd_id AS GMD_ID,
      ASA.aliquot_id AS aliquot_id, 
      P1.number AS DWx_g,
      V.attribute AS DW_source,
      P2.number AS tara_g,
      P3.number AS FWx_g,
      fwdw_ratio_227(P1.number, P2.number, P3.number) AS FW_DW,
      A.amount,
      A.amount / fwdw_ratio_227(P1.number, P2.number, P3.number) AS DWcalc,
      U.name AS DWcalc_unit,
      PL.culture_id AS culture,
      ((1 <= fwdw_ratio_227(P1.number, P2.number, P3.number)) AND (fwdw_ratio_227(P1.number, P2.number, P3.number) <= 15)) AS sanity_ok
    FROM 
      aliquot_query AQ
      LEFT OUTER JOIN aliquot_samples ASA ON AQ.id = ASA.aliquot_id
      LEFT OUTER JOIN aliquot_plants AP ON AP.aliquot_id = AQ.id
      JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (44443, 56726)
      LEFT OUTER JOIN phenotype_samples PS1 ON PS1.sample_id = ASA.sample_id
      LEFT OUTER JOIN phenotypes P1 ON P1.id = PS1.phenotype_id AND P1.value_id = 227 AND P1.invalid IS NULL 
      LEFT OUTER JOIN phenotype_samples PS2 ON PS2.sample_id = ASA.sample_id
      LEFT OUTER JOIN phenotypes P2 ON P2.id = PS2.phenotype_id AND P2.value_id = 163 AND P2.invalid IS NULL
      LEFT OUTER JOIN phenotype_samples PS3 ON PS3.sample_id = ASA.sample_id
      LEFT OUTER JOIN phenotypes P3 ON P3.id = PS3.phenotype_id AND P3.value_id = 164 AND P3.invalid IS NULL  
      LEFT OUTER JOIN aliquots A ON A.id = ASA.aliquot_id
      LEFT OUTER JOIN units U ON A.amount_unit = U.id
      LEFT OUTER JOIN `values` V ON V.id = P1.value_id
    WHERE
      ((P1.date >= P3.date + INTERVAL 1 DAY) AND 
      (P1.date <= P3.date + INTERVAL 7 DAY)) AND 
      ((P2.date >= P3.date - INTERVAL 10 DAY) AND
      (P2.date <= P3.date - INTERVAL 1 DAY))
    UNION ALL 
-- Blue Query: MPITest1.2, MPITest2, Pruef1-3
    SELECT 
      AQ.gmd_id AS GMD_ID,
      AQ.id AS aliquot_id,
      P2.number AS DWx_g,
      V.attribute AS DW_source,
      NULL AS tara_g,
      AVG(P1.number) AS FWx_g,
      AVG((P1.number / P2.number)) AS FW_DW,
      A.amount,
      A.amount / AVG((P1.number / P2.number)) AS DWcalc,
      U.name AS DWcalc_unit,
      PL.culture_id AS culture,
      AVG(((1 <= (P1.number / P2.number)) AND ((P1.number / P2.number) <= 15))) AS sanity_ok
    FROM
      aliquot_query AQ
      LEFT OUTER JOIN aliquots A ON AQ.id = A.id
      LEFT OUTER JOIN aliquot_plants AP ON AP.aliquot_id = A.id
      JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (48656,51790,56575,58243,60319)
      LEFT OUTER JOIN phenotype_plants PP1 ON PP1.plant_id = PL.id
      LEFT OUTER JOIN phenotypes P1 ON P1.id = PP1.phenotype_id AND P1.value_id = 55 AND P1.entity_id = 366 AND P1.invalid IS NULL AND A.sample_date = P1.date 
      LEFT OUTER JOIN phenotype_plants PP2 ON PP2.plant_id = PL.id
      LEFT OUTER JOIN phenotypes P2 ON P2.id = PP2.phenotype_id AND P2.value_id = 69 AND P2.entity_id = 366 AND P2.invalid IS NULL AND ((P2.date <= (P1.date + INTERVAL 7 DAY)) AND (P2.date >= (P1.date + INTERVAL 1 DAY))) 
      JOIN units U ON A.amount_unit = U.id
      JOIN `values` V ON V.id = P1.value_id
      GROUP BY A.id
      UNION ALL
-- Red/Green Query JKI-GWH1/2, JKI-Shelter2011/2, Feld JKI 2012/3, Field trial Dethlingen 2011
    SELECT
      AQ.gmd_id AS GMD_ID,
      ASA.aliquot_id AS aliquot_id, 
      P1.number AS DWx_g,
      V.attribute AS DW_source,
      AVG(P2.number) AS tara_g,
      AVG(P3.number) AS FWx_g,
      AVG(fwdw_ratio_227(P1.number, P2.number, P3.number)) AS FW_DW,
      A.amount,
      A.amount / AVG(fwdw_ratio_227(P1.number, P2.number, P3.number)) AS DWcalc,
      U.name AS DWcalc_unit,
      PL.culture_id AS culture,
      (((1 <= AVG(fwdw_ratio_227(P1.number, P2.number, P3.number)))) AND (AVG(fwdw_ratio_227(P1.number, P2.number, P3.number)) <= 15)) AS sanity_ok
    FROM 
      aliquot_query AQ 
      LEFT OUTER JOIN aliquot_samples ASA ON AQ.id = ASA.aliquot_id
      LEFT OUTER JOIN aliquot_plants AP ON AP.aliquot_id = AQ.id 
                                                  JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (45985,57802,45990,57803,56875,46150,62327)
                                                  LEFT OUTER JOIN phenotype_samples PS1 ON PS1.sample_id = ASA.sample_id 
                                                  LEFT OUTER JOIN phenotypes P1 ON P1.id = PS1.phenotype_id AND P1.value_id = 227 AND P1.invalid IS NULL
                                                  LEFT OUTER JOIN phenotype_samples PS2 ON PS2.sample_id = ASA.sample_id 
                                                  LEFT OUTER JOIN phenotypes P2 ON P2.id = PS2.phenotype_id AND P2.value_id = 163 AND P2.invalid IS NULL
                                                  LEFT OUTER JOIN phenotype_samples PS3 ON PS3.sample_id = ASA.sample_id
                                                  LEFT OUTER JOIN phenotypes P3 ON P3.id = PS3.phenotype_id AND P3.value_id = 164 AND P3.invalid IS NULL
                                                  LEFT OUTER JOIN aliquots A ON A.id = ASA.aliquot_id
                                                  LEFT OUTER JOIN units U ON A.amount_unit = U.id 
                                                  LEFT OUTER JOIN `values` V ON V.id = P1.value_id 
                                                  WHERE 
                                                  ((P1.date >= P3.date + INTERVAL 0 DAY) AND
                                                  (P1.date <= P3.date + INTERVAL 7 DAY)) AND
                                                  (P2.date <= P3.date - INTERVAL 0 DAY) 
                                                  GROUP BY A.id
                                                  UNION ALL
                                                  -- Cherry Query: Breeders 2011/2
                                                  SELECT 
                                                  AQ.gmd_id AS GMD_ID,
                                                  A.id AS aliquot_id,
                                                  P1.number AS DWx_g,
                                                  V.attribute AS DW_source,
                                                  P2.number AS tara_g,
                                                  P3.number AS FWx_g,
                                                  P1.number = P3.number AS wrong_index,
                                                  A.amount AS amount,
                                                  A.amount / fwdw_ratio_227(P1.number, P2.number, P3.number) AS DWcalc,
                                                  U.name AS DWcalc_unit,
                                                  PL.culture_id AS culture,
                                                  (((1 <= AVG(fwdw_ratio_227(P1.number, P2.number, P3.number)))) AND (AVG(fwdw_ratio_227(P1.number, P2.number, P3.number)) <= 15)) AS sanity_ok
                                                  FROM 
                                                  aliquots A
                                                  JOIN aliquot_query AQ ON A.id = AQ.id 
                                                  LEFT OUTER JOIN aliquot_samples ASA ON ASA.aliquot_id = A.id
                                                  LEFT OUTER JOIN aliquot_plants AP ON AP.aliquot_id = A.id
                                                  JOIN plants PL ON PL.id = AP.plant_id AND PL.culture_id IN (47107, 47109, 47110, 47111, 47112, 47114, 47115, 47117, 56878, 56879, 56880, 56881, 56882, 56883, 56884, 56876)
                                                  LEFT OUTER JOIN cultures C ON C.id = PL.culture_id
                                                  LEFT OUTER JOIN units U ON A.amount_unit = U.id
                                                  LEFT OUTER JOIN phenotype_samples PS1 ON PS1.sample_id = ASA.sample_id
                                                  LEFT OUTER JOIN phenotypes P1 ON P1.id = PS1.phenotype_id AND P1.value_id = 227 AND P1.invalid IS NULL
                                                  LEFT OUTER JOIN phenotype_samples PS2 ON PS2.sample_id = ASA.sample_id
                                                  LEFT OUTER JOIN phenotypes P2 ON P2.id = PS2.phenotype_id AND P2.value_id = 163 AND P2.invalid IS NULL
                                                  LEFT OUTER JOIN phenotype_samples PS3 ON PS3.sample_id = ASA.sample_id
                                                  LEFT OUTER JOIN phenotypes P3 ON P3.id = PS3.phenotype_id AND P3.value_id = 164 AND P3.invalid IS NULL
                                                  LEFT OUTER JOIN `values` V ON V.id = P1.value_id
                                                  WHERE
                                                  ((P1.date >= P3.date + INTERVAL 0 DAY) AND
                                                  (P1.date <= P3.date + INTERVAL 7 DAY)) AND
                                                  (P2.date <= P3.date - INTERVAL 0 DAY)
                                                  GROUP BY GMD_ID
                                                  
                                                  ORDER BY culture, sanity_ok;                      
                                                  "))
  
  colnames(master_query_result)[1] <- "GMD_id"
  
  return(master_query_result)  
  
}
 


# meta query
func_use_meta_query <- function(){
  meta_query_result <- sqlQuery(phenotyper, paste("SELECT DISTINCT 
AQ.gmd_id AS GMD_id,
AQ.id AS aliquot_id, 
                                                A.amount AS amount,
                                                C.id AS experiment_id, 
                                                C.description AS experiment_name,
                                                C.location_id AS location_id, 
                                                L.name AS location,
                                                SUB.id AS cultivar_id,
                                                SUB.cultivar AS cultivar, 
                                                V.value AS treatment,
                                                STQ.sampletime AS sample_time,
                                                STQ.sampleloc AS sample_loc,
                                                ASA.sample_id AS sample_id
                                                FROM 
                                                aliquot_query AQ 
                                                LEFT OUTER JOIN aliquots A ON A.id = AQ.id 
                                                LEFT OUTER JOIN aliquot_plants AP ON AP.aliquot_id = AQ.id 
                                                LEFT OUTER JOIN plants PL ON PL.id = AP.plant_id 
                                                LEFT OUTER JOIN cultures C ON C.id = PL.culture_id 
                                                LEFT OUTER JOIN phenotype_plants PP ON PP.plant_id = PL.id 
                                                LEFT OUTER JOIN phenotypes P ON P.id = PP.phenotype_id 
                                                LEFT OUTER JOIN `values` V ON V.id = P.value_id 
                                                LEFT OUTER JOIN subspecies AS SUB ON SUB.id = PL.subspecies_id 
                                                LEFT OUTER JOIN sampletime_query AS STQ ON STQ.gmd_id = AQ.gmd_id
                                                LEFT OUTER JOIN locations L ON L.id = C.location_id
                                                LEFT OUTER JOIN aliquot_samples ASA ON ASA.aliquot_id = AQ.id
                                                WHERE 
                                                P.value_id IN (169,170,171,172)
                                                UNION ALL
                                                SELECT DISTINCT 
                                                AQ.gmd_id AS GMD_id,
                                                AQ.id AS aliquot_id, 
                                                NULL AS amount, 
                                                C.id AS experiment_id, 
                                                C.description AS experiment_name,
                                                C.location_id AS location_id, 
                                                L.name AS location,
                                                SUB.id AS cultivar_id,
                                                SUB.cultivar AS cultivar, 
                                                V.value AS treatment,
                                                STQ.sampletime AS sample_time,
                                                STQ.sampleloc AS sample_loc,
                                                ASA.sample_id AS sample_id
                                                FROM 
                                                aliquot_query AQ 
                                                LEFT OUTER JOIN plants PL ON PL.id = AQ.id
                                                LEFT OUTER JOIN cultures C ON C.id = PL.culture_id 
                                                LEFT OUTER JOIN phenotype_plants PP ON PP.plant_id = PL.id 
                                                LEFT OUTER JOIN phenotypes P ON P.id = PP.phenotype_id 
                                                LEFT OUTER JOIN `values` V ON V.id = P.value_id 
                                                LEFT OUTER JOIN subspecies AS SUB ON SUB.id = PL.subspecies_id 
                                                LEFT OUTER JOIN sampletime_query AS STQ ON STQ.gmd_id = AQ.gmd_id
                                                LEFT OUTER JOIN locations L ON L.id = C.location_id
                                                LEFT OUTER JOIN aliquot_samples ASA ON ASA.aliquot_id = AQ.id
                                                WHERE 
                                                P.value_id IN (169,170,171,172)
                                                ORDER BY GMD_id;
                                                "))
  
  return(meta_query_result)  
  
  }

