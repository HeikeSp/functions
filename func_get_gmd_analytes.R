#===============================================================================
# Name   : Get GMD analytes
# Author : Heike Sprenger
# Date   : 2015-11-03
# Version: 0.1
#===============================================================================

# get all GMD analytes which were annotated for trost or valdis experiments
# tf.MSTAnnotation.FK_Analyte is the long ID from GMD (e.g. E449A211-33FC-45F4-B4F4-03CDE1D4F4BF)
# tf.MSTAnnotation.comment is the short ID from GMD (e.g. A140001-101)
# the latter ID can be used to merge this table to another table containing more detailed information

func_get_all_gmd_analytes <- function(){
  gmd_analytes <- sqlQuery(dbhandle, paste("SELECT DISTINCT tf.MSTAnnotation.FK_Analyte, tf.MSTAnnotation.comment 
                                           FROM tf.MSTAnnotation 
                                           INNER JOIN GMD.tf.TagList ON GMD.tf.MSTAnnotation.FK_TagList = GMD.tf.TagList.id
                                           WHERE FK_Analyte is not NULL AND
                                           tf.MSTAnnotation.[Is Quantitative Cluster] = 1 AND
                                           (GMD.tf.TagList.comment = \'trost\' or GMD.tf.TagList.name like \'%trost%\')"))
  
  colnames(gmd_analytes)[2] <- c("analyteID")

  # replace "-xxx" by nothing
  gmd_analytes$analyteID <- gsub("-xxx", "", gmd_analytes$analyteID)

  # remove duplicate rows
  gmd_analytes_unique <- unique(gmd_analytes)
  
  # remove strange analyteIDs
  strange_analyteIDs <- which(gmd_analytes_unique$analyteID %in% c("A142001-21104", "A110001-21104", "A138002-21104",
                                                                   "copied value from tf.SampleInfo 'Internal Standard (interpolated) [normalization data]' provided by AErban"))
  gmd_analytes_unique <- gmd_analytes_unique[-strange_analyteIDs, ]
  
  return(gmd_analytes_unique)
}


# get GMD analytes per experiment_id

func_get_gmd_analytes <- function(experiment_id){
  gmd_analytes <- sqlQuery(dbhandle, paste("SELECT DISTINCT FK_Analyte, comment FROM GMD.tf.MSTAnnotation 
                                           WHERE FK_Analyte is not NULL AND 
                                           tf.MSTAnnotation.[Is Quantitative Cluster] = 1 AND
                                           GMD.tf.MSTAnnotation.FK_TagList=", "\'", experiment_id, "\'", 
                                           sep="") )
  
  colnames(gmd_analytes)[2] <- c("analyteID")
  
  # replace "-xxx" by nothing
  gmd_analytes$analyteID <- gsub("-xxx", "", gmd_analytes$analyteID)
  
  # remove duplicate rows
  gmd_analytes_unique <- unique(gmd_analytes)
  
  # remove strange analyteIDs
  strange_analyteIDs <- which(gmd_analytes_unique$analyteID %in% c("A142001-21104", "A110001-21104", "A138002-21104",
                                                                  "copied value from tf.SampleInfo 'Internal Standard (interpolated) [normalization data]' provided by AErban"))
  if(length(strange_analyteIDs) == 0 ){
  gmd_analytes_unique <- gmd_analytes_unique
  } else gmd_analytes_unique <- gmd_analytes_unique[-strange_analyteIDs, ]
   
  return(gmd_analytes_unique)
}