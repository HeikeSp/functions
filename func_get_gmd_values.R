#===============================================================================
# Name   : Get GMD intensity values
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

func_get_gmd_values <- function(experiment_id){
  gmd_values <- sqlQuery(dbhandle, paste("SELECT FK_Analyte, GMD.tf.IntensityValue.FK_chromatogram, SUM(value) FROM GMD.tf.MSTAnnotation 
                       INNER JOIN GMD.tf.IntensityValue ON GMD.tf.MSTAnnotation.FK_MST = GMD.tf.IntensityValue.FK_MST and 
                                         GMD.tf.MSTAnnotation.FK_TagList = GMD.tf.IntensityValue.FK_TagList
                                         INNER JOIN GMD.dbo.GC_Chromatogram ON GMD.tf.IntensityValue.FK_chromatogram = GMD.dbo.GC_Chromatogram.id
                                         WHERE GMD.tf.MSTAnnotation.FK_TagList=", "\'", experiment_id, "\'", "and
                                         FK_Analyte is not NULL and
                                         GMD.tf.MSTAnnotation.[Is Quantitative Cluster] = 1
                                         GROUP BY FK_Analyte, FK_chromatogram", sep="") )
  
  colnames(gmd_values)[3] <- "value"
  
  # Reshape the values table to get a matrix with the analytes in columns and chromatograms (= samples) in rows.
  library(reshape)
  gmd_values_cast <- cast(gmd_values, FK_chromatogram ~ FK_Analyte, value="value")
  gmd_values_cast <- as.data.frame(gmd_values_cast)
  
  # change rownames and remove first column with redundant rownames 
  rownames(gmd_values_cast) <- gmd_values_cast$FK_chromatogram
  gmd_values_cast <- gmd_values_cast[, -1]
  
  return(gmd_values_cast)
}