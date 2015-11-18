#===============================================================================
# Name   : Create GMD analytes table
# Author : Heike Sprenger
# Date   : 2015-11-04
# Version: 0.1
#===============================================================================

# Create a table with annotated analytes combining different experiments
# yes: analyte was annotated in specific experiment
# no: analyte was NOT annotated in specific experiment

func_create_analytes_table <- function(analytes_table, experiments){
  for (i in experiments){
    analytes_table$new_col <- rep("no", nrow(analytes_table))
    new_col_ids <- func_get_gmd_analytes( func_get_experiment_id(i) )
    
    new_col_yes <- which(analytes_table$analyteID %in% new_col_ids$analyteID)
    
    analytes_table$new_col[new_col_yes] <- "yes"
    
    colnames(analytes_table)[which(colnames(analytes_table)=="new_col")] <- i
    
  }
  
  # calculate how often annalyte was annotated
  analytes_table$count_yes <- apply(analytes_table, 1, function(x) sum(x=="yes", na.rm = T))
  
  return(analytes_table)
}