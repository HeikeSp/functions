#===============================================================================
# Name   : Get matrix with overlapping analytes
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

func_get_overlapping_analytes <- function(analytes_6sel_exp_sort, chromatogram_subset) {
  
  # create an index vector of SELECTED overlapping analytes ONLY!
  analytes_index <- which( colnames(chromatogram_subset) %in% analytes_6sel_exp_sort$analyte)
  print(paste("number of SELECTED overlapping analytes:", length(analytes_index)))
  
  # create values matrix by partial matrix just with SELECTED overlapping analytes
  overlapping_analytes_value_matrix <- chromatogram_subset[, analytes_index]
  print("dim of overlapping analytes value matrix:")
  print(dim(overlapping_analytes_value_matrix))
  return(overlapping_analytes_value_matrix)
}