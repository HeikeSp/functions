#===============================================================================
# Name   : Replace NAs with minimal value
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

func_replace_na_with_min_value <- function(matrix) {
  # replace NAs with lowest value of dataset
  matrix_without_NA <- matrix
  matrix_without_NA[is.na(matrix)] <- min(matrix, na.rm=TRUE)
  return(matrix_without_NA)
}