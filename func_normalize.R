#===============================================================================
# Name   : ANOVA-Normalization: Apply RemoveFactors function to values table
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================


# TODO: please test, if col/row renaming also works with 
# overlapped_analytes_sample_subset_log10_values = jkitest1_values_cast_select_log10[,-65]

# this applies source("RemoveFactors_function.R") with the following parameters:
#
# overlapped_analytes_sample_subset_log10_values : data to normalize (numeric + in same order as trial_factors)
# trial_factors : dataframe containing the factors/numerical vars for ANOVA model
# facs : all factors to be incorporated in the model in the desired order
# keep : all factors to be retained in the normalized data

func_normalize <- function(overlapped_analytes_sample_subset_log10_values, trial_factors,
                           facs=c("cultivar", "treatment", "sample_time", "SequenceID", "BatchID", "log10_AvgAnnotated"),
                           keep=c("cultivar", "treatment", "sample_time")) {
  normalized_values <- apply(overlapped_analytes_sample_subset_log10_values, 2, RemoveFactors,
                             sam=trial_factors, facs=facs, keep=keep)
  colnames(normalized_values) <- colnames(overlapped_analytes_sample_subset_log10_values)
  rownames(normalized_values) <- rownames(overlapped_analytes_sample_subset_log10_values)
  print("normalized values dim:")
  print(dim(normalized_values))
  print("first 3 rows/cols:")
  print(normalized_values[1:3,1:3])
  return(normalized_values)
}