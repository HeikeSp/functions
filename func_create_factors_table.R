#===============================================================================
# Name   : Create factors table
# Author : Heike Sprenger
# Date   : 2014-07-15
# Version: 0.1
#===============================================================================

# create factors table for treatment, genotype, sample_time, 
# Dw, Fw, Is, AvgAnnotated, AvgAnnotatedLog10, BatchID, SequenceID
func_create_factors_table <- function(trial_matrix, sequence_ids, batch_ids, tolerance_factor) {
  trial_factors <- cbind(trial_matrix[,c("chromatogram","treatment", "genotype_name", "genotype_class", "sample_time",
                                         "sample_id", "Is", "AvgAnnotated", "Dw", "Fw")])
  trial_factors <- cbind(trial_factors, log10(trial_factors$Is), 
                         log10(trial_factors$AvgAnnotated), 
                         sequence_ids, batch_ids, 
                         tolerance_factor)
  colnames(trial_factors)[10:14] <- c("log10_Is", "log10_AvgAnnotated", "SequenceID", "BatchID", "tolerance")
  return(trial_factors)
}

# for field trials: without tolerance factor!
func_create_factors_table_field <- function(trial_matrix, sequence_ids, batch_ids) {
  trial_factors <- cbind(trial_matrix[,c("chromatogram","treatment", "genotype_name", "sample_time",
                                         "sample_id", "Is", "AvgAnnotated", "Dw", "Fw")])
  trial_factors <- cbind(trial_factors, log10(trial_factors$Is), 
                         log10(trial_factors$AvgAnnotated), 
                         sequence_ids, batch_ids)
  colnames(trial_factors)[10:13] <- c("log10_Is", "log10_AvgAnnotated", "SequenceID", "BatchID")
  #if(level)
  trial_factors <- cbind(trial_factors, sample_time_treatment = paste(trial_factors$sample_time, trial_factors$treatment, sep="/"))
  return(trial_factors)
}