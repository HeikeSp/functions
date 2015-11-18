#===============================================================================
# Name   : Compile subset of metabolite data
# Author : Heike Sprenger
# Date   : 2015-11-05
# Version: 0.1
#===============================================================================

# function to create a specific subset of raw data or processed data
# use a specific selection of samples and analytes
# ROWS: analytes
# COLUMNS: samples

func_compile_subset <- function(raw_data, sample_ids, analyte_ids, analyte_list, sample_list){
  
  row_idx <- which(rownames(raw_data) %in% analyte_ids)
  col_idx <- which(colnames(raw_data) %in% sample_ids)
  
  raw_data_subset <- raw_data[row_idx, col_idx]
  
  # order columns by sample list: experiment, treatment, cultivar
  raw_data_subset_ordered <- raw_data_subset[, order(sample_list$experiment,
                                                     sample_list$treatment,
                                                     sample_list$cultivar)]
  
  # merge subset with further analyte information (ID, name, class, sum formula etc.)
  raw_data_subset_merge <- merge(analyte_list, raw_data_subset_ordered,
                                 by.x = "FK_Analyte", by.y = "row.names")
  
  # remove column with long FK_Analyte ID (first column)
  raw_data_subset_merge <- raw_data_subset_merge[,-1]
  
  # order rows by analyte name
  raw_data_subset_merge_ordered <- raw_data_subset_merge[order(raw_data_subset_merge$Name),]
  
  return(raw_data_subset_merge_ordered)

  }