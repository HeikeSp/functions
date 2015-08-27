# function to concatenate two columns containing strings, but only if they are not identical
# finally order table by MapMan BinCode

func_concatenate_two_columns <- function(dataset, column1, column2, column_result = "BinName"){
  dataset[, column1] <- as.character(dataset[, column1])
  dataset[, column2] <- as.character(dataset[, column2])
  
  # paste column1 and column2 together (only if they are not identical!)
  for(i in 1:nrow(dataset) ){
    if( identical(dataset[i, column1], dataset[i, column2]) ) {
      dataset$column_cat[i] <- dataset[i, column1]
    } else { dataset$column_cat[i] <- paste(dataset[i, column1], dataset[i, column2]) }
  }
  
  # replace "NA " or " NA" by nothing
  dataset$column_cat <- gsub(" NA", "", dataset$column_cat)
  dataset$column_cat <- gsub("NA ", "", dataset$column_cat)
  
  # put BinName column after BinCode column
  #dataset <- dataset[, c(1, "column_cat", 2:15) ]
  
  # rename resulting column (pasted BinNames)
  colnames(dataset)[colnames(dataset) == "column_cat"] <- column_result
  # put BinName column after BinCode column
  
  # change factor to character (otherwise errors)
  BinCodes <- as.character(dataset$Bin)
  # generate empty matrix (9 columns because of many levels for some BinCodes)
  order_matrix <- matrix(data = 0, nrow = length(BinCodes), ncol=9)
  
  # Split the BinCode after each dot into single elements of a list
  for (i in 1:length(BinCodes) ){
    BinCodes_list <- strsplit(BinCodes, "[.]")
    BinCodes_length <- lapply(BinCodes_list, length) # length of each list object
    # assign to each row of the order matrix one BinCode list object (one single BinCode level per column)
    order_matrix[i, (1:BinCodes_length[[i]]) ] <- unlist(strsplit(BinCodes[i], "[.]"))
  }
  
  # change class and colnames
  class(order_matrix) <- "numeric"
  colnames(order_matrix) <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9")

  # bind results of Corto with order matrix (containing separate BinCodes)
  dataset_to_order <- cbind(dataset, order_matrix)
  
  # order dataset by BinCodes
  dataset_ordered <- dataset_to_order[ order(dataset_to_order[,"B1"], dataset_to_order[,"B2"], dataset_to_order[,"B3"], 
                                             dataset_to_order[,"B4"], dataset_to_order[,"B5"], dataset_to_order[,"B6"],
                                             dataset_to_order[,"B7"], dataset_to_order[,"B8"], dataset_to_order[,"B9"]) , ]
  
  return(dataset_ordered)
}