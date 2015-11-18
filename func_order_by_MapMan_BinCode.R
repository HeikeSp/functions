# function to order a table by MapMan BinCode

# THE OLD CODE BELOW IS WAY TOO COMPLICATED!
# instead use: naturalorder from naturalsort package

library(naturalsort)

func_order_by_MapMan_BinCode <- function(dataset, BinCode_column) {
  
  dataset_ordered <- dataset[ naturalorder(dataset [ , BinCode_column] ) , ]
  
  return(dataset_ordered)
}


#####################################################################################

# function to order a table by MapMan BinCode
# thus, use an split Bincode into separate columns for every level

func_order_by_MapMan_BinCode_old <- function(dataset, BinCode_column, remove_order_matrix = TRUE){
  
  # change factor to character (otherwise errors)
  BinCodes <- as.character(dataset[, BinCode_column])
  
  # generate empty matrix (7 columns because of many levels for some BinCodes)
  order_matrix <- matrix(data = 0, nrow = length(BinCodes), ncol=7)
  
  # Split the BinCode after each dot into single elements of a list
  for (i in 1:length(BinCodes) ){
    BinCodes_list <- strsplit(BinCodes, "[.]")
    BinCodes_length <- lapply(BinCodes_list, length) # length of each list object
    # assign to each row of the order matrix one BinCode list object (one single BinCode level per column)
    order_matrix[i, (1:BinCodes_length[[i]]) ] <- unlist(BinCodes_list[i])
  }
  
  # change class and colnames
  class(order_matrix) <- "numeric"
  colnames(order_matrix) <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7")
  
  # bind results of Corto with order matrix (containing separate BinCodes)
  dataset_to_order <- cbind(dataset, order_matrix)
  
  # order dataset by BinCodes
  dataset_ordered <- dataset_to_order[ order(dataset_to_order[,"B1"], dataset_to_order[,"B2"], 
                                             dataset_to_order[,"B3"], dataset_to_order[,"B4"], 
                                             dataset_to_order[,"B5"], dataset_to_order[,"B6"],
                                             dataset_to_order[,"B7"]) , ]
  
  # remove columns from order matrix after ordering is finished
  if(remove_order_matrix == TRUE) {
    dataset_ordered <- subset(dataset_ordered, select = -c(B1, B2, B3, B4, B5, B6, B7))
    } else { dataset_ordered <- dataset_ordered }
  
  return(dataset_ordered)
}

