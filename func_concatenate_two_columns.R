# the function aims to generate a new column for the "BinName" 
# input for the function is a merged table containing TWO different "BinName" columns originating from two tables
# function to concatenate two columns containing strings (BinName), but only if they are not identical
# if they are identical, then just take one of them (because it doesn't matter)

func_concatenate_two_columns <- function(dataset, column1, column2, column_result = "BinName"){
  
  # transform column1 and 2 into character
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

  # put BinName (last) column after BinCode (first) column, then the remaining columns (2...)
  dataset <- dataset[, c(1 , ncol(dataset) , 2:(ncol(dataset)-1) ) ]
    
  # rename resulting column (pasted BinNames)
  colnames(dataset)[colnames(dataset) == "column_cat"] <- column_result
  
  return(dataset)
}