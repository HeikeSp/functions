# calculate the median metabolite level for each experiment and metabolite and 
# substract this value from the respective metabolite levels in all samples of an experiment

# input matrix has samples in rows and analytes in columns

func_median_difference <- function(matrix) {
  transformed <- data.frame(matrix(rep(NA, nrow(matrix)*ncol(matrix)), nrow=nrow(matrix)))
  
  median_matrix <- apply(matrix, 2, median, na.rm=TRUE) # column-wise (2) median, so per analyte (over all samples)
  
  for (i  in 1:length(matrix[1,])) { # i refers to columns, here: analytes
    for (j in 1:length(matrix[ ,1])) { # j refers to rows, here: samples
      transformed[j,i] <- matrix[j,i] - median_matrix[i]
    }
  }
  colnames(transformed) <- colnames(matrix)
  rownames(transformed) <- rownames(matrix)
  return(transformed)
}