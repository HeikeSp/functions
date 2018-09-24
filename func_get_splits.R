# function to split a set of indexes in n parts

func_get_splits <- function(seq, num){
  # seq is an array of indexes 
  # num is the number of folds you want to use (in the outer loop of the nested cross validation)
  avg <- length(seq) / num
  out <- matrix(nrow = num, ncol = avg)
  last <- 1
  row_num <- 1
  
  while(last < length(seq)){
    out[row_num,] <- seq[last:(last+avg-1)]
    
    last <- last + avg
    row_num <- row_num + 1
  }
  
  return(out)
}