

func_combine_two_vectors <- function(table = final_greenhouse_up_merged_by_dmg, col1 = "func.x", col2 = "func.y", name_col = "function"){
  res_col = c()
  for (i in 1: nrow(table)){
    if( identical(table[i,col1], table[i,col2])) {
      res_col[i] <- as.character(table[i,col1])
    } else {
      if( is.na(table[i,col1])){
        res_col[i] <- as.character(table[i,col2])
      } else {
        res_col[i] <- as.character(table[i,col1])
      }
    }
  }
  
  res <- cbind(table, res_col)
  
  # change name of last column
  colnames(res)[ncol(res)] <- name_col
  
  # remove duplicate columns which were already combined
  remove_col_idx <- which(colnames(table) %in% c(col1, col2))
  res <- res[,-remove_col_idx]
  
  return(res)
}