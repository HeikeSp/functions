# function to create a vector for specific or common behavior

func_create_type <- function(table, 
                             column1 = "dir_tolerant",
                             column2 = "dir_sensitive",
                             val1 = "tolerant specific", 
                             val2 = "sensitive specific", 
                             val3 = "common"){
  
  table$type <- rep(val1, nrow(table))
  
  # if dir_sensitive is 1 or -1 then set type according
  table$type[ which(table[,column2]==1) ] <- val2
  table$type[ which(table[,column2]==-1) ] <- val2
  
  # if both dir are 1 or -1, then the DGE is common
  table$type[ which( table[,column2]==1 & table[,column1]==1) ] <- val3
  table$type[ which( table[,column2]==-1 & table[,column1]==-1) ] <- val3
  
  return(table)
}
