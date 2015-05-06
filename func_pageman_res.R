# function to get pageman table

func_pageman_res <- function(res){
  res_dir <- rep(0, nrow(res$merge))
  res_dir[res$merge$logFC > 0 & res$merge$FDR < 0.05] <- 1
  res_dir[res$merge$logFC < 0 & res$merge$FDR < 0.05] <- -1
  res_pageman <- cbind(res$merge[,c(8,2)], dir = res_dir)
  #print(head(res_pageman))
  #print(table(res_dir))
  return(res_pageman)
}