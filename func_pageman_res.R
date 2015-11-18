# function to get pageman table

func_pageman_res <- function(res){
  # create dir for up/down-regulation
  res_dir <- rep(0, nrow(res$merge))
  
  # res_dir is assigned +1 if logFC is SIGNIFICANTLY POSITIVE
  res_dir[res$merge$logFC > 0 & res$merge$FDR < 0.05] <- 1
  
  # res_dir is assigned -1 if logFC is SIGNIFICANTLY NEGATIVE
  res_dir[res$merge$logFC < 0 & res$merge$FDR < 0.05] <- -1
  
  # column 8: pgsc_dmp
  # column 2: logFC
  res_pageman <- cbind(res$merge[,c(8,2)], dir = res_dir)
  
  #print(head(res_pageman))
  #print(table(res_dir))
  
  return(res_pageman)
}


# version including also DMG identifier:

func_pageman_res_dmg <- function(res){
  # create dir for up/down-regulation
  res_dir <- rep(0, nrow(res$merge))
  
  # res_dir is assigned +1 if logFC is SIGNIFICANTLY POSITIVE
  res_dir[res$merge$logFC > 0 & res$merge$FDR < 0.05] <- 1
  
  # res_dir is assigned -1 if logFC is SIGNIFICANTLY NEGATIVE
  res_dir[res$merge$logFC < 0 & res$merge$FDR < 0.05] <- -1
  
  # column 1: pgsc_dmg (rownames)
  # column 8: pgsc_dmp
  # column 2: logFC
  res_pageman <- cbind(res$merge[,c(1,8,2)], dir = res_dir)
  colnames(res_pageman)[1] <- "pgsc_dmg"
  
  #print(head(res_pageman))
  #print(table(res_dir))
  
  return(res_pageman)
}