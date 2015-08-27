# function to create a final list of DGE by merging edgeR results with MapMan associations

func_final_DGE_list <- function(res, dir_value, FDR_threshold = 0.05) {
  
  # create dir for up/down-regulation
  res_dir <- rep(0, nrow(res$merge)) # no significant change of gene expression: 0
  res_dir[res$merge$logFC > 0 & res$merge$FDR < FDR_threshold] <- 1
  res_dir[res$merge$logFC < 0 & res$merge$FDR < FDR_threshold] <- -1
  
  # column 8: pgsc_dmp
  # column 1: pgsc_dmg
  # column 2: logFC
  # column 5: FDR
  # column 9: function
  
  # combine part of the DGE results with direction vector (-1, 0 or 1)
  res_part <- cbind(res$merge[,c(8,1,2,5,9)], dir = res_dir)
  
  # filter results for significant DGE of one direction (1 or -1)
  res_part_dir <- subset(res_part, res_part$dir == dir_value)
  
  # merge part of the DGE results with MapMan associations
  res_merge <- merge(res_part_dir, assoc_mapman[,-5], by.x = "pgsc_dmp", by.y = "IDENTIFIER")
  
  colnames(res_merge)[2] <- "pgsc_dmg"
  
  return(res_merge)
}