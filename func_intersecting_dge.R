# function to look at intersecting DGE between two sample sets

# structure of DGE set: 
  # rownames are PGSC DMG identifier
  # columns:  
    # (1) logFC
    # (2) logCPM
    # (3) PValue
    # (4) FDR

func_intersecting_dge <- function(set1, set2, lab1, lab2){
  
  intersecting_rownames <- intersect( rownames(set1), rownames(set2) )
  
  intersect_res <- cbind(set1[ rownames(set1) %in% intersecting_rownames, ], 
                         set2[ rownames(set2) %in% intersecting_rownames, ] )
  
  # colnames(	intersect_res) <- c("logFC_field","logCPM_field","PValue_field","FDR_field", "logFC_gh","logCPM_gh","PValue_gh","FDR_gh")
  colnames(intersect_res) <- c( paste(colnames(set1)[1], "_", lab1, sep=""), 
                                paste(colnames(set1)[2], "_", lab1, sep=""),
                                paste(colnames(set1)[3], "_", lab1, sep=""),
                                paste(colnames(set1)[4], "_", lab1, sep=""),
                                paste(colnames(set2)[1], "_", lab2, sep=""),
                                paste(colnames(set2)[2], "_", lab2, sep=""),
                                paste(colnames(set2)[3], "_", lab2, sep=""),
                                paste(colnames(set2)[4], "_", lab2, sep=""))
  
  # mean/sum of logFC (column 1 and 5)
  intersect_res$logFC_mean <- apply(cbind(	intersect_res[,1], intersect_res[,5]), 1 ,mean)
  intersect_res$logFC_sum  <- apply(cbind(	intersect_res[,1], intersect_res[,5]), 1 ,sum)
  
  # merge intersecting DGE with MapMan and PGSC annotation
  intersect_res <- merge(	intersect_res, merge_mapman_pgsc_dmg, by.x="row.names", by.y="pgsc_dmg")
  
  # order table of intersecting DGE by sum of logFC
  intersect_res <- 	intersect_res[order(	intersect_res$logFC_sum),]
  
  return(intersect_res)
  
  # first 25 intersecting DGE (columns: row.names, logFC of set1, logFC of set2, logFC mean, logFC sum, annotations)
  pander(head(	intersect_res[,c(1,2,6,10:13)], n=25))
  
}