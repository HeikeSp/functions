# merge logFC and FDR values of 3 tables

func_edgeR_top_down <- function(tab1, tab2, tab3){
  top_merge <- merge(tab1$top_all_down[,c(1,4)], tab2$top_all[,c(1,4)], 
                     by.x = "row.names", by.y="row.names", all.x=T)
  rownames(top_merge) <- top_merge[,1]
  top_merge <- top_merge[,-1]

  top_merge <- merge(top_merge, tab3$top_all[,c(1,4)], 
                     by.x = "row.names", by.y="row.names", all.x=T)
  
  top_merge_sig <- top_merge[top_merge$FDR.x<0.05,]
  rownames(top_merge_sig) <- top_merge_sig[,1]
  top_merge_sig <- top_merge_sig[,-1]
  
  top_merge_sig <- merge(top_merge_sig, assoc_pgsc[,c(1,5)], by.x = "row.names", by.y="pgsc_dmg")
  #rownames(top_merge_sig) <- top_merge_sig[,1]
  #top_merge_sig <- top_merge_sig[,-1]
  
  top_merge_sig <- unique(top_merge_sig)
  
  colnames(top_merge_sig)[2:7] <- c("logFC_all", "FDR_all",
                               "logFC_sens", "FDR_sens",
                               "logFC_tol", "FDR_tol")
  
  print(head(top_merge_sig[order(top_merge_sig$logFC_all, decreasing = F),], n=10))
  return(top_merge_sig)
}

func_edgeR_top_up <- function(tab1, tab2, tab3){
  top_merge <- merge(tab1$top_all_up[,c(1,4)], tab2$top_all[,c(1,4)], 
                     by.x = "row.names", by.y="row.names", all.x=T)
  rownames(top_merge) <- top_merge[,1]
  top_merge <- top_merge[,-1]
  
  top_merge <- merge(top_merge, tab3$top_all[,c(1,4)], 
                     by.x = "row.names", by.y="row.names", all.x=T)
  rownames(top_merge) <- top_merge[,1]
  top_merge <- top_merge[,-1]
  
  top_merge_sig <- top_merge[top_merge$FDR.x<0.05,]
    
  top_merge_sig <- merge(top_merge_sig, assoc_pgsc[,c(1,5)], by.x = "row.names", by.y="pgsc_dmg", all.x=T)
#    rownames(top_merge_sig) <- top_merge_sig[,1]
#    top_merge_sig <- top_merge_sig[,-1]

  top_merge_sig <- unique(top_merge_sig)
  
  colnames(top_merge_sig)[2:7] <- c("logFC_all", "FDR_all",
                                "logFC_sens", "FDR_sens",
                                "logFC_tol", "FDR_tol")
   
  print(head(top_merge_sig[order(top_merge_sig$logFC_all, decreasing = T),], n=10))
  return(top_merge_sig)
}