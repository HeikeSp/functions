
func_process_corto <- function(corto_data, threshold = 1) {
    
    # rename 5th column
    colnames(corto_data)[5] <- "adj.pvalue.bonferroni"
  
    # add Benjamini-Hochberg (BH) adjusted pvalues
    corto_data$adj.pvalue.bh <- p.adjust(corto_data$Pvalu, method = "BH")
    
    # transform adjusted pvalue into z-score
    corto_data$z.score.bonferroni <- abs(qnorm(corto_data$adj.pvalue.bonferroni/2))
    corto_data$z.score.bh <- abs(qnorm(corto_data$adj.pvalue.bh/2))
    
    # filter data using adjusted pvalue (BH)
    corto_data_filtered <- subset(corto_data, corto_data$adj.pvalue.bh < threshold)
    
    # droplevels of BinNames
    corto_data_filtered <- droplevels(corto_data_filtered)
    
    return(corto_data_filtered)
}