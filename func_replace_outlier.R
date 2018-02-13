# is applied to all values per metabolite (iterative over all metabolites)
# will be continued as long the p-value of grubbs test is below threshold
# outlier is replaced by NA
# values matrix contains metbolites in columns, samples in rows

func_replace_outlier <- function(values, threshold, original_values, output="original"){
  idx_outlier_col = 1
  
  while(length(idx_outlier_col)>0){
    
    outlier_pvalue <- rep(NA, ncol(values))
    outlier_name <- rep(NA, ncol(values))
    
    for (i in 1:ncol(values)){
      outlier_pvalue[i] <- find_one_outlier(values[,i], output="p.value")
      outlier_name[i] <- find_one_outlier(values[,i], output="name")
    }
    
    idx_outlier_col <- which(outlier_pvalue < threshold)
    
    # replace outliers by NA
    for (i in idx_outlier_col){
      values[outlier_name[i], i] <- NA
    }
    
    for (i in idx_outlier_col){
      original_values[outlier_name[i], i] <- NA
    }
    
  }
  
  if (output=="original")
    return(original_values)
  else 
    return(values)
}


func_replace_outlier2 <- function(data, threshold){
  
  check <- 1
  grp_vals <- levels(data$timing_tissue_treatment)
  
  while(check > 0){
    
    outlier_name <- matrix(NA, nrow = length(grp_vals), ncol = ncol(data))
    outlier_pvalue <- matrix(NA, nrow = length(grp_vals), ncol = ncol(data))
    
    for(j in 1:length(grp_vals)){
      
      val <- grp_vals[j]
      data_sub <- droplevels(subset(data, timing_tissue_treatment == val))
    
      for (i in 2:ncol(data_sub)){
        x <- data_sub[,i]
        names(x) <- rownames(data_sub)
        
        outlier_pvalue[j,i] <- find_one_outlier(x, output = "p.value")
        outlier_name[j,i] <- find_one_outlier(x, output = "name")
        }
      }
    
    outlier_name[,1] <- outlier_pvalue[,1] <- grp_vals
    colnames(outlier_name) <- colnames(outlier_pvalue) <- colnames(data)
    
    outlier_pvalue <- data.frame(timing_tissue_treatment = outlier_pvalue[,1],
                                 apply(outlier_pvalue[,-1], 2, as.numeric), check.names = F)
    
    # melt grubbs test results
    outlier_pvalue_m <- melt(outlier_pvalue)
    outlier_name_m <- melt(data.frame(outlier_name, check.names = F), id = "timing_tissue_treatment")

    outlier_pvalue_name <- merge(outlier_name_m, outlier_pvalue_m,
                                 by = c("timing_tissue_treatment", "variable"))
    colnames(outlier_pvalue_name)[3:4] <- c("sampleID","p_value")
    outlier_pvalue_name$p_adj <- p.adjust(outlier_pvalue_name$p_value, method = "BH")
    
    outlier_pvalue_name_sig <- droplevels(subset(outlier_pvalue_name, p_adj < threshold))
    
    check <- nrow(outlier_pvalue_name_sig)

    # replace outliers by NA
    if(check > 0){
      for(z in 1:check){
        a <- as.character(outlier_pvalue_name_sig$sampleID[z]) # row
        b <- as.character(outlier_pvalue_name_sig$variable[z]) # column
        data[a,b] <- NA
      }
    }
  }
  
  return(data)
}
