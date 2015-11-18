#===============================================================================
# Name   : Perform ANOVA for 2/3 factors (with/without interaction)
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================

# using 3 factors (without interaction)

func_anova_3fac <- function(normalized_values, trial_factors, factor1, factor2, factor3, threshold, analyte_names=analytes_sel_exp_sort$Name){
  res_anova <- matrix("NA", ncol=4, nrow=ncol(normalized_values))
  for (i in 1:ncol(normalized_values)){
    res_anova[i,] <- summary(aov(normalized_values[,i] ~ 
                                   trial_factors[,factor1] + trial_factors[,factor2] + trial_factors[,factor3]))[[1]][["Pr(>F)"]] 
  }
  res_anova <- res_anova[,-4]
  colnames(res_anova) <- c(factor1, factor2, factor3)
  res_anova <- apply(res_anova, 2, as.numeric)
  
  res_anova_adj <- apply(res_anova, 2, p.adjust, method="BH")
  res_anova_adj <- signif(res_anova_adj,3)  
  
  print("dimensions of ANOVA results table:")
  print(dim(res_anova_adj))
  
  print(paste("number of analytes with signifcant effect for", factor1, ", threshold:", threshold))
  print(length(which(res_anova_adj[,1] < threshold)))
  print(paste("number of analytes with signifcant effect for", factor2, ", threshold:", threshold))
  print(length(which(res_anova_adj[,2] < threshold)))
  print(paste("number of analytes with signifcant effect for", factor3, ", threshold:", threshold))
  print(length(which(res_anova_adj[,3] < threshold)))
  
#   if(ncol(normalized_values)==109) # --> jki test1
#     rownames(res_anova_adj) <- analytes$name[-65]
#   else
#     if(ncol(normalized_values)==106) # --> mpi feld 2012
#       rownames(res_anova_adj) <- analytes$name[-c(32,26,73,53)]
#     else
#       if(ncol(normalized_values)==108) # --> jki feld 2012
#         rownames(res_anova_adj) <- analytes$name[-c(99,65)]
#     else  
  rownames(res_anova_adj) <- analyte_names
  
  print(head(res_anova_adj))
  return(res_anova_adj)
}

#######################################################################

# using 2 factors (without interaction)

func_anova_2fac <- function(normalized_values, trial_factors, factor1, factor2, threshold, analyte_names=analytes_sel_exp_sort$Name){
  res_anova <- matrix("NA", ncol = 3, nrow = ncol(normalized_values))
  for (i in 1:ncol(normalized_values)){
    res_anova[i,] <- summary(aov(normalized_values[,i] ~ 
                                   trial_factors[,factor1] + trial_factors[,factor2]))[[1]][["Pr(>F)"]] 
  }
  res_anova <- res_anova[,-3]
  colnames(res_anova) <- c(factor1, factor2)
  res_anova <- apply(res_anova, 2, as.numeric)
  
  res_anova_adj <- apply(res_anova, 2, p.adjust, method="BH")
  res_anova_adj <- signif(res_anova_adj, 3)  
  
  print("dimensions of ANOVA results table:")
  print(dim(res_anova_adj))
  
  print(paste("number of analytes with signifcant effect for", factor1, ", threshold:", threshold))
  print(length(which(res_anova_adj[,1] < threshold)))
  print(paste("number of analytes with signifcant effect for", factor2, ", threshold:", threshold))
  print(length(which(res_anova_adj[,2] < threshold)))

  #   if(ncol(normalized_values)==109) # --> jki test1
  #     rownames(res_anova_adj) <- analytes$name[-65]
  #   else
  #     if(ncol(normalized_values)==106) # --> mpi feld 2012
  #       rownames(res_anova_adj) <- analytes$name[-c(32,26,73,53)]
  #     else
  #       if(ncol(normalized_values)==108) # --> jki feld 2012
  #         rownames(res_anova_adj) <- analytes$name[-c(99,65)]
  #     else  
  rownames(res_anova_adj) <- analyte_names
  
  print(head(res_anova_adj))
  return(res_anova_adj)
}

#######################################################################

# using 2 factors (with interaction)

func_anova_2fac_ia <- function(normalized_values, trial_factors, factor1, factor2, threshold, analyte_names=analytes_sel_exp_sort$Name){
  res_anova <- matrix("NA", ncol=4, nrow=ncol(normalized_values))
  for (i in 1:ncol(normalized_values)){
    res_anova[i,] <- summary(aov(normalized_values[,i] ~ 
                                   trial_factors[,factor1] * trial_factors[,factor2]))[[1]][["Pr(>F)"]] 
  }
  res_anova <- res_anova[,-4]
  colnames(res_anova) <- c(factor1, factor2, "interaction")
  res_anova <- apply(res_anova, 2, as.numeric)
  
  res_anova_adj <- apply(res_anova, 2, p.adjust, method="BH")
  res_anova_adj <- signif(res_anova_adj,3)  
  
  print("dimensions of ANOVA results table:")
  print(dim(res_anova_adj))
  
  print(paste("number of analytes with signifcant effect for", factor1, ", threshold:", threshold))
  print(length(which(res_anova_adj[,1] < threshold)))
  print(paste("number of analytes with signifcant effect for", factor2, ", threshold:", threshold))
  print(length(which(res_anova_adj[,2] < threshold)))
  print(paste("number of analytes with signifcant effect for interaction, threshold:", threshold))
  print(length(which(res_anova_adj[,3] < threshold)))
  
#   if(ncol(normalized_values)==109) # --> jki test1
#     rownames(res_anova_adj) <- analytes$name[-65]
#   else
#     if(ncol(normalized_values)==106) # --> mpi feld 2012
#       rownames(res_anova_adj) <- analytes$name[-c(32,26,73,53)]
#   else
#     if(ncol(normalized_values)==108) # --> jki feld 2012
#       rownames(res_anova_adj) <- analytes$name[-c(99,65)]
#   else
  rownames(res_anova_adj) <- analyte_names
  
  print(head(res_anova_adj))
  return(res_anova_adj)
}


#######################################################################

# using 1 factor

func_anova_1fac <- function(normalized_values, trial_factors, factor1, threshold, analyte_names=analytes_sel_exp_sort$Name){
  res_anova <- matrix("NA", ncol=2, nrow=ncol(normalized_values))
  for (i in 1:ncol(normalized_values)){
    res_anova[i,] <- summary(aov(normalized_values[,i] ~ trial_factors[,factor1]))[[1]][["Pr(>F)"]] 
  }
  res_anova <- res_anova[,-2]
  #colnames(res_anova) <- c(factor1)
  res_anova <- as.numeric(res_anova)
  
  res_anova_adj <- p.adjust(res_anova, method="BH")
  res_anova_adj <- signif(res_anova_adj,3)  
  
  print("length of ANOVA results vector:")
  print(length(res_anova_adj))
  
  print(paste("number of analytes with signifcant effect for", factor1, ", threshold:", threshold))
  print(length(which(res_anova_adj < threshold)))
  
  #print(head(res_anova_adj))
  return(res_anova_adj)
}