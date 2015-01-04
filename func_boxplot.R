#===============================================================================
# Name   : Boxplots per analyte using 1 factor / 2 factors
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================

# ONLY for jkitest1 necessary!


# boxplot with 1 factor
func_boxplot_1fac <- function(normalized_values, trial_factors, factor, res_anova_adj, cols, analytes=analytes_6sel_exp_sort){
  for (i in 1:ncol(normalized_values)){
    if(ncol(normalized_values)==109)
      {analytes_6sel_exp_sort_changed <- analytes[-65,]
      boxplot(normalized_values[,i] ~ trial_factors[,factor], 
              ylab="log10-normalized relative amount",
              main = analytes_6sel_exp_sort_changed$name[i], col=cols,
              sub=paste("ANOVA p-value:",res_anova_adj[i,factor]))}
    else
      boxplot(normalized_values[,i] ~ trial_factors[,factor], 
              ylab="log10-normalized relative amount",
              main = analytes$name[i], col=cols,
              sub=paste("ANOVA p-value:",res_anova_adj[i,factor]))
  }
}


########################################################

# boxplot with 2 factors
func_boxplot_2fac <- function(normalized_values, trial_factors, factor1, factor2, res_anova_adj, 
                              cols, names_factors, analytes=analytes_6sel_exp_sort, las_value=2){
  
  for (i in 1:ncol(normalized_values)){
    if(ncol(normalized_values)==109){
      analytes_6sel_exp_sort_changed <- analytes[-65,]
      boxplot(normalized_values[,i] ~ trial_factors[,factor1] * trial_factors[,factor2], 
              ylab="log10-normalized relative amount", las=las_value, names=names_factors,
              main = analytes_6sel_exp_sort_changed$name[i], col=cols)
    }
       
    #       mtext(paste(factor1, "p-value:",res_anova_adj[i,factor1], 
    #                 "\n", factor2, "p-value:",res_anova_adj[i,factor2], 
    #                 "\n interaction p-value:",res_anova_adj[i,3]), side=3)
    
    else{
      boxplot(normalized_values[,i] ~ trial_factors[,factor1] * trial_factors[,factor2], 
              ylab="log10-normalized relative amount", las=las_value, names=names_factors,
              main = analytes$name[i], col=cols)
    }
      mtext(paste(factor1, "p-value:",res_anova_adj[i,factor1], 
                "\n", factor2, "p-value:",res_anova_adj[i,factor2], 
                "\n interaction p-value:",res_anova_adj[i,3]), side=3)
    abline(v=(length(levels(interaction(trial_factors[,factor1], trial_factors[,factor2])))+1) / 2, col="gray")
  }
}


########################################################

# boxplot with 2 factors for single analyte
func_boxplot_2fac_single <- function(normalized_values, trial_factors, factor1, factor2, 
                              cols, names_factors, analytes=analytes_7sel_exp_sort, analyte_name, main_text, x.axis="s",
                              ymin = min(normalized_values[,analyte_row], na.rm=T), 
                              ymax = max(normalized_values[,analyte_row], na.rm=T),las_value=2)
  {
  analyte_row <- which(analytes$name_short == analyte_name)
  boxplot(normalized_values[,analyte_row] ~ trial_factors[,factor1] * trial_factors[,factor2], 
          ylab="log10-normalized relative amount", las=las_value, names=names_factors, xaxt=x.axis,
          main = main_text, col=cols, cex.lab=1.5, cex.axis=1.2, cex.main=1.7,
          ylim = c(ymin, ymax))

    abline(v=(length(levels(interaction(trial_factors[,factor1], trial_factors[,factor2])))+1) / 2, col="gray")
  
}