#===============================================================================
# Name   : Boxplots per analyte using 1 factor / 2 factors
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================

# ONLY for jkitest1 necessary!


# boxplot with 1 factor
func_boxplot_1fac <- function(normalized_values, trial_factors, factor, res_anova_adj, cols, analyte_names = analytes_sel_exp_sort$Name){
  for (i in 1:ncol(normalized_values)){
    if(ncol(normalized_values)==109)
      {analytes_sel_exp_sort_changed <- analyte_names[-65] # entferne analyte in spalte 65
      boxplot(normalized_values[,i] ~ trial_factors[,factor], 
              ylab = "log10-normalized relative amount",
              main = analytes_sel_exp_sort_changed[i], 
              col = cols,
              sub = paste("ANOVA p-value:",res_anova_adj[i,factor]))
      }
    else
      {if (length(levels(trial_factors[,factor])) > 8) # wenn factor sehr viele levels hat, dann drehe Beschriftung um 90 Grad --> las = 2
        {boxplot(normalized_values[,i] ~ trial_factors[,factor], 
              ylab = "log10-normalized relative amount",
              main = analyte_names[i], 
              col = cols,
              las = 2,
              sub = paste("ANOVA p-value:", res_anova_adj[i,factor]))}
        else
        {boxplot(normalized_values[,i] ~ trial_factors[,factor], 
                 ylab = "log10-normalized relative amount",
                 main = analyte_names[i], 
                 col = cols,
                 sub = paste("ANOVA p-value:", res_anova_adj[i,factor]))}
      }
  }
}


########################################################

# boxplot with 2 factors
func_boxplot_2fac <- function(normalized_values, trial_factors, factor1, factor2, res_anova_adj, 
                              cols, names_factors, analyte_names = analytes_sel_exp_sort$Name, las_value = 2){
  
  for (i in 1:ncol(normalized_values)){
    if(ncol(normalized_values)==109){
      analytes_6sel_exp_sort_changed <- analyte_names[-65]
      boxplot(normalized_values[,i] ~ trial_factors[,factor1] * trial_factors[,factor2], 
              ylab="log10-normalized relative amount", las=las_value, names=names_factors,
              main = analytes_6sel_exp_sort_changed[i], col=cols)
    }
       
    #       mtext(paste(factor1, "p-value:",res_anova_adj[i,factor1], 
    #                 "\n", factor2, "p-value:",res_anova_adj[i,factor2], 
    #                 "\n interaction p-value:",res_anova_adj[i,3]), side=3)
    
    else{
      boxplot(normalized_values[,i] ~ trial_factors[,factor1] * trial_factors[,factor2], 
              ylab="log10-normalized relative amount", las=las_value, names=names_factors,
              main = analyte_names[i], col=cols)
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
  analyte_row <- which(analytes$Name == analyte_name)
  boxplot(normalized_values[,analyte_row] ~ trial_factors[,factor1] * trial_factors[,factor2], 
          ylab="log10-normalized relative amount", las=las_value, names=names_factors, xaxt=x.axis,
          main = main_text, col=cols, cex.lab=1.5, cex.axis=1.2, cex.main=1.7,
          ylim = c(ymin, ymax))

    abline(v=(length(levels(interaction(trial_factors[,factor1], trial_factors[,factor2])))+1) / 2, col="gray")
  
}

########################################################

# simple boxplot for Arabidopsis field metabolite data
func_boxplot_simple <- function(value, title, 
                                ylab_text = "concentration in µmol/g FG"){
  boxplot(value , main = title, 
          ylab = ylab_text, 
          cex.main = 1.5, 
          cex.axis = 1.3, 
          cex.lab = 1.3, 
          col = "#9ACD32")
}