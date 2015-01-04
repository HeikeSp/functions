#===============================================================================
# Name   : Boxplots per analyte, sorted by median of genotype (= cultivar = line)
# Author : Heike Sprenger
# Date   : 2014-12-19
# Version: 0.1
#===============================================================================

# matrix = all_valdis_merge_IIIc_only_valdis
# factor = valdis_factors$cultivar


# boxplot with 1 factor
func_boxplot_sorted <- function(matrix, factor, analytes){
  for (i in 1:ncol(matrix)){
    
    # subset of matrix per analyte
    matrix_df <- data.frame(value = matrix[, i], genotype = factor)
    
    # sort genotypes according to median for boxplot
    matrix_df_sorted <- with(matrix_df, reorder(genotype, value, median, na.rm = T))
    
    # create color factor
    color <- rep("gold", length(levels(matrix_df_sorted)))
    color [which( grepl("^AxR", levels(matrix_df_sorted) ))] <- "darkseagreen1"
    color [which( levels (matrix_df_sorted) == "Desiree" )] <- "deepskyblue"
    color [which( levels (matrix_df_sorted) == "Ramses" )] <- "forestgreen"
    color [which( levels (matrix_df_sorted) == "Albatros" )] <- "purple"
    color [which( levels (matrix_df_sorted) == "Euroresa" )] <- "red"
    color_fac <- factor(color)
    # table(color_fac)
    
    # sorted boxplot
    boxplot(value ~ matrix_df_sorted, data = matrix_df, 
            ylab="log10 intensity", cex.lab=1, cex.axis=0.7, 
            las=2, col=color, main = analytes$name_short[i])
    legend("bottomright", fill=levels(color_fac), 
           legend=c("AxR", "Desiree", "Ramses", "ExA", "Albatros", "Euroresa"), 
           horiz=T, cex=0.8)
  }
}




# aggregate median and sort
# all_valdis_merge_IIIc_only_valdis_A1_median <- aggregate(all_valdis_merge_IIIc_only_valdis_A1$value, 
#                                                          by=list(all_valdis_merge_IIIc_only_valdis_A1$genotype),
#                                                          median)
# colnames(all_valdis_merge_IIIc_only_valdis_A1_median) <- c("genotype", "value")
# all_valdis_merge_IIIc_only_valdis_A1_median_sorted <- all_valdis_merge_IIIc_only_valdis_A1_median[order(all_valdis_merge_IIIc_only_valdis_A1_median$value),]
# head(all_valdis_merge_IIIc_only_valdis_A1_median_sorted)



