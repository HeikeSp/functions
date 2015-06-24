func_yield_plots <- function(yield_trial, names_factors, legend_position){
  
  hist(yield_trial$tuber_FW_g, breaks=10, freq=F, col="grey")
  lines(density(yield_trial$tuber_FW_g, na.rm=T))
  
  hist(yield_trial$tubercore_FW_g, breaks=10, freq=F, col="grey")
  lines(density(yield_trial$tubercore_FW_g, na.rm=T))
  
  hist(yield_trial$tubercore_DW_g, breaks=10, freq=F, col="grey")
  lines(density(yield_trial$tubercore_DW_g, na.rm=T))
  
  hist(yield_trial$drymatter_percent, breaks=10, freq=F, col="grey")
  lines(density(yield_trial$drymatter_percent, na.rm=T))
  
  boxplot(yield_trial$drymatter_percent~yield_trial$treatment, col=cols_treatment, ylab="drymatter in %")
  
  boxplot(yield_trial$starch_yield_g_per_plant~yield_trial$treatment, col=cols_treatment, ylab="starch yield in g/plant")
    
  boxplot(yield_trial$starch_yield_g_per_plant~yield_trial$treatment*yield_trial$cultivar, col=cols_treatment, las=2, ylab="starch yield in g/plant", names=names_factors)
  legend(legend_position, levels(yield_trial$treatment), fill=cols_treatment, cex=1)
  
  boxplot(yield_trial$starch_yield_g_per_plant~yield_trial$treatment*yield_trial$cultivar, col=cols_cultivar_treatment, las=2, ylab="starch yield in g/plant", names=names_factors)
}

func_yield_plots_jkitest <- function(yield_trial, names_factors, legend_position){
  hist(yield_trial$tuber_FW_kg, breaks=20, freq=F, col="grey", main="tuber_FW_kg")
  lines(density(yield_trial$tuber_FW_kg, na.rm=T))
  
  hist(yield_trial$starch_g_per_kg, breaks=20, freq=F, col="grey", main="starch_g_per_kg")
  lines(density(yield_trial$starch_g_per_kg, na.rm=T))
  
  boxplot(yield_trial$starch_yield_g_per_plant~yield_trial$treatment*yield_trial$cultivar, col=cols_treatment, las=2, ylab="starch yield in g/plant", names=names_factors)
  legend(legend_position, levels(yield_trial$treatment), fill=cols_treatment, cex=1)
  
  boxplot(yield_trial$starch_yield_g_per_plant~yield_trial$treatment*yield_trial$cultivar, col=cols_cultivar_treatment, las=2, ylab="starch yield in g/plant", names=names_factors)
}

################

# for thesis

func_starch_yield_plot <- function(yield_trial, names_factors, legend_position){
  
  boxplot(yield_trial$starch_yield_g_per_plant~yield_trial$treatment*yield_trial$cultivar, col=cols_treatment, las=2, ylab="starch yield (in g/plant)", names=names_factors, cex.lab=1.4, cex.axis=1.2)
  legend(legend_position, levels(yield_trial$treatment), fill=cols_treatment, cex=1, bty="n")
}