### Function to plot BBCH values in density plots and histograms


  library(ggplot2)
  theme_new <- theme_set(theme_bw())
  theme_new <- theme_update(
    #axis.title.x = element_text(face="bold", size=16),
    #axis.title.y = element_text(face="bold", size=16, angle=90),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16, angle=90),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=12),
    legend.title = element_text(face="bold", size=14),
    legend.text = element_text(size=12),
    plot.title = element_text(face="bold", size=16),
    legend.position="bottom")
  # aspect.ratio=1

  func_histograms_treatment <- function(sub_dataset, value, treatment){
  
  plot_B <- ggplot(sub_dataset, aes(x=value, fill=treatment, colour=treatment)) + geom_histogram(binwidth=1, alpha=0.5, position="identity")  + coord_cartesian(xlim=c(10,41)) + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("Ct value") + ggtitle(levels(sub_dataset$variable))
  
  return(plot(plot_B))
  
}
  
  func_histograms_trial <- function(sub_dataset, value, trial){
    
    plot_B <- ggplot(sub_dataset, aes(x=value, fill=trial, colour=trial)) + geom_histogram(binwidth=1, alpha=0.5, position="identity")  + coord_cartesian(xlim=c(10,41)) + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("Ct value") + ggtitle(levels(sub_dataset$variable))
    
    return(plot(plot_B))
    
  }
  
  func_histograms_crossing <- function(sub_dataset, value, crossing){
    
    plot_B <- ggplot(sub_dataset, aes(x=value, fill=crossing, colour=crossing)) + geom_histogram(binwidth=1, alpha=0.5, position="identity")  + coord_cartesian(xlim=c(10,41)) + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("Ct value") + ggtitle(levels(sub_dataset$variable))
    
    return(plot(plot_B))
    
  }

  
  ##########################################################
  
  func_density_treatment <- function(sub_dataset, value, treatment){
    
    plot_B <- ggplot(sub_dataset, aes(x=value, fill=treatment, colour=treatment)) + geom_density(alpha=0.5)  + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("log10 amount") + ggtitle(levels(sub_dataset$variable))
    
    return(plot(plot_B))
    
  }
  
  func_density_trial <- function(sub_dataset, value, trial){
    
    plot_B <- ggplot(sub_dataset, aes(x=value, fill=trial, colour=trial)) + geom_density(alpha=0.5)  + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("log10 amount") + ggtitle(levels(sub_dataset$variable))
    
    return(plot(plot_B))
    
  }
  
  func_density_crossing <- function(sub_dataset, value, crossing){
    
    plot_B <- ggplot(sub_dataset, aes(x=value, fill=crossing, colour=crossing)) + geom_density(alpha=0.5)  + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("log10 amount") + ggtitle(levels(sub_dataset$variable))
    
    return(plot(plot_B))
    
  }