### Function to plot BBCH values in density plots and histograms

func_bbch_plots <- function(dataset, bbch, treatment, min=20, max=70, label_xaxis = "developmental stage (BBCH)"){
  
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

  plot_A <- ggplot(dataset, aes(x=bbch)) + 
    geom_density(aes(group=treatment, colour=treatment, fill=treatment), alpha=0.5)  + 
    coord_cartesian(xlim=c(min, max)) + 
    scale_fill_manual(values = cols_treatment) + 
    scale_color_manual(values = cols_treatment) + 
    xlab(label_xaxis)
  
  plot_B <- ggplot(dataset, aes(x=bbch, fill=treatment, colour=treatment)) + 
    geom_histogram(binwidth=1, alpha=0.5, position="identity")  + 
    coord_cartesian(xlim=c(min, max)) + 
    scale_fill_manual(values = cols_treatment) + 
    scale_color_manual(values = cols_treatment) + 
    xlab(label_xaxis)
  
  plot_C <- ggplot(dataset, aes(x=bbch, fill=treatment)) + 
    geom_histogram(binwidth=1, alpha=0.5, position="identity")  + 
    coord_cartesian(xlim=c(min, max)) + 
    scale_fill_manual(values = cols_treatment) + 
    scale_color_manual(values = cols_treatment) + 
    xlab(label_xaxis)
  
  return(list(plot_A, plot_B, plot_C))
  
}


func_bbch_density_plot <- function(dataset, bbch, treatment, min=20, max=70){
  
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
    legend.position=c(0, 1), 
    legend.justification = c(0, 1),
    legend.title=element_blank(),
    legend.text = element_text(size=14),
    panel.background = element_rect(colour = "black"),
    panel.border = element_rect(fill=NA, colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank())
  # aspect.ratio=1
  
  plot_A <- ggplot(dataset, aes(x=bbch)) + geom_density(aes(group=treatment, colour=treatment, fill=treatment), alpha=0.5)  + coord_cartesian(xlim=c(min,max)) + scale_fill_manual(values = cols_treatment) + scale_color_manual(values = cols_treatment) + xlab("developmental stage (BBCH scale)")
    
  return(plot_A)
  
}
  

# theme_new$element_rect
