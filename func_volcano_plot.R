# function for volcano plot: log2FC vs. p-value of t-test (-log10)
# color for points with log2FC > 0.6 and p.value < 0.01 is red,
# others are grey (with transparency)

func_volcano_plot <- function(res_ttest, log2FC, 
                              p_threshold=0.01, 
                              fc_threshold=(log2(1.5)), 
                              x_limits=c(min(log2FC, na.rm=T),max(log2FC, na.rm=T))){
  
  volcano_col <- rep(rgb(100,100,100,100,maxColorValue=255), length(res_ttest))
  volcano_col[intersect( which(res_ttest < p_threshold), 
                         which(log2FC > 0))] <- "red"
  volcano_col[intersect( which(res_ttest < p_threshold), 
                         which(log2FC < 0))] <- "blue"
  
  plot(log2FC, -log10(res_ttest), type="n",
       xlab = "log2-fold-change", 
       ylab = "-log10 p-value",
       xlim = x_limits)
  
  abline(h = -log10(p_threshold), 
         lwd = 2, col = "grey", lty = 2)  
  
#   abline(h = -log10(p_threshold), 
#          v = c(-fc_threshold, fc_threshold), 
#          lwd = 2, col = "grey", lty = 2)  
  
  points(log2FC, -log10(res_ttest), 
       col = "black",
       pch=21, 
       bg = volcano_col, 
       cex=1.5, cex.lab = 1.5, cex.axis = 1.3)

}
