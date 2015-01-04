#===============================================================================
# Name   : PCA scores plots
# Author : Heike Sprenger
# Date   : 2014-07-16
# Version: 0.1
#===============================================================================


func_5pairs_plot <- function(pca_res, factors, color_factor, symbol=19, maintext=""){
  pairs(pca_res@scores[,1:5], 
        col = factors[,color_factor],
        pch=symbol, 
        main=maintext)
}

func_5pairs_plot_sym <- function(pca_res, factors, color_factor, symbols=c(17,19), symbol_factor, maintext=""){
  pairs(pca_res@scores[,1:5], 
        col = factors[,color_factor],
        pch = symbols[as.integer(factors[,symbol_factor])], 
        main=maintext)
  
}


func_pca_plot <- function(pca_res, dim1, dim2, factors, color_factor, symbols=19, symbol_size=1.5, pos1, leg1, 
                          maintext="", legend.text, xmin=min(pca_res@scores[,dim1], na.rm=T), xmax=max(pca_res@scores[,dim1], na.rm=T),
                          ymin=min(pca_res@scores[,dim2], na.rm=T), ymax=max(pca_res@scores[,dim2], na.rm=T))
{
  plot(pca_res@scores[,dim1], pca_res@scores[,dim2], 
       xlab = paste("PC",dim1, " (", round(pca_res@R2[dim1]*100, 1),"%)", sep=""),
       ylab = paste("PC",dim2, " (", round(pca_res@R2[dim2]*100, 1),"%)", sep=""),
       col = factors[,color_factor],
       pch = symbols, 
       main = maintext, 
       cex = symbol_size,
       cex.lab=1.4, 
       cex.axis=1.2,
       xlim=c(xmin, xmax),
       ylim=c(ymin, ymax)
  )
  
  legend(pos1, cex=leg1, bty="n",
         levels(factors[,color_factor]), 
         fill=1:length(levels(factors[,color_factor])),
         legend=legend.text)
}

func_pca_plot_sym <- function(pca_res, dim1, dim2, factors, color_factor, symbols=c(17,19), 
                              symbol_factor, symbol_size=1.5, pos1, leg1, pos2, leg2, maintext="",
                              legend.text1, xmin=min(pca_res@scores[,dim1], na.rm=T), xmax=max(pca_res@scores[,dim1], na.rm=T),
                              ymin=min(pca_res@scores[,dim2], na.rm=T), ymax=max(pca_res@scores[,dim2], na.rm=T))
{
  plot(pca_res@scores[,dim1], pca_res@scores[,dim2], 
       xlab = paste("PC",dim1, " (", round(pca_res@R2[dim1]*100, 1),"%)", sep=""),
       ylab = paste("PC",dim2, " (", round(pca_res@R2[dim2]*100, 1),"%)", sep=""),
       col = factors[,color_factor],
       pch = symbols[as.integer(factors[,symbol_factor])], 
       main = maintext, 
       cex = symbol_size,
       cex.lab=1.4, 
       cex.axis=1.2,
       xlim=c(xmin, xmax),
       ylim=c(ymin, ymax)
  )
  
  legend(pos1, cex=leg1, bty="n",
         levels(factors[,color_factor]), 
         fill=1:length(levels(factors[,color_factor])),
         legend=legend.text1)
  
  legend(pos2, cex=leg2, bty="n",
         levels(factors[,symbol_factor]), 
         pch=symbols)
}