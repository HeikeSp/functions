
library(VennDiagram)

# Venn diagramm with 2 sets

func_venn_diagram_2 <- function(res1, res2, threshold = 0, lab1, lab2, filepath=NULL){
  if (threshold == 0) # don't use threshold
  {
    w <- list(
      lab1 = res1,
      lab2 = res2
    )
    
    names(w) <- c(paste(lab1, "\n", "(" ,length(res1), ")", sep=""),
                  paste(lab2, "\n", "(" ,length(res2), ")", sep=""))
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "black",
      fill = c("#74C476","#FD8D3C"), # green, orange
      #fill = c("goldenrod1", "dodgerblue2"), # category colors  
      alpha = 0.5,
      cat.cex = 1.5, # cat font size
      cat.fontface = "bold",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      margin = 0.2, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = 0.1, 
      scaled = FALSE)
    
    return(venn.plot)
  }
  
  else # use threshold
  {  
    w <- list(
      label1 = which(res1 < threshold),
      label2 = which(res2 < threshold)
    )
    
    names(w) <- c(paste(lab1, "(" ,length(which(res1 < threshold)), ")", sep=""),
                  paste(lab2, "(" ,length(which(res2 < threshold)), ")", sep=""))
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "black",
      fill = c("#74C476","#FD8D3C"), # green, orange
      #fill = c("goldenrod1", "dodgerblue2"), #category colors  
      alpha = 0.5,
      cat.cex = 1.5, #cat font size
      cat.fontface = "bold",
      margin = 0.2, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = 0.1, 
      scaled = FALSE)
    
    return(venn.plot)
  }
}

######################################################

# Venn diagramm with 4 sets

func_venn_diagram_4 <- function(res1, res2, res3, res4, threshold = 0, lab1, lab2, lab3, lab4, filepath){
  
  if (threshold == 0) # don't use threshold
  {
    w <- list(
      lab1 = res1,
      lab2 = res2,
      lab3 = res3,
      lab4 = res4
    )
    
    names(w) <- c(paste(lab1, "\n", "(" ,length(res1), ")", sep=""),
                  paste(lab2, "\n", "(" ,length(res2), ")", sep=""),
                  paste(lab3, "\n", "(" ,length(res3), ")", sep=""),
                  paste(lab4, "\n", "(" ,length(res4), ")", sep=""))
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "black",
      fill = c("goldenrod1", "dodgerblue2", "darkorange2", "cyan3"), #category colors  
      alpha = 0.5,
      cat.cex = 1.3, # cat font size
      cat.fontface = "bold",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      margin = 0.25, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = 0.2, 
      scaled = FALSE)
    
    return(venn.plot)
  }
  
  else # use threshold
  {  
    w <- list(
      paste(lab1, "(" ,length(which(res1 < threshold)) , ")", sep="") <- which(res1 < threshold),
      paste(lab2, "(" ,length(which(res2 < threshold)) , ")", sep="") <- which(res2 < threshold),
      paste(lab3, "(" ,length(which(res3 < threshold)) , ")", sep="") <- which(res3 < threshold),
      paste(lab4, "(" ,length(which(res4 < threshold)) , ")", sep="") <- which(res4 < threshold)
      )
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "black",
      fill = c("goldenrod1", "dodgerblue2", "darkorange2", "cyan3"), #category colors  
      alpha = 0.5,
      cat.cex = 1, #cat font size
      cat.fontface = "bold",
      margin = 0.1, # Bildbegrenzung 
      cex = 1.5, 
      #cat.dist = 0.25, 
      scaled = FALSE)
    
    return(venn.plot)
  }
}

