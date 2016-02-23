
library(VennDiagram)
library(RColorBrewer)

# Venn diagramm with 2 sets
# The function defaults to placing the larger set on the left. inverted or rotation.degree can be used to reverse this. 

func_venn_diagram_2 <- function(res1, res2, threshold = 0, lab1, lab2, filepath=NULL, rot=0){
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
      col = "transparent", # colour of each circle's circumference
      fill = c("#FD8D3C", "#74C476"), # green, orange
      alpha = 0.5,
      cat.cex = 1.5, # cat font size
      fontface = "bold",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      margin = 0.2, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = 0.1, 
      scaled = FALSE,
      rotation.degree=rot) # The function defaults to placing the larger set on the left. 
                           # inverted or rotation.degree can be used to reverse this. 
    
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
      col = "transparent",
      fill = c("#74C476","#FD8D3C"), # green, orange
      alpha = 0.5,
      cat.cex = 1.5, #cat font size
      fontface = "bold",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      margin = 0.2, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = 0.1, 
      scaled = FALSE)
    
    return(venn.plot)
  }
}

######################################################

# Venn diagramm with 3 sets
# The function defaults to placing the larger set on the left. inverted or rotation.degree can be used to reverse this. 

func_venn_diagram_3 <- function(res1, res2, res3, threshold = 0, lab1, lab2, lab3, 
                                filepath=NULL, rot=0, dist_val = 0.1){
                                #position_val = c(0,0,0)){
  if (threshold == 0) # don't use threshold
  {
    w <- list(
      lab1 = res1,
      lab2 = res2,
      lab3 = res3
    )
    
    names(w) <- c(paste(lab1, "\n", "(" ,length(res1), ")", sep=""),
                  paste(lab2, "\n", "(" ,length(res2), ")", sep=""),
                  paste(lab3, "\n", "(" ,length(res3), ")", sep=""))
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "transparent", # colour of each circle's circumference
      fill = c("goldenrod1", "dodgerblue2", "darkorange2"), # category colors 
      alpha = 0.5,
      cat.cex = 1.5, # cat font size
      fontface = "bold",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      margin = 0.2, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = dist_val, 
      scaled = FALSE,
      rotation.degree=rot)
      #cat.pos = position_val) # The function defaults to placing the larger set on the left. 
    # inverted or rotation.degree can be used to reverse this. 
    
    return(venn.plot)
  }
  
  else # use threshold
  {  
    w <- list(
      paste(lab1, "(" ,length(which(res1 < threshold)) , ")", sep="") <- which(res1 < threshold),
      paste(lab2, "(" ,length(which(res2 < threshold)) , ")", sep="") <- which(res2 < threshold),
      paste(lab3, "(" ,length(which(res3 < threshold)) , ")", sep="") <- which(res3 < threshold)
    )
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "transparent",
      fill = c("goldenrod1", "dodgerblue2", "darkorange2"), # category colors  
      alpha = 0.5,
      cat.cex = 1.5, #cat font size
      fontface = "bold",
      fontfamily = "sans",
      cat.fontfamily = "sans",
      margin = 0.2, # Bildbegrenzung 
      cex = 1.5, 
      cat.dist = 0.1, 
      scaled = FALSE)
    
    return(venn.plot)
  }
}

######################################################

# Venn diagramm with 4 sets

func_venn_diagram_4 <- function(res1, res2, res3, res4, threshold = 0, lab1, lab2, lab3, lab4, filepath=NULL){
  
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
      fill = c("goldenrod1", "dodgerblue2", "darkorange2", "cyan3"), # category colors  
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
      fill = c("goldenrod1", "dodgerblue2", "darkorange2", "cyan3"), # category colors  
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


######################################################

# Venn diagramm with 5 sets

func_venn_diagram_5 <- function(res1, res2, res3, res4, res5, 
                                lab1, lab2, lab3, lab4, lab5, 
                                filepath = NULL){

    w <- list(
      lab1 = res1,
      lab2 = res2,
      lab3 = res3,
      lab4 = res4,
      lab5 = res5)
    
    names(w) <- c(paste(lab1, "\n", "(" ,length(res1), ")", sep=""),
                  paste(lab2, "\n", "(" ,length(res2), ")", sep=""),
                  paste(lab3, "\n", "(" ,length(res3), ")", sep=""),
                  paste(lab4, "\n", "(" ,length(res4), ")", sep=""),
                  paste(lab5, "\n", "(" ,length(res5), ")", sep=""))
    
    venn.plot <- venn.diagram(
      x = w,  
      filename = filepath,
      col = "black",
      fill = brewer.pal(5, "Set1"), # category colors  
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
