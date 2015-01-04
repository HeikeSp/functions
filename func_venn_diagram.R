library(VennDiagram)


# Venn diagramm with 2 sets

func_venn_diagram_2 <- function(res1, res2, threshold, lab1, lab2, filepath){
  label1 <- paste(lab1, "(" ,length(which(res1 < threshold)), ")", sep="")
  label2 <- paste(lab2, "(" ,length(which(res2 < threshold)), ")", sep="")
  
  w <- list(
    label1 = which(res1 < threshold),
    label2 = which(res2 < threshold)
  )
  
  venn.plot <- venn.diagram(
    x = w,  filename = filepath,
    col = "black",
    fill = c("goldenrod1", "dodgerblue2"), #category colors  
    alpha = 0.5,
    cat.cex = 1.5, #cat font size
    cat.fontface = "bold",
    margin = 0.2, # Bildbegrenzung 
    cex=1.5, 
    cat.dist=0.07, 
    scaled=FALSE)
  
  return(venn.plot)
}

######################################################

# Venn diagramm with 4 sets

func_venn_diagram_4 <- function(res1, res2, res3, res4, threshold, lab1, lab2, lab3, lab4, filepath){
  w <- list(
    paste(lab1, "(" ,length(which(res1 < threshold)) , ")", sep="") <- which(res1 < threshold),
    paste(lab2, "(" ,length(which(res2 < threshold)) , ")", sep="") <- which(res2 < threshold),
    paste(lab3, "(" ,length(which(res3 < threshold)) , ")", sep="") <- which(res3 < threshold),
    paste(lab4, "(" ,length(which(res4 < threshold)) , ")", sep="") <- which(res4 < threshold)
    )
  
  venn.plot <- venn.diagram(
    x = w,  filename = filepath,
    col = "black",
    fill = c("goldenrod1", "dodgerblue2", "darkorange2", "cyan3"), #category colors  
    alpha = 0.5,
    cat.cex = 1, #cat font size
    cat.fontface = "bold",
    margin = 0.1, # Bildbegrenzung 
    cex=1.5, 
    #cat.dist=0.25, 
    scaled=FALSE)
  
  return(venn.plot)
}

