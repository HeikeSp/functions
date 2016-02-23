# function for corto plot (heatmap)

func_corto_plot <- function(corto_data, threshold, show_all = TRUE){
  
  colnames(corto_data) <- c("Bin", "BinName",
                            "tolerant_up", "sensitive_up",
                            "tolerant_down",  "sensitive_down")
 
  # replace NAs by Zero
  corto_data[is.na(corto_data)] <- 0
  
  corto_data_part <- corto_data
  
  # set all z-scores that are below the threshold to zero --> they appear as white in the plot
  # only boxes with z-score above threshold are shown in the plot!
  # columns 3 to 6: tolerant_up, sensitive_up, tolerant_down, sensitive_down
  for (i in 3:6){
    corto_data_part[which(abs(corto_data_part[,i])<threshold), i] <- 0
  }
  
  # calculate sum per row
  corto_data_part_sum <- apply(corto_data_part[,3:6], 1, sum)
  
  # select only rows where sum is > 0, that means at least one column needs to be significant (absolute z-score above threshold)
  
  if (show_all == TRUE) # plot ALL z-scores in a row where at least one column is below threshold
  {
    corto_data_sel <- corto_data[which(abs(corto_data_part_sum)>0),] 
  } else {
    corto_data_sel <- corto_data_part[which(abs(corto_data_part_sum)>0),]
  }
  
  # else: plot ONLY z-scores below threshold 
  # use "corto_data_part" which contains only z-scores meeting the threshold
  
  return(corto_data_sel)
}

######################################################


# function for larger table

func_corto_plot2 <- function(corto_data, threshold){
  colnames(corto_data) <- c("Bin", "BinName", 
                            "GH_C_T>S", "GH_D_T>S", "Field_C_T>S", "Field_D_T>S",
                            "GH_C_S>T", "GH_D_S>T", "Field_C_S>T", "Field_D_S>T")
  
  # replace NAs by Zero
  corto_data[is.na(corto_data)] <- 0
  
  corto_data_part <- corto_data
  
  for (i in c(3:10)){
    corto_data_part[which(abs(corto_data_part[,i])<threshold), i] <- 0
  }
  
  # calculate sum per row
  corto_data_part_sum <- apply(corto_data_part[,c(3:10)], 1, sum)
  
  corto_data_sel <- corto_data_part[which(abs(corto_data_part_sum)>0),]
  
  return(corto_data_sel)
}

#######################################################

# function for plotting the heatmap itself


func_corto_heatmap <- function(corto_data, bin_names, 
                               col_breaks_greenhouse = TRUE,
                               show_col_dend = TRUE, 
                               margins_values = c(8,50), 
                               lwid_input = c(1, 7),
                               lhei_input = c(1, 6),
                               srtCol_value = 330,
                               bin_codes){
  
  mat_data <- as.matrix(corto_data)
  
  distance <- dist(mat_data, method = "euclidian")
  cluster <- hclust(distance, method = "average")
  
  mycluster <- cutree(cluster, k=6)
  
  mycolor <- brewer.pal(8, "Set2")
  mycolor <- mycolor[as.vector(mycluster)]
  #my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 29)
  my_palette <- colorRampPalette(c("white", "red"))(n = 29)
  
  if(col_breaks_greenhouse == TRUE){
    col_breaks <- c(seq(0, 5.5, length.out =  15), # for white
                    seq(6.1, 12, length.out = 15)) # for red
  } else {
    col_breaks <- c(seq(0, 5.5, length.out =  15), # for white
                    seq(6.1, 15, length.out = 15)) # for red
  }
  
  lmat_values = rbind(4:3 , 2:1)
  lhei_values = lhei_input
  lwid_values = lwid_input
  layout(mat = lmat_values, widths = lwid_values, heights = lhei_values)
  
  if(show_col_dend == TRUE){
    heatmap.2(mat_data,
              colsep = 1:ncol(mat_data), 
              rowsep = 1:nrow(mat_data),
              labRow = paste(bin_codes, "\t",bin_names),
              sepwidth = c(0.01, 0.05),
              sepcolor = "black",
              main = "",              # heat map title
              density.info = "none",  # turns off density plot inside color legend
              trace = "none",         # turns off trace lines inside the heat map
              margins = margins_values,      # widens margins around plot
              keysize = 1.5,
              cexRow = 1.5,
              cexCol = 1.5,
              adjCol = c(0, 1),
              srtCol = srtCol_value,
              lhei = lhei_values,
              lwid = lwid_values,
              col = my_palette,       # use on color palette defined earlier 
              breaks = col_breaks,    # enable color transition at specified limits
              RowSideColors = mycolor,
              Rowv = as.dendrogram(cluster),
              dendrogram = "row",     # only draw a row dendrogram
              Colv = "NA",            # turn off column clustering
              na.color = "grey")            
  } else {
    heatmap.2(mat_data,
              colsep = 1:ncol(mat_data), 
              rowsep = 1:nrow(mat_data),
              labRow = paste(bin_codes, "\t", bin_names),
              sepwidth = c(0.01, 0.05),
              sepcolor = "black",
              main = "", # heat map title
              density.info = "none",  # turns off density plot inside color legend
              trace = "none",         # turns off trace lines inside the heat map
              margins = margins_values,      # widens margins around plot
              keysize = 1.5,
              cexRow = 1.5,
              cexCol = 1.5,
              adjCol = c(0,1),
              srtCol = srtCol_value,
              lmat = lmat_values,
              lhei = lhei_values,
              lwid = lwid_values,
              col = my_palette,       # use on color palette defined earlier 
              breaks = col_breaks,    # enable color transition at specified limits
              Rowv = NULL,
              dendrogram = "none",    # only draw a row dendrogram
              Colv = "NA",            # turn off column clustering
              na.color = "grey")
  }
}