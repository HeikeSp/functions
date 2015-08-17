# function for pageman plot (heatmap)

func_pageman_plot <- function(pageman_data, threshold, show_all = TRUE){
  colnames(pageman_data) <- c("bin_id", "bin_name", 
                              "tolerant_up", "sensitive_up", "spacer",
                              "tolerant_down",  "sensitive_down", "sig")
    
  pageman_data <- pageman_data[,-c(9,10)]
  pageman_data$spacer <- NA
  
  pageman_data_sig <- pageman_data[pageman_data$sig==1,]

  pageman_data_sig_3 <- pageman_data_sig
  
  # set all z-scores that are below the threshold to zero --> they appear as white in the plot
  # only boxes with z-score above threshold are shown in the plot!
  # columns 3,4,6,7: tolerant_up, sensitive_up, tolerant_down, sensitive_down
  for (i in c(3,4,6,7)){
    pageman_data_sig_3[which(abs(pageman_data_sig_3[,i])<threshold), i] <- 0
  }
  
  # calculate sum per row
  pageman_data_sig_3_sum <- apply(pageman_data_sig_3[,c(3,4,6,7)], 1, sum)
  
  # select only rows where sum is > 0, that means at least one column needs to be significant (absolute z-score above threshold)
  
  if (show_all == TRUE) # plot ALL z-scores in a row where at least one column is below threshold
  {
    pageman_data_sig_3_sel <- pageman_data_sig[which(abs(pageman_data_sig_3_sum)>0),] 
  } else {
    pageman_data_sig_3_sel <- pageman_data_sig_3[which(abs(pageman_data_sig_3_sum)>0),]
    }
  
  # else: plot ONLY z-scores below threshold 
  # use "pageman_data_sig_3" which contains only z-scores meeting the threshold
    
}

######################################################

# function for larger table

func_pageman_plot2 <- function(pageman_data, threshold){
  colnames(pageman_data) <- c("bin_id", "bin_name", 
                              "GH_C_T>S", "GH_D_T>S", "Field_C_T>S", "Field_D_T>S", "spacer",
                              "GH_C_S>T", "GH_D_S>T", "Field_C_S>T", "Field_D_S>T", "sig")
  
  pageman_data <- pageman_data[,-c(13,14)]
  pageman_data$spacer <- NA
  
  pageman_data_sig <- pageman_data[pageman_data$sig==1,]
  
  pageman_data_sig_3 <- pageman_data_sig
  
  for (i in c(3,4,5,6, 8,9,10,11)){
    pageman_data_sig_3[which(abs(pageman_data_sig_3[,i])<threshold), i] <- 0
  }
  
  pageman_data_sig_3_sum <- apply(pageman_data_sig_3[,c(3,4,5,6, 8,9,10,11)], 1, sum)
  
  pageman_data_sig_3_sel <- pageman_data_sig_3[which(abs(pageman_data_sig_3_sum)>0),]
  return(pageman_data_sig_3_sel)
}


func_pageman_heatmap <- function(pageman_data, bin_names, 
                                 col_breaks_greenhouse = TRUE,
                                 show_col_dend = TRUE, 
                                 margins_values = c(8,50), 
                                 lwid_input = c(1, 7),
                                 lhei_input = c(1, 6),
                                 srtCol_value = 330){
  
  mat_data <- as.matrix(pageman_data)
  
  distance <- dist(mat_data, method = "euclidian")
  cluster <- hclust(distance, method = "average")
  
  mycluster <- cutree(cluster, k=6)

  mycolor <- brewer.pal(8, "Set2")
  mycolor <- mycolor[as.vector(mycluster)]
  my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 29)
  
  if(col_breaks_greenhouse == TRUE){
    col_breaks <- c(seq(-11, -4.1, length.out =  10), # for blue
                    seq(-3.5, 3.5, length.out =  10), # for white
                    seq(4.1, 16, length.out = 10)) # for red
  } else {
  col_breaks <- c(seq(-8, -3.1, length.out =  10), # for blue
                  seq(-2.5, 2.5, length.out =  10), # for white
                  seq(3.1, 11, length.out = 10)) # for red
  }
  
  lmat_values = rbind(4:3 , 2:1)
  lhei_values = lhei_input
  lwid_values = lwid_input
  layout(mat = lmat_values, widths = lwid_values, heights = lhei_values)
  
  if(show_col_dend == TRUE){
  heatmap.2(mat_data,
            colsep = 1:ncol(mat_data), 
            rowsep = 1:nrow(mat_data),
            labRow = bin_names,
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
              labRow = bin_names,
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