# function for pageman plot (heatmap)

func_pageman_plot <- function(pageman_data, threshold){
  colnames(pageman_data) <- c("bin_id", "bin_name", 
                                      "tolerant_up", "sensitive_up", "spacer",
                                      "tolerant_down",  "sensitive_down", "sig")
    
  pageman_data <- pageman_data[,-c(9,10)]
  pageman_data$spacer <- NA
  
  pageman_data_sig <- pageman_data[pageman_data$sig==1,]
  head(pageman_data_sig)
  
  pageman_data_sig_3 <- pageman_data_sig
  
  # columns 3,4,6,7: tolerant_up, sensitive_up, tolerant_down, sensitive_down
  for (i in c(3,4,6,7)){
    pageman_data_sig_3[which(abs(pageman_data_sig_3[,i])<threshold), i] <- 0
  }
  
  # calculate sum per row
  pageman_data_sig_3_sum <- apply(pageman_data_sig_3[,c(3,4,6,7)], 1, sum)
  
  # select only rows where sum is > 0, that means at least one column needs to be significant (below threshold)
  #pageman_data_sig_3_sel <- pageman_data_sig_3[which(abs(pageman_data_sig_3_sum)>0),] # plot only z-scores below threshold 
  pageman_data_sig_3_sel <- pageman_data_sig[which(abs(pageman_data_sig_3_sum)>0),] # plot ALL z-scores below threshold
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

