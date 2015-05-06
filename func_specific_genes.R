func_specific_genes_cultivar <- function(fpkm_data, samplelist_name, cultivar_name, threshold=2){
  idx <- which(samplelist_name$cultivar==cultivar_name)
  fpkm_cul <- fpkm_data[, idx]
  fpkm_not_cul <- fpkm_data[, -idx]
  
  fpkm_cul_min <- apply(fpkm_cul, 1, min) 
  fpkm_cul_mean <- apply(fpkm_cul, 1, mean) 
  fpkm_cul_median <- apply(fpkm_cul, 1, median) 
  
  fpkm_not_cul_max <- apply(fpkm_not_cul, 1, max)
  fpkm_not_cul_median <- apply(fpkm_not_cul, 1, median)
  
  fpkm_fc <- (fpkm_cul_median + 1)/ (fpkm_not_cul_median + 1)
  print(length(which(fpkm_fc > threshold)))
  print(length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)))
  
  return(list(fc = fpkm_fc,
              specific_number = length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)),
              specific = which(fpkm_fc > threshold & fpkm_not_cul_max < 1),
              higher_number = length(which(fpkm_fc > threshold)),
              higher = which(fpkm_fc > threshold)
              )
         )
}

func_specific_genes_cultivar_boxplot <- function(fpkm_data, samplelist_name, cultivar_name, gene_name){
  idx <- which(samplelist_name$cultivar==cultivar_name)
  fpkm_cul <- fpkm_data[, idx]
  fpkm_not_cul <- fpkm_data[, -idx]
  
  boxplot(as.numeric(fpkm_cul[gene_name,]), as.numeric(fpkm_not_cul[gene_name,]), 
          names=c(cultivar_name, "other cultivars"), ylab="FPKM")
  abline(h=1, col="red")
}


#### specific genes per tolerance group

func_specific_genes_tolerance <- function(fpkm_data, samplelist_name, tolerance_group, threshold=2){
  idx <- which(samplelist_name$tolerance == tolerance_group)
  fpkm_cul <- fpkm_data[, idx]
  fpkm_not_cul <- fpkm_data[, -idx]
  
  fpkm_cul_min <- apply(fpkm_cul, 1, min) 
  fpkm_cul_mean <- apply(fpkm_cul, 1, mean) 
  fpkm_cul_median <- apply(fpkm_cul, 1, median) 
  
  fpkm_not_cul_median <- apply(fpkm_not_cul, 1, median)
  fpkm_not_cul_max <- apply(fpkm_not_cul, 1, max)
  
  fpkm_fc <- (fpkm_cul_median + 1)/ (fpkm_not_cul_median + 1)
  print(length(which(fpkm_fc > threshold)))
  print(length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)))
  
  return(list(fc = fpkm_fc,
              specific_number = length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)),
              specific = which(fpkm_fc > threshold & fpkm_not_cul_max < 1),
              higher_number = length(which(fpkm_fc > threshold)),
              higher = which(fpkm_fc > threshold)
  )
  )
}

func_specific_genes_tolerance_boxplot <- function(fpkm_data, samplelist_name, tolerance_group, gene_name, others){
  idx <- which(samplelist_name$tolerance == tolerance_group)
  fpkm_cul <- fpkm_data[, idx]
  fpkm_not_cul <- fpkm_data[, -idx]
  
  boxplot(as.numeric(fpkm_cul[gene_name,]), as.numeric(fpkm_not_cul[gene_name,]), 
          names=c(tolerance_group, others), ylab="FPKM", main=gene_name)
  abline(h=1, col="red")
}

##### for cultivation

func_specific_genes_cultivation <- function(fpkm_data, samplelist_name, cultivation_name, threshold=2){
  idx <- which(samplelist_name$cultivation==cultivation_name)
  fpkm_cul <- fpkm_data[, idx]
  fpkm_not_cul <- fpkm_data[, -idx]
  
  fpkm_cul_min <- apply(fpkm_cul, 1, min) 
  fpkm_cul_mean <- apply(fpkm_cul, 1, mean) 
  fpkm_cul_median <- apply(fpkm_cul, 1, median) 
  
  fpkm_not_cul_max <- apply(fpkm_not_cul, 1, max)
  fpkm_not_cul_median <- apply(fpkm_not_cul, 1, median)
  
  fpkm_fc <- (fpkm_cul_median + 1)/ (fpkm_not_cul_median + 1)
  print(length(which(fpkm_fc > threshold)))
  print(length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)))
  
  return(list(fc = fpkm_fc,
              specific_number = length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)),
              specific = which(fpkm_fc > threshold & fpkm_not_cul_max < 1),
              higher_number = length(which(fpkm_fc > threshold)),
              higher = which(fpkm_fc > threshold)
  )
  )
}

##### for condition

func_specific_genes_condition <- function(fpkm_data, samplelist_name, condition_name, threshold=2){
  idx <- which(samplelist_name$condition == condition_name)
  fpkm_cul <- fpkm_data[, idx]
  fpkm_not_cul <- fpkm_data[, -idx]
  
  fpkm_cul_min <- apply(fpkm_cul, 1, min) 
  fpkm_cul_mean <- apply(fpkm_cul, 1, mean) 
  fpkm_cul_median <- apply(fpkm_cul, 1, median) 
  
  fpkm_not_cul_max <- apply(fpkm_not_cul, 1, max)
  fpkm_not_cul_median <- apply(fpkm_not_cul, 1, median)
  
  fpkm_fc <- (fpkm_cul_median + 1)/ (fpkm_not_cul_median + 1)
  print(length(which(fpkm_fc > threshold)))
  print(length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)))
  
  return(list(fc = fpkm_fc,
              specific_number = length(which(fpkm_fc > threshold & fpkm_not_cul_max < 1)),
              specific = which(fpkm_fc > threshold & fpkm_not_cul_max < 1),
              higher_number = length(which(fpkm_fc > threshold)),
              higher = which(fpkm_fc > threshold)
  )
  )
}