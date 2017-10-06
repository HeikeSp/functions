
func_edger_pipeline <- function(counts_mat = counts_keep2, 
                                info_mat = metadata_sel2, 
                                group_name = "Treatment",
                                group1 = Treatment, 
                                group2 = Batch2,
                                contrasts_mat = my.contrasts2,
                                FDR_threshold = 0.05){
  # create DGEList object
  y <- DGEList(counts = counts_mat, 
               group = info_mat[, group_name])
  
  # apply TMM normalization
  y = calcNormFactors(y)
  
  # multidimensional scaling plot
  #plotMDS(y, labels = y$samples$group, cex=0.5)
  
  # construct the design matrix 
  design <- model.matrix(~ 0 + group1 + group2)
  rownames(design) <- colnames(y)
  print(design)
  print(colnames(design))
  
  # estimating the dispersions
  y = estimateDisp(y, design, robust=TRUE)

  # Plot the tagwise BCV against its abundance (in log2 counts per million logCPM)
  #plotBCV(y, cex=0.4)
  
  # detecting outliers
  # outliers with a prior.df value < 1
  outliers = y$counts[y$prior.df < 1,]
  dim(outliers)
  
  # fit QLglm
  fit  = glmQLFit(y, design, robust=TRUE)
  
  # DE of interest
  qlf = glmQLFTest(fit, contrast = contrasts_mat)
  
  tpTgs = topTags(qlf, n = Inf, sort.by="none")
  
  # pvalue adjustment (FDR)
  qlf$table$FDR <- p.adjust(qlf$table$PValue, method = "BH")
  qlfSig <- qlf$table[ which(qlf$table$FDR < FDR_threshold ), ]
  
  # summary(dt <- decideTestsDGE(qlf)) # only based on FDR
  print(summary(dt <- decideTestsDGE(qlf, lfc = 1))) # based on FDR and LFC
  
  isDE <- as.logical(dt)
  DEnames <- rownames(y)[isDE]
  print(length(DEnames))
  
  plotSmear(qlf, de.tags = DEnames)
  abline(h = c(-1,1), col = "blue")
  
  # return list ob results objects
  res_list <- list(fit = fit, #1
                   qlf = qlf,
                   res = qlf$table, #2
                   resSig = qlfSig,
                   topTags = tpTgs, #3
                   #resSig_down = resSig_sub_down, #4
                   #resSig_up = resSig_sub_up, #5
                   DEnames = DEnames #6
                   )
  return(res_list)
}

# STEP 1
# create DGEList, normalization, and construct design matrix
func_edger_step1 <- function(counts_mat = counts_keep2, 
                             info_mat = metadata_sel2, 
                             group_name = "Treatment",
                             group1 = Treatment, 
                             group2 = Batch2){
  # create DGEList object
  y <- DGEList(counts = counts_mat, 
               group = info_mat[, group_name])
  
  # apply TMM normalization
  y = calcNormFactors(y)
  
  # multidimensional scaling plot
  #plotMDS(y, labels = y$samples$group, cex=0.5)
  
  # construct the design matrix 
  if(is.null(group2)){
    design <- model.matrix(~ 0 + group1)
  } else {
    design <- model.matrix(~ 0 + group1 + group2) # Intercept is zero  
  }
  
  rownames(design) <- colnames(y)
  print(design)
  print(colnames(design))
  
  # estimating the dispersions
  y = estimateDisp(y, design, robust=TRUE)
  
  # Plot the tagwise BCV against its abundance (in log2 counts per million logCPM)
  #plotBCV(y, cex=0.4)
  
  # detecting outliers
  # outliers with a prior.df value < 1
  outliers = y$counts[y$prior.df < 1,]
  dim(outliers)
  
  return( list(y = y, 
               design = design) )
}

# STEP 2
# fit model and perform test, extract statistics
func_edger_step2 <- function(dge_list = y,
                             design_mat = design,
                             contrasts_mat = my.contrasts,
                             FDR_threshold = 0.05){

  # fit QLglm
  fit  = glmQLFit(y = dge_list, design = design_mat, robust=TRUE)
  
  # DE of interest
  qlf = glmQLFTest(fit, contrast = contrasts_mat)
  
  tpTgs = topTags(qlf, n = Inf, sort.by="none")
  
  # pvalue adjustment (FDR)
  qlf$table$FDR <- p.adjust(qlf$table$PValue, method = "BH")
  qlfSig <- qlf$table[ which(qlf$table$FDR < FDR_threshold ), ]
  
  # get genes that are significantly UP or DOWN
  qlfSigUp <- qlfSig[ which(qlfSig$logFC > 1 ), ]
  qlfSigDown <- qlfSig[ which(qlfSig$logFC < (-1) ), ]
  
  UpNames <- rownames(qlfSigUp)
  DownNames <- rownames(qlfSigDown)
  
  # summary(dt <- decideTestsDGE(qlf)) # only based on FDR
  print(summary(dt <- decideTestsDGE(qlf, lfc = 1))) # based on FDR and LFC
  
  isDE <- as.logical(dt)
  DEnames <- rownames(dge_list)[isDE]
  print(length(DEnames))
  
  plotSmear(qlf, de.tags = DEnames)
  abline(h = c(-1,1), col = "blue")
  
  # return list ob results objects
  res_list <- list(fit = fit, 
                   qlf = qlf,
                   res = qlf$table, 
                   resSig = qlfSig,
                   topTags = tpTgs,
                   #resSig_down = resSig_sub_down, 
                   #resSig_up = resSig_sub_up, 
                   DEnames = DEnames,
                   UpNames = UpNames,
                   DownNames = DownNames)
  return(res_list)
}
