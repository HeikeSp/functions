# this function is for general procedure of DGE by edgeR (groups are defined before)

func_edgeR_dge <- function(counts_keep = counts_keep, 
                      group_idx, 
                      group_1, 
                      FDR_threshold = 0.05) # default value for FDR
  {
  
  counts_1_keep <- counts_keep[ ,group_idx]
  print("colnames of counts table:")
  print(colnames(counts_1_keep))
  print("dimensions of counts table:")
  print(dim(counts_1_keep))
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print("before normalization:")
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
  print("after normalization:")
  print(head(counts_dge_list_1$samples))
  
  
  ## Producing an MDS plot
  #An MDS plots shows distances, in terms of biological coeficient of variation (BCV), between samples.
  plotMDS(counts_dge_list_1, main="MDS Plot")
  
  
  ## Estimating the dispersion
  ### The common dispersion estimates the overall BCV of the dataset, averaged over all genes:
  counts_dge_list_1 <- estimateCommonDisp(counts_dge_list_1, verbose=TRUE)
  # BCV (square root of the common dispersion) 
  
  
  ### Now estimate gene-specific dispersions:
  counts_dge_list_1 <- estimateTagwiseDisp(counts_dge_list_1)
  
  
  ### Plot the estimated dispersions:
  #Plot genewise biological coefficient of variation (BCV) against gene abundance (in log2 counts per million)
  #The BCV is the square root of the negative binomial dispersion. This function displays the common, trended and tagwise BCV estimates.
  plotBCV(counts_dge_list_1)
  
  
  ## Differential Expression
  ### Compute exact genewise tests for differential expression:
  #**exactTest:** Compute genewise exact tests for differences in the means between two groups of negative-binomially distributed counts.
  #**topTags:** Extracts the top DE tags in a data frame for a given pair of groups, ranked by p-value or absolute log-fold change.
  # *--> default for option adjust.method = "BH"*
  et_1 <- exactTest(counts_dge_list_1)
  print("contrasting group factors:")
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags:")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  # significant down-regulation
  top_all_down = subset(top_all_1$table, top_all_1$table$logFC<0)
  # significant up-regulation
  top_all_up = subset(top_all_1$table, top_all_1$table$logFC>0)
  
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1$table, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  #print(head(top_all_1_dmp))
  
  # only significant results with FDR < 0.05
  top_sig = subset(top_all_1$table, top_all_1$table$FDR < FDR_threshold)
  
  # significant down-regulation
  top_sig_down = subset(top_sig, top_sig$logFC<0)
  # significant up-regulation
  top_sig_up = subset(top_sig, top_sig$logFC>0)
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  #print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print("Total number of DE genes at 5% FDR:")
  print(summary(de_1 <- decideTestsDGE(et_1)))
  
  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sig = top_sig, 
               top_all = top_all_1$table, 
               top_all_down = top_all_down,
               top_all_up = top_all_up,
               top_sig_down = top_sig_down,
               top_sig_up = top_sig_up,
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sig = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<FDR_threshold)), ]
  ))
 }