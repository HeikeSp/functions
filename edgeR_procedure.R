## Counts table for greenhouse samples

edgeR_dge_1 <- function(cultivation_type, contrast) {
  counts_1 <- exp_counts_genes_rounded[ ,which(samplelist_modified$cultivation==cultivation_type)]
  print(colnames(counts_1))
  
  
  # keep only genes with mean =! 0 and mean above 1st Quartile
  counts_1_keep <- counts_1[which( rownames(counts_1) %in% rownames_intersect_greenhouse_field) , ]
  print(dim(counts_1_keep))
  
  
  ## group definitions  --> group = condition which is treatment (control/stress)
  group_1 <- as.factor(subset(samplelist_modified, cultivation ==cultivation_type)[, contrast])
  # group_1 <- as.factor(subset(samplelist_modified, cultivation =="greenhouse")$condition)
  print("group definition")
  print(group_1)
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
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
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  print(head(top_all_1_dmp))
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print(summary(de_1 <- decideTestsDGE(et_1)))

  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sub = subset(top_all_1$table, top_all_1$table$FDR<0.05), 
               top_all = top_all_1$table, 
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sub = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<0.05)), ]
               ))
}

########################################################################################################################

# this 2nd function is for selection of samples using 2 factors: cultivation and condition
# (e.g. only greenhouse, control. with contrast tolerance: tolerant vs. sensitive)

edgeR_dge_condition <- function(var1, cultivation_type, var2, condition_type, contrast) {
  counts_1_keep <- counts_keep[ ,which(samplelist_modified[, var1]==cultivation_type & samplelist_modified[, var2]==condition_type)]
  print(colnames(counts_1_keep))
  
  
  # keep only genes with mean =! 0 and mean above 1st Quartile
  #counts_1_keep <- counts_1[which( rownames(counts_1) %in% rownames_intersect_greenhouse_field) , ]
  #print(dim(counts_1_keep))
  
  
  ## group definitions  --> group = condition which is treatment (control/stress)
  group_1 <- as.factor(subset(samplelist_modified, cultivation == cultivation_type & condition == condition_type)[, contrast])
  print("group definition")
  print(group_1)
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
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
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  print(head(top_all_1_dmp))
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print(summary(de_1 <- decideTestsDGE(et_1)))
  
  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sub = subset(top_all_1$table, top_all_1$table$FDR<0.05), 
               top_all = top_all_1$table, 
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sub = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<0.05)), ]
               ))
}

########################################################################################################################
# levels(samplelist_modified$trial_name)
# Levels: JKI-GWH1 JKIFeld2012 MPIFeld2011 MPIFeld2012 MPITest1.2 MPITest2

trial_name <- samplelist_modified$trial_name
levels(samplelist_modified$trial)
#condition <- samplelist_modified$condition

edgeR_dge_trial <- function(trial_id, contrast) {
  counts_1 <- exp_counts_genes_rounded[ ,which(samplelist_modified$trial==trial_id)]
  print(colnames(counts_1))
  
  
  # keep only genes with mean =! 0 and mean above 1st Quartile
  counts_1_keep <- counts_1[which( rownames(counts_1) %in% rownames_intersect_greenhouse_field) , ]
  print(dim(counts_1_keep))
  
  
  ## group definitions  --> group = condition which is treatment (control/stress)
  group_1 <- as.factor(subset(samplelist_modified, trial ==trial_id)[, contrast])
  #group_1 <- as.factor(subset(samplelist_modified, trial ==trial)$condition)
  print("group definition")
  print(group_1)
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
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
  
  # plotBCV(counts_dge_list_1)
  
  
  ## Differential Expression
  ### Compute exact genewise tests for differential expression:
  #**exactTest:** Compute genewise exact tests for differences in the means between two groups of negative-binomially distributed counts.
  #**topTags:** Extracts the top DE tags in a data frame for a given pair of groups, ranked by p-value or absolute log-fold change.
  # *--> default for option adjust.method = "BH"*
  et_1 <- exactTest(counts_dge_list_1)
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  print(head(top_all_1_dmp))
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print(summary(de_1 <- decideTestsDGE(et_1)))
  
  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sub = subset(top_all_1$table, top_all_1$table$FDR<0.05), 
               top_all = top_all_1$table, 
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sub = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<0.05)), ]
               ))
}

########################################################################################################################

# this function is for selection of samples using 2 factors: cultivation and cultivar
# (e.g. only greenhouse, Desiree with contrast condition: control vs. stress)
levels(samplelist_modified$cultivar)
cultivation_cultivar <- samplelist_modified$cultivation_cultivar
class(cultivation_cultivar)

edgeR_dge_cultivar <- function(var1, cultivation_cultivar) {
  counts_1 <- exp_counts_genes_rounded[ ,which(samplelist_modified[, var1]==cultivation_cultivar)]
  print(colnames(counts_1))
  
  
  # keep only genes with mean =! 0 and mean above 1st Quartile
  counts_1_keep <- counts_1[which( rownames(counts_1) %in% rownames_intersect_greenhouse_field) , ]
  print(dim(counts_1_keep))
  
  
  ## group definitions  --> group = condition which is treatment (control/stress)
  #group_1 <- as.factor(subset(samplelist_modified, var1 == cultivation_cultivar)$condition)
  group_1 <- as.factor(rep(c("control", "stress"),3))
  print("group definition")
  print(group_1)
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
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
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  print(head(top_all_1_dmp))
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print(summary(de_1 <- decideTestsDGE(et_1)))
  
  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sub = subset(top_all_1$table, top_all_1$table$FDR<0.05), 
               top_all = top_all_1$table, 
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sub = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<0.05)), ]
  ))
}

########################################################################################################################

# this function is for selection of samples using 2 factors: cultivation and tolerance
# (e.g. only greenhouse, tolerant with contrast condition: control vs. stress)
levels(samplelist_modified$tolerance)

edgeR_dge_tolerance <- function(var1, cultivation_tolerance) {
  counts_1 <- exp_counts_genes_rounded[ ,which(samplelist_modified[, var1]==cultivation_tolerance)]
  print(colnames(counts_1))
  
  
  # keep only genes with mean =! 0 and mean above 1st Quartile
  counts_1_keep <- counts_1[which( rownames(counts_1) %in% rownames_intersect_greenhouse_field) , ]
  print(dim(counts_1_keep))
  
  
  ## group definitions  --> group = condition which is treatment (control/stress)
  #group_1 <- as.factor(subset(samplelist_modified, cultivation_tolerance == "field.tolerant")$condition)
  group_1 <- as.factor(rep(c("control", "control", "drought stress", "drought stress"),3))
  print("group definition")
  print(group_1)
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
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
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  print(head(top_all_1_dmp))
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print(summary(de_1 <- decideTestsDGE(et_1)))
  
  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sub = subset(top_all_1$table, top_all_1$table$FDR<0.05), 
               top_all = top_all_1$table, 
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sub = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<0.05)), ]
  ))
}


########################################################################################################################

# this function is for selection of samples using 2 factors: trial_name and cultivar
# (e.g. only MPITest2, Desiree with contrast condition: control vs. stress)
levels(samplelist_modified$cultivar)
trial_name_cultivar <- samplelist_modified$trial_name_cultivar
class(trial_name_cultivar)

edgeR_dge_trial_cultivar <- function(var1, trial_name_cultivar1, trial_name_cultivar2) {
  counts_1 <- exp_counts_genes_rounded[ ,which(samplelist_modified[, var1] %in% c(trial_name_cultivar1, trial_name_cultivar2))]
  print(colnames(counts_1))
  
  
  # keep only genes with mean =! 0 and mean above 1st Quartile
  counts_1_keep <- counts_1[which( rownames(counts_1) %in% rownames_intersect_greenhouse_field) , ]
  print(dim(counts_1_keep))
  
  
  ## group definitions  --> group = condition which is treatment (control/stress)
  #group_1 <- as.factor(subset(samplelist_modified, var1 == trial_name_cultivar)$condition)
  group_1 <- as.factor(c("control", "drought stress", "control", "drought stress"))
  print("group definition")
  print(group_1)
  
  ## generate DGEList object for 
  # If not specied by the user, the library sizes will be computed from the column sums of the counts!
  # filtered dataset "keep" (mean != 0 and > 1st Qu.)
  counts_dge_list_1 <- DGEList(counts = counts_1_keep, group = group_1)
  print("samples before normalization")
  print(head(counts_dge_list_1$samples))
  
  
  # NORMALIZATION
  counts_dge_list_1 <- calcNormFactors(counts_dge_list_1)
  print("samples after normalization")
  print(head(counts_dge_list_1$samples))
  
  
  ## Producing an MDS plot
  # An MDS plots shows distances, in terms of biological coeficient of variation (BCV), between samples.
  # plotMDS(counts_dge_list_1, main="MDS Plot")
  
  
  ## Estimating the dispersion
  ### The common dispersion estimates the overall BCV of the dataset, averaged over all genes:
  counts_dge_list_1 <- estimateCommonDisp(counts_dge_list_1, verbose=TRUE)
  # BCV (square root of the common dispersion) 
  
  
  ### Now estimate gene-specific dispersions:
  counts_dge_list_1 <- estimateTagwiseDisp(counts_dge_list_1)
  
  
  ### Plot the estimated dispersions:
  #Plot genewise biological coefficient of variation (BCV) against gene abundance (in log2 counts per million)
  #The BCV is the square root of the negative binomial dispersion. This function displays the common, trended and tagwise BCV estimates.
  # plotBCV(counts_dge_list_1)
  
  
  ## Differential Expression
  ### Compute exact genewise tests for differential expression:
  #**exactTest:** Compute genewise exact tests for differences in the means between two groups of negative-binomially distributed counts.
  #**topTags:** Extracts the top DE tags in a data frame for a given pair of groups, ranked by p-value or absolute log-fold change.
  # *--> default for option adjust.method = "BH"*
  et_1 <- exactTest(counts_dge_list_1)
  print(counts_dge_list_1$samples$group)
  
  top_1 <- topTags(et_1) # default number of tags to return: 10
  print("topTags")
  print(top_1)
  
  # Save all Tags resulting from exactTest (not sorted)
  top_all_1 <- topTags(et_1, n=25846, sort.by="none")
  
  
  # Merge all Tags results with PGSC identifier table
  top_all_1_dmp <- merge(top_all_1, assoc_pgsc, by.x = "row.names", by.y="pgsc_dmg")
  print(head(top_all_1_dmp))
  
  # Save merged table in text_output
  #write.table(top_all_greenhouse_keep_dmp, "../text_output/top_all_greenhouse_keep_dmp.txt", sep="\t")
  
  
  ### CPM table
  #**cpm:** Computes counts per million (CPM) or reads per kilobase per million (RPKM) values.
  print(cpm(counts_dge_list_1)[rownames(top_1), ])
  
  
  ### The total number of DE genes at 5% FDR is given by
  #**decideTestsDGE:** Classify a series of related differential expression statistics as up, down or not significant. A number of different multiple testing schemes are offered which adjust for multiple testing down the genes as well as across contrasts for each gene.
  #default: adjust.method="BH", p.value=0.05
  print(summary(de_1 <- decideTestsDGE(et_1)))
  
  
  ### Plot the log-fold-changes, highlighting the DE genes:
  detags_1 <- rownames(counts_dge_list_1)[as.logical(de_1)]
  plotSmear(et_1, de.tags=detags_1)
  abline(h=c(-1, 1), col="blue")
  
  ### Return
  return(list( sums = summary(de_1 <- decideTestsDGE(et_1)), 
               top_sub = subset(top_all_1$table, top_all_1$table$FDR<0.05), 
               top_all = top_all_1$table, 
               merge = top_all_1_dmp,
               cpm_all = cpm(counts_dge_list_1),
               cpm_sub = cpm(counts_dge_list_1)[rownames(subset(top_all_1$table, top_all_1$table$FDR<0.05)), ]
  ))
}


########################################################################################################################


