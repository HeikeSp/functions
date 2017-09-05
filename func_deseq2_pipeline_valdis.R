
func_deseq2_pipeline <- function(counts, samplelist, design_value = "without batch"){

  # Construct a DESeqDataSet
  if(design_value == "without batch"){
      dds_obj <- DESeqDataSetFromMatrix(
        countData = counts,
        colData = samplelist,
        design = ~ yield_potential)
  } else if(design_value == "with batch"){
      dds_obj <- DESeqDataSetFromMatrix(
        countData = counts,
        colData = samplelist,
        design = ~ batch + yield_potential)
  } else {
      dds_obj <- DESeqDataSetFromMatrix(
        countData = counts,
        colData = samplelist,
        design = ~ treatment)
  }
  
  print(dds_obj)
  
  dds <- DESeq(dds_obj, parallel = TRUE)

  vsd <- varianceStabilizingTransformation(dds)
  vsd_fast <- vst(dds)

  res_up <- results(dds, parallel = TRUE, 
                     contrast = c("yield_potential","high yield","low yield"),
                     altHypothesis = c("greater"), alpha = 0.1)
  
  res_down <- results(dds, parallel = TRUE, 
                    contrast = c("yield_potential","high yield","low yield"),
                    altHypothesis = c("less"), alpha = 0.1)
  
  summary(res_up)
  summary(res_down)

  # resSig <- res[ which(res$padj < 0.1), ]
  # dim(resSig) # 6
  # # strongest downregulation
  # head( resSig_E3C[ order( resSig_E3C$log2FoldChange ), ] )
  # # strongest upregulation
  # tail( resSig_E3C[ order( resSig_E3C$log2FoldChange ), ] )
}

# test_res <- func_deseq2_pipeline(counts_keep, samplelist_ordered, design_value = "without batch")
# design(test_res)

# deseq_res_control <- func_deseq2_pipeline(counts_C, samplelist_C, design_value = "without batch")
# deseq_res_drought <- func_deseq2_pipeline(counts_D, samplelist_D, design_value = "without batch")
