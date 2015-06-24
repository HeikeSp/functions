func_deseq2_pipeline_trial <- function(dds_object,  val1){
  
  # subset of relevant columns
  dds_sub <- dds_object[ , dds_object$trial == val1]
  
  # drop levels
  dds_sub$cultivation <-  droplevels(dds_sub$cultivation)
  dds_sub$trial <-        droplevels(dds_sub$trial)
  dds_sub$trial_name <-   droplevels(dds_sub$trial_name)
  
  print(dds_sub)
  
  # run the pipeline
  dds_sub <- DESeq(dds_sub)
  
  res_sub <- results(dds_sub)
  print(res_sub)
  
  # subset of significant results
  resSig_sub <- res_sub[ which(res_sub$padj < 0.05 ), ]
  
  print("number of significant results table:")
  print(dim(resSig_sub)[1])
  
  # down/up regulated genes
  resSig_sub_down <- res[ which(res_sub$padj < 0.05 & res_sub$log2FoldChange<0), ]
  resSig_sub_up <- res[ which(res_sub$padj < 0.05 & res_sub$log2FoldChange>0), ]
  
  print("dimensions of significantly down-regulated genes:")
  print(dim(resSig_sub_down)[1])
  print("dimensions of significantly up-regulated genes:")
  print(dim(resSig_sub_up)[1])
  
  # rlog-transformation
  rld_sub <- rlog(dds_sub)
  
  # sample distance
  sampleDists_sub <- dist( t( assay(rld_sub) ) )
  	sampleDistMatrix_sub <- as.matrix( sampleDists_sub )
  rownames(	sampleDistMatrix_sub) <- paste( rld_sub$condition,
                                            rld_sub$cultivar, sep="-" )
  #colnames(	sampleDistMatrix_sub) <- NULL
  colnames(	sampleDistMatrix_sub) <- rownames(	sampleDistMatrix_sub) 
    
  # topVarGenes
  topVarGenes_sub <- head( order( rowVars( assay(rld_sub) ), decreasing=TRUE ), 50 )
  #topVarGenes_sub <- head( order( resSig_sub$padj ), 50)
  
  # return list ob results objects
  res_list <- list(dds = dds_sub,#1
                   res = res_sub,#2
                   resSig = resSig_sub,#3
                   resSig_down = resSig_sub_down,#4
                   resSig_up = resSig_sub_up,#5
                   rld = rld_sub,#6
                   sampleDistMatrix = sampleDistMatrix_sub,#7
                   topVarGenes = topVarGenes_sub)#8
  return(res_list)
}

func_deseq2_plots <- function(filename, res){
  pdf(filename, width=6, height=6)
  
  plotMA(res[[2]], ylim=c(-5, 5))
  plotDispEsts( res[[1]])
  hist( res[[2]]$pvalue, breaks=20, col="grey" )
  
  plot( log2( 1+counts(res[[1]], normalized=TRUE)[, 1:2] ), col="#00000020", pch=20, cex=0.3 )
  plot( assay(res[[6]])[, 1:2], col="#00000020", pch=20, cex=0.3 )
  
  colours = colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
  print( heatmap.2( res[[7]], trace="none", col=colours, margins =c(6,6)))
   
  ramp <- 1:2/2
  cols <- c(rgb(ramp, 0, 0),
            rgb(0, 0, ramp),
            rgb(ramp, 0, ramp),
            rgb(0, ramp, 0))
  print( plotPCA( res[[6]], intgroup = c( "cultivar", "condition"), col=cols ) )
  
  print( heatmap.2( assay(res[[6]])[ res[[8]], ], scale="row",
                    trace="none", dendrogram="both",
                    col = colorRampPalette( rev(brewer.pal(9, "RdBu")) )(255)))
  
  dev.off()
}

func_deseq2_pipeline <- function(dds_sub){
    
  # drop levels
  dds_sub$cultivation <-  droplevels(dds_sub$cultivation)
  dds_sub$trial <-        droplevels(dds_sub$trial)
  dds_sub$trial_name <-   droplevels(dds_sub$trial_name)
  dds_sub$cultivar <-     droplevels(dds_sub$cultivar)
  dds_sub$tolerance <-    droplevels(dds_sub$tolerance)
  
  print(dds_sub)
  
  # run the pipeline
  dds_sub <- DESeq(dds_sub)
  
  res_sub <- results(dds_sub)
  print(res_sub)
  
  # subset of significant results
  resSig_sub <- res_sub[ which(res_sub$padj < 0.05 ), ]
  
  print("number of significant results table:")
  print(dim(resSig_sub)[1])
  
  # down/up regulated genes
  resSig_sub_down <- resSig_sub[ which(resSig_sub$log2FoldChange<0), ]
  resSig_sub_up <- resSig_sub[ which(resSig_sub$log2FoldChange>0), ]
  
  print("dimensions of significantly down-regulated genes:")
  print(dim(resSig_sub_down)[1])
  print("dimensions of significantly up-regulated genes:")
  print(dim(resSig_sub_up)[1])
  
  # rlog-transformation
  rld_sub <- rlog(dds_sub)
  
  # sample distance
  sampleDists_sub <- dist( t( assay(rld_sub) ) )
  sampleDistMatrix_sub <- as.matrix( sampleDists_sub )
  rownames(	sampleDistMatrix_sub) <- paste( rld_sub$condition,
                                            rld_sub$cultivar, sep="-" )
  #colnames(	sampleDistMatrix_sub) <- NULL
  colnames(	sampleDistMatrix_sub) <- rownames(	sampleDistMatrix_sub) 
  
  # topVarGenes
  topVarGenes_sub <- head( order( rowVars( assay(rld_sub) ), decreasing=TRUE ), 50 )
  #topVarGenes_sub <- head( order( resSig_sub$padj ), 50)
  
  # return list ob results objects
  res_list <- list(dds = dds_sub,#1
                   res = res_sub,#2
                   resSig = resSig_sub,#3
                   resSig_down = resSig_sub_down,#4
                   resSig_up = resSig_sub_up,#5
                   rld = rld_sub,#6
                   sampleDistMatrix = sampleDistMatrix_sub,#7
                   topVarGenes = topVarGenes_sub)#8
  return(res_list)
}


