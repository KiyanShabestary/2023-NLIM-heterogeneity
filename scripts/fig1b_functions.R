


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

perform.DESeq2 <- function(params,conditions,subPop_list,contrast,gene_list){
  
  sampleInfo <- params %>% rownames_to_column('cellID') %>% select(cellID,cluster,genotypeGroup) %>%
    inner_join(subPop_list)
  
  # Compute pseudo-bulk counts for the subpopulation
  counts.for.DESeq2 = cbind(t(conditions@assays@data@listData[["counts"]] %>% as.data.frame()),sampleInfo) %>%
    select(-cellID, -cluster) %>% group_by(genotypeGroup,subPop) %>%
    summarise_each(funs(sum)) # dplyr constantly depreciates this part, might need to update in the near future. In brief, sums reads for each subpop/genotype pair
  
  counts <- t(counts.for.DESeq2[,3:ncol(counts.for.DESeq2)]) %>% as.data.frame()
  
  # Sample represent the sum of reads for each genotype for each condition
  sample = paste(rep('S', nrow(counts.for.DESeq2)),1:nrow(counts.for.DESeq2),sep='')
  
  colnames(counts) <- sample
  counts <- rownames_to_column(counts,'gene')
  
  #Add Identifier
  colData <- data.frame(cbind(sample=sample,genotypeGroup=counts.for.DESeq2$genotypeGroup,subPop=counts.for.DESeq2$subPop))
  
  
  dds <- DESeqDataSetFromMatrix(countData=counts, 
                                colData=colData, 
                                design=~subPop, tidy = TRUE)
  
  # Run DESeq2
  dds <- DESeq(dds)
  
  res <- results(dds, contrast = contrast)
  
  # Write file
  master_table <- data.frame("locus"=rownames(res),"log2FC"=res[,2],"pvalue"=res[,5],"padj"=res[,6]) %>%
    inner_join(gene_list,by = c("locus" = "gene.systematicName"))
  
  
  return(master_table)
}