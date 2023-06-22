library(dplyr)
library(tidyverse)

# Load growth model
load(file = file.path("../ext_data","Airoldi2009.RData"))

apply.snn.cluster <- function(sce, dimred) {
  snn.gr <- scran::buildSNNGraph(sce, use.dimred=dimred)
  clusters <- igraph::cluster_louvain(snn.gr)
  sce$cluster <- as.factor(clusters$membership)
  return(sce)
}

add.growthScores <- function(sce, countStr='logcounts', frmeGRParameters, lsCalibration){
  normCounts <- assay(sce, countStr) %>% as.data.frame()
  sce$growthScore <- calculateRates(normCounts, frmeGRParameters, lsCalibration)
  return(sce)
}

get.params <- function(sce){
  params <- data.frame(sce$growthScore,sce$Genotype_Group,sce$cluster,sce@int_colData@listData[["reducedDims"]]@listData[["UMAP"]],sce@int_colData@listData[["reducedDims"]]@listData[["PCA"]]) %>%
    rename('growthScore' = 'sce.growthScore','genotypeGroup' = 'sce.Genotype_Group','cluster' = 'sce.cluster','UMAP1' = 'X1', 'UMAP2' = 'X2')
  params
}

