# 1. Pre-processing
# KS 31.12.2022

# This first script pre-processes Jackson2020 as described in the original paper.
# Modification from the original code includes normalization with default sce
# function logNormCounts and calculation of predicted growth rates as described
# below:
# scRNAseq data is fitted to the growth regression model from Airoldi2008 to 
# compute predicted growth scores.
# Data is stored in a SingleCellExperiment container to facilitate downstream 
# analysis

# Load packages

# Bioconductor (using BiocManager)
require(scater)
require(scran) 
require(SingleCellExperiment)
require(SummarizedExperiment)
require(BiocParallel)

# Plotting
require(igraph)
require(ggplot2)
require(viridis)

# Others
require(svglite)

# Set working directory to current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load functions
source('scRNAseq_pre-processing_functions.R')

## Jackson2020 normalization, UMAP and growth scores calculation.

# Load dataset as sce type. Obtained from Jackson2020 supplementary (figure_3.R)
sce.conditions <- readRDS(file = file.path("../ext_data","Jackson2020_sce.rds"))

# Normalize dataset using sce default logNorm normalization function
sce.conditions <- lapply(sce.conditions, logNormCounts)

# Add PCA and UMAP
sce.conditions <- lapply(sce.conditions, function(x) {runPCA(x, ncomponents=50)})
sce.conditions <- lapply(sce.conditions, function(x) {runUMAP(x, dimred = 'PCA', external_neighbors=TRUE)})
# Add growth scores
sce.conditions <- lapply(sce.conditions, add.growthScores, frmeGRParameters=frmeGRParameters, lsCalibration=lsCalibration)
# Cluster based on PCA/UMAP
sce.conditions <- lapply(sce.conditions, function(x) {apply.snn.cluster(x,'UMAP')}) #PCA

# Pull all parameters in one dataframe per condition for further analysis and plotting
sce.params <- lapply(sce.conditions, get.params)

# Save output for downstream analysis
saveRDS(sce.conditions,file = file.path("../Rdata","Jackson2020_sce_conditions_UMAP.rds"))
saveRDS(sce.params,file = file.path("../Rdata","Jackson2020_sce_params_UMAP.rds"))

