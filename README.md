# 2023-NLIM-heterogeneity

This repository contains scripts intended to reproduce the main figures in Shabestary et al. 2023. 
LINK to article

Download source data from the link below and place in the source data folder within this repository. 
LINK to source data

The repository is organized as follow:

`ext_data` Data obtained from external repositories/papers.
`figures` Contains the figures that should be obtained after running the script processing source data.
`output_table` Contains data that was generated after running the script processing source data.
`Rdata` Contains compressed data to facilitate the flow between the scripts.
`scripts` As the name implies.
`source data` This folder should contain the source data generated throughout this study. Please download and add data from the link pasted above.

**1. scRNAseq data analysis and UMAP plot generation**

This section is based on two main scripts with associated utilities (function file):

`scRNAseq_pre-processing_main.R` This script is used to pre-process scRNAseq data from Jackson et al. 2020. It is mostly based on the othors data analysis description. We added growth score generation from Airoldi et al 2008. For simplicity, compressed output of this file is already generated and available under `Rdata`

`fig1b_main.R` This script generates the UMAP plot and growth score distribution shown in Fig. 1b.




