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

### Fig. 1. scRNAseq data analysis & UMAP plot generation / flow cytometry data / subpopRNAseq ###

scRNAseq data workup is based on two main scripts with associated utilities (function file):

`scRNAseq_pre-processing_main.R` This script is used to pre-process scRNAseq data from Jackson et al. 2020. It is mostly based on the othors data analysis description. We added growth score generation from Airoldi et al. 2008. For simplicity, compressed output of this file has already been generated and available under `Rdata`.

`fig1b_main.R` This script generates the UMAP plot and growth score distribution shown in Fig. 1b. It also generates genes that are differentially regulated across clusters as well as mean gene intensities for each cluster under `output_table`.

`fig1cde.R` This script generates flow cytometry plot for Fig. 1c, 1d and 1e.

`fig1f.R` This script generates the volcano plot of subpopRNAseq

### Fig. 2. Flow cytometry data / Growth curve / Chronological lifespan measurement ###

`fig2a.R` This script generates the bimodality score. Note that it is divided in two parts. First part calculated the bimodality scores using flexmix. However, due to the algorithm that can be stuck at local optima, flexmix was performed with multiple initialization (different seeds) and glocal optima results stored under `output_table`. For simplicity, part1 is shown for NLIM only and the summary file has already been generated for this step. The second part plots the scores for NLIM and NREP from the summary file (NLIM and NREP)`bimodality_scores_HO_HLM_triplicates.rds` in `Rdata`.

`fig2b.R` This script generates the maximal growth rate. Export those values as a max growth file `230112_growth_params_by_group_NLIM.Rds` and `230118_growth_params_by_group_NREP.Rds` within `Rdata`.

`fig2c.R` This script joins the metrics obtained from `fig2a.R` and `fig2b.R` and computes the correlation between the two (tradeoff). Additionally, it generates 2 out of the 3 subplots of figS13 displaying correlations in bimodality and maximal growth for NLIM vs NREP.

`fig2d.R` This script plots population survival rates.

`fig2ef.R` This script displays subpop survival rate over time (fig2e) for a selected condition and after 30d for all tested conditions. At the end significance are calculated. Script can be modified to display bar chart of survival rates for 10d or 20d. Subpopulation survival rates are saved in `230323_CLS_subpop.Rds` within `Rdata`.

`fig2gh.R` This script displays subpopulation specific growth resumption in rich (YPD) media and calculates lag time to reach two divisions for each subpopulation. Subpopulation lag times are saved in `230222_t_lags.Rds` within `Rdata`.

`fig2i.R` This scripts reads data generate as part of `fig2ef.R` and `fig2gh.R`. Note that this script can be modified to display tradeoff with survival rates after 20d or 30d.


