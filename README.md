# 2023-NLIM-heterogeneity

This repository contains scripts intended to reproduce the main figures in Shabestary et al. 2024. 
*LINK to article*

Download source data from the link below and place its content in the source data folder within this repository. 
*LINK to source data*

The repository is organized as follow:

`ext_data` Data obtained from external repositories/papers.  
`figures` Contains the figures that should be obtained after running the script processing source data.  
`output_table` Contains tabulated data that was generated after running the script processing source data.  
`Rdata` Contains compressed data to facilitate data flow between scripts.  
`scripts` As the name implies.  
`source data` This folder should contain the source data generated throughout this study. Please download and add data from the link pasted above.  

### Fig. 1. scRNAseq data analysis & UMAP plot generation / flow cytometry data ###

scRNAseq data workup is based on two main scripts with associated utilities (function file):

`scRNAseq_pre-processing_main.R` is used to pre-process scRNAseq data from Jackson et al. 2020. It is mostly based on the othors data analysis description. We added growth score generation from Airoldi et al. 2008. For simplicity, compressed output of this file has already been generated and available under `Rdata`.

`fig1b_main.R` generates the UMAP plot and growth score distribution shown in Fig. 1b. It also generates genes that are differentially regulated across clusters as well as mean gene intensities for each cluster under `output_table`.

`fig1d.R` generates flow cytometry histogram plot (4h) for Fig. 1d as well as supplementary plots displaying cell size normalized pRPL28 fluorescence.

### Fig. 2. Flow cytometry data / Clustering / Subpopulation analysis / subpopulation RNA sequencing ###

`fig2.R` generates bulk population and subpopulation specific 2D density plot and histogram (cell size vs pRPL28 intensity). It also display the clustering and the number of cells per subpopulation per condition per time. Finally, it also generates the volcano plot of subpopRNAseq and compare scRNA sequencing data with subpopulation RNA sequencing.

### Fig. 3 Calcofluor staining ###

Helper script used for Fig. 3b plotting is available upon request.

### Fig. 4. Flow cytometry data / Growth curve / Chronological lifespan measurement ###

`fig4a.R` generates the bimodality score. Note that it is divided in two parts. First part calculated the bimodality scores using flexmix. However, due to the algorithm that can be stuck at local optima, flexmix was performed with multiple initialization (different seeds) and glocal optima results stored under `output_table`. For simplicity, part1 is shown for NLIM only and the summary file has already been generated for this step. The second part plots the scores for NLIM and NREP from the summary file (NLIM and NREP)`bimodality_scores_HO_HLM_triplicates.rds` in `Rdata`. Bimodality scores are listed in `bimodality_scores_HO_HLM_6h_triplicates_summary.csv` placed in `output_table`

`fig4b.R` generates the maximal growth rate. Export those values as a max growth file `230112_growth_params_by_group_NLIM.Rds` and `230118_growth_params_by_group_NREP.Rds` within `Rdata`.

`fig4c.R` joins the metrics obtained from `fig4a.R` and `fig4b.R` and computes the correlation between the two (tradeoff). Additionally, it generates 2 out of the 3 subplots displaying correlations in bimodality and maximal growth for NLIM vs NREP (Supplementary Fig. 20a and 20b).

`fig4de.R` displays subpopulation survival rate over time (Fig. 4d) for a selected condition and after 30d for all tested conditions (Fig. 4d). Statistical analysis is included at the end of this script. Script can be modified to display bar chart of survival rates for 10d or 20d. Subpopulation survival rates are saved in `230323_CLS_subpop.Rds` within `Rdata`.

`fig4fg.R` displays subpopulation specific growth resumption in rich (YPD) media and calculates lag time to reach two divisions for each subpopulation. Subpopulation lag times are saved in `230222_t_lags.Rds` within `Rdata`.

`fig4h.R` reads data generate as part of `fig4de.R` and `fig2fg.R`. Note that this script can be modified to display tradeoff with survival rates after 20d or 30d.

### Fig. 5. HTP microscopy ###

TF nuclear intensity computed from segmented microscopy images and BFP/GFP overlap were obtained from our developed HTP microscopy pipeline (https://github.com/Benedict-Carling/YeaZ-Output-Analysis). For simplicity, single-cell TF nuclear intensities output were placed in the `source_data/fig5bcdef` folder.

`fig5bcdef.R` generates cell size and pRPL28 fluorescence 2D density plots for glutamine and proline treatments (Fig. 5b). It also generates condition- and subpopulation- specific TF nuclear intensity (Fig. 5c and 5d). It further camputes TF nuclear intensity over time between conditions (Fig. 5e) and subpopulation (Fig. 5f). TF nuclear intensity rankings are exported as `TF_rankings.xlsx` in the `output_table` folder. 

### Fig. 6. In vivo ATP and FBP measurement using sensors ###

`fig6cde.R` creates all the sensor plots as well as associated supplementaries using Rmixmod for cluster assignment. This also contains the scripts to perform cell size/sensor output analysis across all nitrogen conditions (Supplementary Fig. 25). 



