# 1. scRNAseq data analysis
# KS 30.12.2022

# This script is used to generate Fig.1b (UMAP plots) and Fig.1 Y (Violin plot
# of scores for H and L sub-populations). Additionally, differentially expressed
# genes are calculated with DESeq2.

# Data management
require(plyr) #Important: load plyr BEFORE dplyr!
require(dplyr)
require(tidyverse)
require(xlsx)

# Plotting
require(viridis)
require(ggplot2)

# Bioconductor
require(DESeq2)

# Set working directory to current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load functions
source('fig1b_functions.R')

sce.conditions = readRDS(file = file.path("../Rdata","Jackson2020_sce_conditions_UMAP.rds"))
sce.params = readRDS(file = file.path("../Rdata","Jackson2020_sce_params_UMAP.rds"))
gene_list=readRDS(file="../ext_data/gene_list.rds")

# Visualization with gene labeling and genotype filtering -----------------
# Code used to generate Fig.1.b. Specify conditions for UMAP plot and gene marker
# or growth score for plotting


# Fetch counts and associated params for each condition
counts.glutamine <- t(sce.conditions$Glutamine@assays@data@listData[["logcounts"]]) %>% 
  as.data.frame() %>% rownames_to_column('cellID')
selection.glutamine <- sce.params$Glutamine %>% rownames_to_column('cellID') %>% inner_join(counts.glutamine)

counts.proline <- t(sce.conditions$Proline@assays@data@listData[["logcounts"]]) %>% 
  as.data.frame() %>% rownames_to_column('cellID')
selection.proline <- sce.params$Proline %>% rownames_to_column('cellID') %>% inner_join(counts.proline)

counts.YPD <- t(sce.conditions$YPD@assays@data@listData[["logcounts"]]) %>% 
  as.data.frame() %>% rownames_to_column('cellID')
selection.YPD <- sce.params$YPD %>% rownames_to_column('cellID') %>% inner_join(counts.YPD)

# Select particular genotype or cluster (optional)
selection2.glutamine <- selection.glutamine %>% filter(genotypeGroup=='WT(ho)')
#selection2 <- selection %>% filter(cluster==18)  

#### UMAP Plot - Growth score histogram ####

# Proline - Growth score
ggplot(selection.proline, aes(x=UMAP1, y=UMAP2, colour=growthScore))+
  geom_point(size=1.4,alpha=0.6)+
  scale_colour_viridis(option='B',limits = c(0.2, 0.46))+ #discrete=TRUE/limits = c(0.2, 1)
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_classic()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_UMAP_NLIMPRO_growth.svg"), width = 3, height = 3, bg='transparent')


# Proline - growth histogram
ggplot(selection.proline, aes(x=growthScore)) + 
  geom_density(size=1)+
  xlim(c(0.1,0.5))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave(file.path("../figures","fig1_PRO_GS_hist.svg"), width = 6, height = 2, bg='transparent')


# Proline - pRPL28
display.gene <- dplyr::pull(selection.proline, 'YGL103W') # Pull pRPL28 mRNA counts // Can also modify to visualize other markers
selection.proline$target <- display.gene

ggplot(selection.proline, aes(x=UMAP1, y=UMAP2, colour=target))+
  geom_point(size=1)+
  scale_colour_viridis(option='B',limits = c(0, 5.6))+ #discrete=TRUE/limits = c(0.2, 1)
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_classic()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_UMAP_NLIMPRO_RPL28.svg"), width = 3, height = 3, bg='transparent')


# Glutamine - Growth score
ggplot(selection.glutamine, aes(x=UMAP1, y=UMAP2, colour=growthScore))+
  geom_point(size=1.4,alpha=0.6)+
  scale_colour_viridis(option='B',limits = c(0.2, 0.46))+ #discrete=TRUE/limits = c(0.2, 1)
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_classic()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_UMAP_NLIMGLN_growth.svg"), width = 3, height = 3, bg='transparent')


# Glutamine - growth histogram
ggplot(selection.glutamine, aes(x=growthScore)) + 
  geom_density(size=1)+
  xlim(c(0.1,0.5))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave(file.path("../figures","fig1_GLN_GS_hist.svg"), width = 6, height = 2, bg='transparent')


# Glutamine - pRPL28
display.gene <- dplyr::pull(selection.glutamine, 'YGL103W') # Pull pRPL28 mRNA counts // Can also modify to visualize other markers
selection.glutamine$target <- display.gene

ggplot(selection.glutamine, aes(x=UMAP1, y=UMAP2, colour=target))+
  geom_point(size=1)+
  scale_colour_viridis(option='B',limits = c(0, 5.6))+ #discrete=TRUE/limits = c(0.2, 1)
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_classic()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_UMAP_NLIMGLN_RPL28.svg"), width = 3, height = 3, bg='transparent')



# Sub-population predicted growth scores -----------------------------------

# Define sub-population based on sub-clusters
cluster <- c(c(1,10,12,4,5,8,9),c(11,2,3,6,7))
subPop <- c(rep('P1', 7), rep('P2', 5))
subPop_list.proline <- data.frame(cbind(cluster=cluster, subPop=subPop))

cluster <- c(c(9,5,4,2,1,10,11,12,13,14,17,18,15),c(8,7,6,3,16))
subPop <- c(rep('G1', 13), rep('G2', 5))
subPop_list.glutamine <- data.frame(cbind(cluster=cluster, subPop=subPop))

# Merge subpopulation assignments to data
growth.by.cluster.proline <- selection.proline %>% 
  select(cellID,growthScore,genotypeGroup,cluster,UMAP1,UMAP2) %>% 
  inner_join(subPop_list.proline)

growth.by.cluster.glutamine <- selection.glutamine %>% 
  select(cellID,growthScore,genotypeGroup,cluster,UMAP1,UMAP2) %>% 
  inner_join(subPop_list.glutamine)

# Control that subpopulation assigments match UMAP
ggplot(growth.by.cluster.glutamine, aes(x=UMAP1, y=UMAP2, colour=subPop))+
  geom_point(size=1)+
  scale_colour_viridis(option='B',discrete=TRUE)+
  xlab("UMAP1")+
  ylab("UMAP2")+
  theme_classic()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"))

# Computes mean and sd of growthScores per population per genotype
growth.by.cluster.stats.proline <- growth.by.cluster.proline %>% 
  data_summary("growthScore",c("subPop","genotypeGroup"))

growth.by.cluster.stats.glutamine <- growth.by.cluster.glutamine %>% 
  data_summary("growthScore",c("subPop","genotypeGroup"))

# Differentially expressed genes with DESeq2 ------------------------------
# Separate cell by specificed subpopulation and genotype, as sepcified in 
# Supplementary Method 1. Perform DESEq2.

path_to_file=file.path(getwd(), "../output_tables/DESeq2_scRNAseq.xlsx")

# Proline
contrast.proline = c("subPop", "P2", "P1") # Specify to DESeq2 how to compare and use wrapper
deseq.table.proline = perform.DESeq2(sce.params$Proline,
                                     sce.conditions$Proline,
                                     subPop_list.proline,
                                     contrast.proline,
                                     gene_list)

write.xlsx(deseq.table.proline, file=path_to_file, sheetName="Proline_P2_P1", row.names=FALSE)

# Glutamine
contrast.glutamine = c("subPop", "G2", "G1") 
deseq.table.glutamine = perform.DESeq2(sce.params$Glutamine,
                                       sce.conditions$Glutamine,
                                       subPop_list.glutamine,
                                       contrast.glutamine,
                                       gene_list)

write.xlsx(deseq.table.glutamine, file=path_to_file, sheetName="Glutamine_G2_G1", append=TRUE, row.names=FALSE)



# Generate table with average number of reads per subpopulation -----------

# Merge subpopulation assignments to data while keeping all genes
counts.subPop.proline <- selection.proline %>% 
  inner_join(subPop_list.proline) %>%
  select(!contains("PC")) %>%
  group_by(subPop, genotypeGroup) %>%
  dplyr::summarise(across(everything(), mean),
            .groups = 'drop')  %>%
  as.data.frame()

counts.subPop.glutamine <- selection.glutamine %>% 
  inner_join(subPop_list.glutamine) %>%
  select(!contains("PC")) %>%
  group_by(subPop, genotypeGroup) %>%
  dplyr::summarise(across(everything(), mean),
                   .groups = 'drop')  %>%
  as.data.frame()


# Calculate significance level for subpopulation specific pRPL28 marker (WT case)

selection.proline.subPop <- selection.proline %>% 
  inner_join(subPop_list.proline) %>%
  select(!contains("PC")) %>%
  filter(genotypeGroup=="WT(ho)")

selection.glutamine.subPop  <- selection.glutamine %>% 
  inner_join(subPop_list.glutamine) %>%
  select(!contains("PC")) %>%
  filter(genotypeGroup=="WT(ho)")

path_to_file=file.path(getwd(), "../output_tables/Mean_counts_per_subPop.xlsx")

write.xlsx(counts.subPop.proline, file=path_to_file, sheetName="Proline", append=TRUE, row.names=FALSE)
write.xlsx(counts.subPop.glutamine, file=path_to_file, sheetName="Glutamine", append=TRUE, row.names=FALSE)

# Calculate significance levels for RPL28 for subpopulations
RPL28.P1 <- selection.proline.subPop %>% filter(subPop=="P1") %>% pull(YGL103W)
RPL28.P2 <- selection.proline.subPop %>% filter(subPop=="P2") %>% pull(YGL103W)
RPL28.G1 <- selection.glutamine.subPop %>% filter(subPop=="G1") %>% pull(YGL103W)
RPL28.G2 <- selection.glutamine.subPop %>% filter(subPop=="G2") %>% pull(YGL103W)

t.test(RPL28.P1,RPL28.P2) 
t.test(RPL28.G1,RPL28.G2)

YGL103W.Pro <- selection.proline.subPop %>% select(subPop,YGL103W)
YGL103W.Gln <- selection.glutamine.subPop %>% select(subPop,YGL103W)

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2])


#### Box plot - pRPL28 per cluster ####

ggplot(YGL103W.Pro,aes(colour=subPop, y=YGL103W, x=subPop)) + 
  geom_jitter(width=0.2)+
  geom_boxplot(position="dodge", alpha=0.5, colour="black", fill=NA, outlier.colour="transparent") +
  ylab("RPL28 reads (log normalised)")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(0,6)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

ggsave(file.path("../figures","figS2_NLIMPRO_RPL28_box.svg"), width = 3, height = 3, bg='transparent')

ggplot(YGL103W.Gln,aes(colour=subPop, y=YGL103W, x=subPop)) + 
  geom_jitter(width=0.2)+
  geom_boxplot(position="dodge", alpha=0.5, colour="black", fill=NA, outlier.colour="transparent") +
  ylab("RPL28 reads (log normalised)")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(0,6)+
  theme_bw()+
  theme(panel.background =  element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none")

ggsave(file.path("../figures","figS2_NLIMGLN_RPL28_box.svg"), width = 3, height = 3, bg='transparent')
