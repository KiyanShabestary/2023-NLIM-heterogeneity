
require(ggplot2)
require(viridis)
require(readxl)
require(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

deg_list <- read_xlsx("../source_data/fig1f/CORE_LOWvsEXT_HIGH_deg.xlsx",sheet=1)

deg_list <- deg_list %>% mutate(
  significance = if_else(((padj<=0.05) & (abs(log2FoldChange) >=1)),"True","False"),
  log10_padj = -log10(padj)
)

colour_map = c(inferno(10)[2],inferno(10)[9])

ggplot(deg_list,aes(x=log2FoldChange,y=log10_padj, colour=significance)) + 
  geom_point(size=1, alpha=0.8) +
  ylab("log2 fold change")+
  ylab("-log10 p-value")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig1_volcano_HIGH_EXT_CORE_LOW.svg", width = 5, height = 3, bg='transparent')


