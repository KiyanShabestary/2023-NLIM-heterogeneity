# KS 01.11.23

require(ggplot2)
require(viridis)
require(dplyr)
require(readxl)
require(reshape2)
require(xlsx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Since two different types of data : I transform them to Z-score per condition/data type
Znorm_flow <- function(data){
  Zdata <- data %>%
    mutate(Z_size = scale(FSC.A),
           Z_RFP = scale(X7.AAD.A)) %>%
    select(Z_size,Z_RFP)
  Zdata
}
Znorm_micro <- function(data){
  Zdata <- data %>%
    mutate(Z_size = scale(size),
           Z_RFP = scale(meanRedValue)) %>%
    select(Z_size,Z_RFP)
  Zdata
}

YPD.flow <- read.csv("../source_data/Flow_vs_Microscopy_Comparison/YPD/6th April - 0 hour exponential - Flow Data.csv") %>% Znorm_flow
YPD.micro <- read.csv("../source_data/Flow_vs_Microscopy_Comparison/YPD/April 6th - 0 hour exponential microscopy data.csv") %>% Znorm_micro

PRO.flow <- read.csv("../source_data/Flow_vs_Microscopy_Comparison/PRO_4h/5th April - Proline - 4 hour Nlim - Flow Data.csv") %>% Znorm_flow
PRO.micro <- read.csv("../source_data/Flow_vs_Microscopy_Comparison/PRO_4h/April 5th - 4 hour Proline microscopy data.csv") %>% Znorm_micro

GLN.flow <- read.csv("../source_data/Flow_vs_Microscopy_Comparison/GLN_4h/6th April - GLN - 4 hour NLIM - Flow Data.csv") %>% Znorm_flow
GLN.micro <- read.csv("../source_data/Flow_vs_Microscopy_Comparison/GLN_4h/April 6th - 4 hour GLN microscopy data.csv") %>% Znorm_micro

conditions <- c(rep('YPD',nrow(YPD.flow)+nrow(YPD.micro)),
                rep('PRO',nrow(PRO.flow)+nrow(PRO.micro)),
                rep('GLN',nrow(GLN.flow)+nrow(GLN.micro)))

# Solid for flow, dashed for microscopy
method <- c(rep('flow cytometry',nrow(YPD.flow)),rep('microscope',nrow(YPD.micro)),
              rep('flow cytometry',nrow(PRO.flow)),rep('microscope',nrow(PRO.micro)),
              rep('flow cytometry',nrow(GLN.flow)),rep('microscope',nrow(GLN.micro)))

comparison <- bind_rows(YPD.flow,YPD.micro,PRO.flow,PRO.micro,GLN.flow,GLN.micro)
comparison$conditions <- conditions
comparison$method <- method

comparison$Z_size <- as.numeric(comparison$Z_size)
comparison$Z_RFP <- as.numeric(comparison$Z_RFP)

# Random down-sizing to make plotting easier
comparison.plot <- comparison %>% filter(conditions == "PRO") %>% sample_n(10000)

#colour_map = c(inferno(10)[2],inferno(10)[9])

ggplot(comparison.plot, aes(x=Z_size, y=Z_RFP,colour=method)) + 
  geom_density_2d(aes())+
  #xlim(8000,200000)+  
  xlab("Z-score cell size")+
  ylab("Z-score RFP")+
  theme_bw()+
  #facet_grid(factor(conditions, levels=c("GLN","PRO"))~factor(time, levels=c('2h', '4h', '6h')))+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("../figures/fig5d_SUPP_COMP_flow_cytometry.svg", width = 5, height = 4, bg='transparent')


