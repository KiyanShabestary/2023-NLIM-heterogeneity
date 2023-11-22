# For histogram with FSC.H -> replace BL1.H with FSC.H when calling ggplot


require(ggplot2)
require(scales)
require(viridis)
require(dplyr)


# Set working directory to current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 



# Fig1d -------------------------------------------------------------------


YPD.1840 <- read.csv(file = '../source_data/fig1d/220707_1840_YPD.csv')
PRO.1830 <- read.csv(file = '../source_data/fig1d/221121_1830_E1_PRO_GFP.csv')
GLN.1830 <- read.csv(file = '../source_data/fig1d/221121_1830_E2_GLN_GFP.csv')

conditions <- c(rep('YPD',nrow(YPD.1840)),
              rep('NLIM-PRO',nrow(PRO.1830)),
              rep('NLIM-GLN',nrow(GLN.1830)))

master <- bind_rows(YPD.1840, PRO.1830, GLN.1830)
master$conditions <- conditions

master$BL1_FSC.H <- master$BL1.H/master$FSC.H

ggplot(master, aes(x=BL1.H, colour = conditions)) + # Replace BL1.H with FSC.H
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  scale_colour_viridis(option='inferno',discrete=TRUE)+
  ylab("Density (a.u.)")+
  xlab("pRPL28 fluorescence (BL1-H)")+
  theme_classic()+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1d_BL1_H.svg"), width = 3, height = 3, bg='transparent')

ggplot(master, aes(x=FSC.H, colour = conditions)) + # Replace BL1.H with FSC.H
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  scale_colour_viridis(option='inferno',discrete=TRUE)+
  ylab("Density (a.u.)")+
  xlab("Cell size (FSC-H)")+
  theme_classic()+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1d_FSC_H.svg"), width = 3, height = 3, bg='transparent')


# Supp figs ---------------------------------------------------------------

ggplot(master, aes(x=FSC.H, y=BL1.H, colour = conditions)) + 
  geom_density_2d()+
  scale_colour_viridis(option='viridis',discrete=TRUE)+
  xlim(10000,NA)+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(file.path("../figures","fig1d_Supp_GFP_size.svg"), width = 3, height = 3, bg='transparent')


ggplot(master, aes(x=FSC.H, y=BL1_FSC.H, colour = conditions)) + 
  geom_density_2d()+
  scale_colour_viridis(option='viridis',discrete=TRUE)+
  xlim(10000,NA)+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H) / Cell size (FSC-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") 

ggsave(file.path("../figures","fig1d_Supp_GFP_size_size.svg"), width = 2.5, height = 3, bg='transparent')


# Clustering on cell size and GFP
clustered_NLIMPRO <- flexmix_MV_clustering(master%>%filter(conditions=="NLIM-PRO"),dimensions=c("FSC.H","BL1.H"))
clustered_NLIMGLN <- flexmix_MV_clustering(master%>%filter(conditions=="NLIM-GLN"),dimensions=c("FSC.H","BL1.H"))

# Clustered scatter
clustered_data_plot <- clustered_NLIMPRO %>% filter(classification == "High")

ggplot(clustered_data_plot, aes(x=FSC.H, y=BL1_FSC.H)) + 
  geom_density2d(aes(colour=classification),bins=6, alpha=0.6)+
  xlim(8000,200000)+  
  ylim(0.36,1.0)+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H) / Cell size (FSC-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(file.path("../figures","fig1d_Supp_GFP_size_size_PRO_HIGH.svg"), width = 2.5, height = 3, bg='transparent')

# Merging Proline and Glutamine
conditions <- c(rep("NLIM-PRO",nrow(clustered_NLIMPRO)),
                rep("NLIM-GLN",nrow(clustered_NLIMGLN)))

data <- bind_rows(clustered_NLIMPRO,clustered_NLIMGLN)

data$conditions <- conditions

colour_map = c(inferno(10)[8],inferno(10)[6])

data_plot <- data %>% filter(classification != "Unknown")

ggplot(data_plot,aes(y=BL1_FSC.H, x=conditions,color=factor(classification), fill=factor(classification))) + 
  geom_boxplot(aes(y=BL1_FSC.H, x=conditions,color=factor(classification)),colour="black",outlier.colour=NA, position=position_dodge(width=0.9))+
  ylab("pRPL28 fluorescence (BL1-H) / Cell size (FSC-H)")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")


p1 = clustered_NLIMGLN %>% filter(classification == "Low") %>% pull(BL1_FSC.H)
p2 = clustered_NLIMGLN %>% filter(classification == "High") %>% pull(BL1_FSC.H)
t.test(p1, p2, paired = FALSE, alternative = "two.sided")

p1 = clustered_NLIMPRO %>% filter(classification == "Low") %>% pull(BL1_FSC.H)
p2 = clustered_NLIMPRO %>% filter(classification == "High") %>% pull(BL1_FSC.H)
t.test(p1, p2, paired = FALSE, alternative = "two.sided")

ggsave(file.path("../figures","fig1d_Supp_differences_GFP_size.svg"), width = 2.5, height = 3, bg='transparent')
