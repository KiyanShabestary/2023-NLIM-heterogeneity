# For histogram with FSC.H -> replace BL1.H with FSC.H when calling ggplot


require(ggplot2)
require(scales)
require(viridis)
require(dplyr)


# Set working directory to current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# YPD
YPD.1640 <- read.csv(file = '../source_data/fig1cde/220707_1640_YPD.csv')
YPD.1840 <- read.csv(file = '../source_data/fig1cde/220707_1840_YPD.csv')
YPD.2040 <- read.csv(file = '../source_data/fig1cde/220707_2040_YPD.csv')
YPD.2240 <- read.csv(file = '../source_data/fig1cde/220707_2240_YPD.csv')

YPD_time <- c(rep('2h',nrow(YPD.1640)),
              rep('4h',nrow(YPD.1840)),
              rep('6h',nrow(YPD.2040)),
              rep('8h',nrow(YPD.2240)))

YPD.master <- bind_rows(YPD.1640, YPD.1840, YPD.2040, YPD.2240)
YPD.master$Time <- YPD_time

# Plot histogram
ggplot(YPD.master, aes(x=BL1.H, colour = Time)) + # Replace BL1.H with FSC.H
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  scale_colour_viridis(option='viridis',discrete=TRUE)+
  ylab("Density")+
  xlab("GFP intensity (a.u.)")+
  theme_classic()+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_YPD_BL1.H.svg"), width = 3, height = 3, bg='transparent')


# PROLINE
PRO.1630 <- read.csv(file = '../source_data/fig1cde/221121_1630_A1_PRO_GFP.csv')
PRO.1830 <- read.csv(file = '../source_data/fig1cde/221121_1830_E1_PRO_GFP.csv')
PRO.2030 <- read.csv(file = '../source_data/fig1cde/221121_2030_A1_PRO_GFP.csv')
PRO.2230 <- read.csv(file = '../source_data/fig1cde/221121_2230_E1_PRO_GFP.csv')

PRO_time <- c(rep('2h',nrow(PRO.1630)),
              rep('4h',nrow(PRO.1830)),
              rep('6h',nrow(PRO.2030)),
              rep('8h',nrow(PRO.2230)))

PRO.master <- bind_rows(PRO.1630, PRO.1830, PRO.2030, PRO.2230)
PRO.master$Time <- PRO_time

# Plot histogram
ggplot(PRO.master, aes(x=BL1.H, colour = Time)) + # Replace BL1.H with FSC.H
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  scale_colour_viridis(option='viridis',discrete=TRUE)+
  ylab("Density")+
  xlab("GFP intensity (a.u.)")+
  theme_classic()+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_PRO_BL1.H.svg"), width = 3, height = 3, bg='transparent')


# GLUTAMINE
GLN.1630 <- read.csv(file = '../source_data/fig1cde/221121_1630_A2_GLN_GFP.csv')
GLN.1830 <- read.csv(file = '../source_data/fig1cde/221121_1830_E2_GLN_GFP.csv')
GLN.2030 <- read.csv(file = '../source_data/fig1cde/221121_2030_A2_GLN_GFP.csv')
GLN.2230 <- read.csv(file = '../source_data/fig1cde/221121_2230_E2_GLN_GFP.csv')

GLN_time <- c(rep('2h',nrow(GLN.1630)),
              rep('4h',nrow(GLN.1830)),
              rep('6h',nrow(GLN.2030)),
              rep('8h',nrow(GLN.2230)))

GLN.master <- bind_rows(GLN.1630, GLN.1830, GLN.2030, GLN.2230)
GLN.master$Time <- GLN_time

# Plot histogram
ggplot(GLN.master, aes(x=BL1.H, colour = Time)) + # Replace BL1.H with FSC.H
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  scale_colour_viridis(option='viridis',discrete=TRUE)+
  ylab("Density")+
  xlab("GFP intensity (a.u.)")+
  theme_classic()+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave(file.path("../figures","fig1_GLN_BL1.H.svg"), width = 3, height = 3, bg='transparent')



PRO.master.plot <- PRO.master %>% filter(Time=="2h" | Time=="8h")

# Plot scatter - 

ggplot(PRO.master.plot, aes(x=FSC.H, y=BL1.H, colour = Time)) + 
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

ggsave("../figures/fig1_NLIMPRO_scatter.svg", width = 4.8, height = 3, bg='transparent')


