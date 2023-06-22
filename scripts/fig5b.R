

require(ggplot2)
require(scales)
require(viridis)
require(dplyr)

# Set working directory to current location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Set file locations for each flow file
file_names <- read.csv('helper_file_names_TORC_fam_over_time.csv') 

# Convert to list to lapply
file_names <- file_names %>% split(., seq(nrow(.)), drop=T)

# Define function to pplot FSC-H and BL1-H for each condition
plot_for_each <- function(file_name){
  
  ##### TORC vs WT #####
  
  ###2h
  
  colour_map = c(inferno(10)[2],inferno(10)[4])
  
  # Define the replicates according to the helper file
  tor1.2h <- read.csv(file = file_name$TOR1_2h)
  wt.2h <- read.csv(file = file_name$WT_2h)
  
  strain <- c(rep('Δtor1',nrow(tor1.2h)),
              rep('WT alleles',nrow(wt.2h)))
  
  data.2h.master <- bind_rows(tor1.2h, wt.2h)
  data.2h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.2h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_2h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.2h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_2h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 4h
  
  # Define the replicates according to the helper file
  tor1.4h <- read.csv(file = file_name$TOR1_4h)
  wt.4h <- read.csv(file = file_name$WT_4h)
  
  strain <- c(rep('Δtor1',nrow(tor1.4h)),
              rep('WT alleles',nrow(wt.4h)))
  
  data.4h.master <- bind_rows(tor1.4h, wt.4h)
  data.4h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.4h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_4h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.4h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_4h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 6h
  
  # Define the replicates according to the helper file
  tor1.6h <- read.csv(file = file_name$TOR1_6h)
  wt.6h <- read.csv(file = file_name$WT_6h)
  
  strain <- c(rep('Δtor1',nrow(tor1.6h)),
              rep('WT alleles',nrow(wt.6h)))
  
  data.6h.master <- bind_rows(tor1.6h, wt.6h)
  data.6h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.6h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_6h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.6h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_6h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 8h
  
  # Define the replicates according to the helper file
  tor1.8h <- read.csv(file = file_name$TOR1_8h)
  wt.8h <- read.csv(file = file_name$WT_8h)
  
  strain <- c(rep('Δtor1',nrow(tor1.8h)),
              rep('WT alleles',nrow(wt.8h)))
  
  data.8h.master <- bind_rows(tor1.8h, wt.8h)
  data.8h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.8h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_8h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.8h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_tor1_8h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  
  ##### GTR1 vs WT #####
  
  colour_map = c(inferno(10)[2],inferno(10)[8])
  
  ###2h
  
  # Define the replicates according to the helper file
  gtr1.2h <- read.csv(file = file_name$GTR1_2h)
  wt.2h <- read.csv(file = file_name$WT_2h)
  
  strain <- c(rep('Δgtr1',nrow(gtr1.2h)),
              rep('WT alleles',nrow(wt.2h)))
  
  data.2h.master <- bind_rows(gtr1.2h, wt.2h)
  data.2h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.2h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_2h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.2h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_2h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 4h
  
  # Define the replicates according to the helper file
  gtr1.4h <- read.csv(file = file_name$GTR1_4h)
  wt.4h <- read.csv(file = file_name$WT_4h)
  
  strain <- c(rep('Δgtr1',nrow(gtr1.4h)),
              rep('WT alleles',nrow(wt.4h)))
  
  data.4h.master <- bind_rows(gtr1.4h, wt.4h)
  data.4h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.4h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_4h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.4h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_4h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 6h
  
  # Define the replicates according to the helper file
  gtr1.6h <- read.csv(file = file_name$GTR1_6h)
  wt.6h <- read.csv(file = file_name$WT_6h)
  
  strain <- c(rep('Δgtr1',nrow(gtr1.6h)),
              rep('WT alleles',nrow(wt.6h)))
  
  data.6h.master <- bind_rows(gtr1.6h, wt.6h)
  data.6h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.6h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_6h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.6h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_6h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 8h
  
  # Define the replicates according to the helper file
  gtr1.8h <- read.csv(file = file_name$GTR1_8h)
  wt.8h <- read.csv(file = file_name$WT_8h)
  
  strain <- c(rep('Δgtr1',nrow(gtr1.8h)),
              rep('WT alleles',nrow(wt.8h)))
  
  data.8h.master <- bind_rows(gtr1.8h, wt.8h)
  data.8h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.8h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_8h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.8h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr1_8h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  
  ##### GTR2 vs WT #####
  
  colour_map = c(inferno(10)[2],inferno(10)[6])
  
  ###2h
  
  # Define the replicates according to the helper file
  gtr2.2h <- read.csv(file = file_name$GTR2_2h)
  wt.2h <- read.csv(file = file_name$WT_2h)
  
  strain <- c(rep('Δgtr2',nrow(gtr2.2h)),
              rep('WT alleles',nrow(wt.2h)))
  
  data.2h.master <- bind_rows(gtr2.2h, wt.2h)
  data.2h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.2h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_2h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.2h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_2h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 4h
  
  # Define the replicates according to the helper file
  gtr2.4h <- read.csv(file = file_name$GTR2_4h)
  wt.4h <- read.csv(file = file_name$WT_4h)
  
  strain <- c(rep('Δgtr1',nrow(gtr2.4h)),
              rep('WT alleles',nrow(wt.4h)))
  
  data.4h.master <- bind_rows(gtr2.4h, wt.4h)
  data.4h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.4h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_4h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.4h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_4h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 6h
  
  # Define the replicates according to the helper file
  gtr2.6h <- read.csv(file = file_name$GTR2_6h)
  wt.6h <- read.csv(file = file_name$WT_6h)
  
  strain <- c(rep('Δgtr2',nrow(gtr2.6h)),
              rep('WT alleles',nrow(wt.6h)))
  
  data.6h.master <- bind_rows(gtr2.6h, wt.6h)
  data.6h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.6h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_6h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.6h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_6h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
  
  ### 8h
  
  # Define the replicates according to the helper file
  gtr2.8h <- read.csv(file = file_name$GTR2_8h)
  wt.8h <- read.csv(file = file_name$WT_8h)
  
  strain <- c(rep('Δgtr1',nrow(gtr2.8h)),
              rep('WT alleles',nrow(wt.8h)))
  
  data.8h.master <- bind_rows(gtr2.8h, wt.8h)
  data.8h.master$strain <- strain
  
  # Plot histogram GFP
  ggplot(data.8h.master, aes(x=BL1.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("GFP Fluorescence (BL1-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  BL1.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_8h_BL1.H.svg",sep='')
  
  ggsave(BL1.path, width = 3, height = 3, bg='transparent')
  
  # Plot histogram cell size
  ggplot(data.8h.master, aes(x=FSC.H, colour = strain)) + 
    geom_density(aes(fill=strain), size=1, alpha=.4)+ #after_stat(scaled)
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    ylab("Density")+
    xlab("Cell size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  FSC.path <- paste("../figures/fig5b_figS19/",file_name$name,"_gtr2_8h_FSC.H.svg",sep='')
  
  ggsave(FSC.path, width = 3, height = 3, bg='transparent')
}

lapply(file_names, plot_for_each)
