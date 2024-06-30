

require(ggplot2)
require(scales)
require(viridis)
require(dplyr)
require(Rmixmod)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Functions
plot_hist <- function(dataframe,xval,xtitle){
  
  p <- ggplot(dataframe, aes(x=.data[[xval]])) + 
    geom_density(size=1, alpha=.4)+
    ylab("Density")+
    xlab(xtitle)+
    theme_bw()+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  print(p)
}
plot_scatter <- function(dataframe,xval,yval,xtitle,ytitle){
  
  p <- ggplot(dataframe, aes(x=.data[[xval]],y=.data[[yval]])) + 
    geom_point(size=1, alpha=.1,colour="grey")+
    ylab(ytitle)+
    xlab(xtitle)+
    theme_bw()+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  print(p)
}
plot_hist_2c <- function(dataframe,xval,colval,xtitle,xlim,ylim,levels){
  
  col_fct = factor(dataframe[[colval]], levels=levels) # Order on plot
  dataframe$col_fct = col_fct
  
  colour_map = c(inferno(10)[2],inferno(10)[9])
  
  p <- ggplot(dataframe, aes(x=.data[[xval]],colour=col_fct,fill=col_fct)) + 
    geom_density(size=1, alpha=.4)+
    ylab("Density")+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    xlim(0,xlim)+
    ylim(0,ylim)+
    xlab(xtitle)+
    theme_bw()+
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())#legend.position = "none"
  
  print(p)
}
plot_scatter_2c <- function(dataframe,xval,yval,colval,xtitle,ytitle,alpha,xlim,ylim,levels){
  
  col_fct = factor(dataframe[[colval]], levels=levels) # Order on plot
  dataframe$col_fct = col_fct
  
  colour_map = c(inferno(10)[9],inferno(10)[2])
  
  p <- ggplot(dataframe, aes(x=.data[[xval]],y=.data[[yval]],colour=col_fct)) + 
    geom_point(size=1, alpha=alpha)+
    ylab(ytitle)+
    xlab(xtitle)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    theme_bw()+
    xlim(0,xlim)+
    ylim(0,ylim)+
    theme(text = element_text(size=10),
          panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())#legend.position = "none"
  
  print(p)
}
plot_contour_2c <- function(dataframe,xval,yval,colval,xtitle,ytitle,alpha,xlim,ylim,levels){
  
  col_fct = factor(dataframe[[colval]], levels=c("NLIMGLN","NLIMPRO")) # Order on plot
  dataframe$col_fct = col_fct
  
  colour_map = c(inferno(10)[9],inferno(10)[2])
  
  p <- ggplot(dataframe, aes(x=.data[[xval]],y=.data[[yval]],colour=col_fct)) + 
    geom_density_2d()+
    ylab(ytitle)+
    xlab(xtitle)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    theme_bw()+
    xlim(0,xlim)+
    ylim(0,ylim)+
    theme(text = element_text(size=10),
          panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  print(p)
}
perform_clustering_Rmixmod <- function(data,model){
  
  k <- 2
  
  model_list <- mixmodGaussianModel(listModels = model)
  
  # Perform clustering using Rmixmod's mixmodCluster function
  mixmod_clusters <- mixmodCluster(data, nbCluster = k, model=model_list) #as.data.frame(scaled_data)
  
  plot(mixmod_clusters)
  
  # Get the cluster labels (vector)
  clustering <- mixmod_clusters["bestResult"]["partition"]
  
  clustered_data <- cbind(data,clustering)
  
  clustered_mean <- tapply(clustered_data$FSC.H, clustered_data$clustering, mean)
  
  clustered_data <- clustered_data %>% mutate(
    classification = if_else(clustering==which.min(clustered_mean)[[1]],"Low","High")
  )
  
  
}
return_Rmixmod <- function(data,model){
  
  k <- 2
  
  model_list <- mixmodGaussianModel(listModels = model)
  
  # Perform clustering using Rmixmod's mixmodCluster function
  mixmod_clusters <- mixmodCluster(data, nbCluster = k, model=model_list) #as.data.frame(scaled_data)
  
  mixmod_clusters
}
trim_below <- function(dataframe,trim_time){
  dataframe <- dataframe %>% filter(Time>trim_time)
}
pull_sc_values <- function(file_names){
  "Pull data into a single dataframe per condition"
  
  file_name <- file.path("../source_data/fig6cde",file_names$file)
  print(file_name)
  data <- read.csv(file = file_name)
  
  data$timepoint <- c(rep(file_names$timepoint,nrow(data)))
  data$condition <- c(rep(file_names$condition,nrow(data)))  
  data$replicate <- c(rep(file_names$replicate,nrow(data))) 
  
  data
}

# Comparing two conditions ------------------------------------------------

########### yAT1.03 VL2/VL1 ##########

data.cond1 <- read.csv(file = "../source_data/fig6cde/1430_A1_yATP_NLIMPRO.csv") #1hr post-shift
data.cond2 <- read.csv(file = "../source_data/fig6cde/1430_A3_yATP_NLIMGLN.csv")

data.cond1 <- read.csv(file = "../source_data/fig6cde/1530_C2_yATP_NLIMPRO.csv") #2hr post-shift
data.cond2 <- read.csv(file = "../source_data/fig6cde/1530_C4_yATP_NLIMGLN.csv")

data.cond1 <- read.csv(file = "../source_data/fig6cde/1730_E1_yATP_NLIMPRO.csv") #4hr post-shift
data.cond2 <- read.csv(file = "../source_data/fig6cde/1730_E3_yATP_NLIMGLN.csv")

data.cond1 <- read.csv(file = "../source_data/fig6cde/1930_G1_yATP_NLIMPRO.csv") #6hr post-shift
data.cond2 <- read.csv(file = "../source_data/fig6cde/1930_G3_yATP_NLIMGLN.csv")

data.cond1 <- read.csv(file = "../source_data/fig6cde/2130_A1_yATP_NLIMPRO.csv") #8hr post-shift
data.cond2 <- read.csv(file = "../source_data/fig6cde/2130_A3_yATP_NLIMGLN.csv")

condition <- c(rep('NLIMPRO',nrow(data.cond1)),
               rep('NLIMGLN',nrow(data.cond2)))

data.master <- bind_rows(data.cond1,
                         data.cond2)

data.master$condition <- condition

data.master$yATP.A <- data.master$VL2.A/data.master$VL1.A
data.master$yATP.H <- data.master$VL2.H/data.master$VL1.H

# Plot scatter size/ATP
plot_scatter_2c(data.master,"FSC.H","yATP.H","condition","Cell size (FSC-H)","yATP output (VL2-H/VL1-H)",.02,280000,8,c("NLIMGLN","NLIMPRO"))

# Plot histogram size
plot_hist_2c(data.master,"FSC.H","condition","Cell size (FSC-H)",280000, 0.000020,c("NLIMGLN","NLIMPRO"))
ggsave("../figures/fig6a_size_2h_ATP_PRO_GLN_rep1.svg", width = 5, height = 5, bg='transparent')

# Plot histogram ATP
plot_hist_2c(data.master,"yATP.H","condition","yATP output (VL2-H/VL1-H)",4.5, 2,c("NLIMGLN","NLIMPRO"))
ggsave("../figures/fig6a_ATP_2h_ATP_PRO_GLN_rep2_4_5.svg", width = 5, height = 5, bg='transparent')

# Plot contour
plot_contour_2c(data.master,"FSC.H","yATP.H","condition","Cell size (FSC-H)","yATP output (VL2-H/VL1-H)",.02,280000,4.5,c("NLIMPRO","NLIMGLN"))
ggsave("../figures/fig6a_contour_2h_ATP_PRO_GLN_rep2_ylim_4_5.svg", width = 5, height = 5, bg='transparent')


########### FBP YL2/BL1 ##########

# 4hr post-shift
data.cond1 <- read.csv(file = "../source_data/fig6cde/1650_A9_FBPs1_NLIMPRO.csv") %>% trim_below(10)
data.cond2 <- read.csv(file = "../source_data/fig6cde/1650_A11_FBPs1_NLIMGLN.csv") %>% trim_below(10)

# 6hr post-shift
data.cond1 <- read.csv(file = "../source_data/fig6cde/1850_C9_FBPs1_NLIMPRO.csv") %>% trim_below(10)
data.cond2 <- read.csv(file = "../source_data/fig6cde/1850_C11_FBPs1_NLIMGLN.csv") %>% trim_below(10)

# 8hr post-shift
data.cond1 <- read.csv(file = "../source_data/fig6cde/2050_G9_FBPs1_NLIMPRO.csv") %>% trim_below(13)
data.cond2 <- read.csv(file = "../source_data/fig6cde/2050_G11_FBPs1_NLIMGLN.csv") %>% trim_below(13)

condition <- c(rep('NLIMPRO',nrow(data.cond1)),
               rep('NLIMGLN',nrow(data.cond2)))

data.master <- bind_rows(data.cond1,
                         data.cond2)

data.master$condition <- condition

data.master$FBPs1.H <- data.master$YL2.H/data.master$BL1.H

# Plot scatter size/FBP
plot_scatter_2c(data.master,"FSC.H","FBPs1.H","condition","Cell size (FSC-H)","FBP sensor output (BL1-H/YL2-H)",.02,280000,20,c("NLIMGLN","NLIMPRO"))
plot_contour_2c(data.master,"FSC.H","FBPs1.H","condition","Cell size (FSC-H)","FBP sensor output (BL1-H/YL2-H)",.02,280000,20,c("NLIMGLN","NLIMPRO"))


# Multiple timepoints ATP -------------------------------------------------

data.2h <- read.csv(file = "../source_data/fig6cde/1530_C1_yATP_NLIMPRO.csv") #2h
data.4h <- read.csv(file = "../source_data/fig6cde/1730_E1_yATP_NLIMPRO.csv") #4h
data.6h <- read.csv(file = "../source_data/fig6cde/1930_G1_yATP_NLIMPRO.csv") #6h
data.8h <- read.csv(file = "../source_data/fig6cde/2130_A1_yATP_NLIMPRO.csv") #8h

data.2h$yATP.H <- data.2h$VL2.H/data.2h$VL1.H
data.4h$yATP.H <- data.4h$VL2.H/data.4h$VL1.H
data.6h$yATP.H <- data.6h$VL2.H/data.6h$VL1.H
data.8h$yATP.H <- data.8h$VL2.H/data.8h$VL1.H

data.2h <- data.2h %>% dplyr::select(FSC.H,yATP.H)
data.4h <- data.4h %>% dplyr::select(FSC.H,yATP.H)
data.6h <- data.6h %>% dplyr::select(FSC.H,yATP.H)
data.8h <- data.8h %>% dplyr::select(FSC.H,yATP.H)

clustered_data.2h <- perform_clustering_Rmixmod(data.2h,"Gaussian_pk_L_I")
clustered_data.4h <- perform_clustering_Rmixmod(data.4h,"Gaussian_pk_L_I")
clustered_data.6h <- perform_clustering_Rmixmod(data.6h,"Gaussian_pk_L_I")
clustered_data.8h <- perform_clustering_Rmixmod(data.8h,"Gaussian_pk_L_I")

timepoints <- c(rep("2h",nrow(clustered_data.2h)),
                rep("4h",nrow(clustered_data.4h)),
                rep("6h",nrow(clustered_data.6h)),
                rep("8h",nrow(clustered_data.8h)))

data <- bind_rows(clustered_data.2h,clustered_data.4h,clustered_data.6h,clustered_data.8h)

data$timepoints <- timepoints

# Violin plot based on clustering
colour_map = c(inferno(10)[8],inferno(10)[6])

data_plot <- data %>% filter(!timepoints=="2h")


ggplot(data_plot,aes(y=yATP.H, x=timepoints,color=factor(classification), fill=factor(classification))) + 
  geom_boxplot(aes(y=yATP.H, x=timepoints,color=factor(classification)),colour="black",outlier.colour=NA, position=position_dodge(width=0.9))+
  ylab("yATP output (VL2-H/VL1-H)")+
  ylim(0,6)+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

#Stats

p1 = clustered_data.6h %>% filter(classification == "Low") %>% pull(yATP.H)
p2 = clustered_data.6h %>% filter(classification == "High") %>% pull(yATP.H)

t.test(p1, p2, paired = FALSE, alternative = "two.sided")


ggsave("../figures/fig6b_ATP_box_plot_Rmixmod_Gaussian_pk_L_I_points.svg", width = 4, height = 2, bg='transparent')


##
# SUPP
##

data <- clustered_data.4h


col_fct = factor(data$classification, c("Low","High")) # Order on plot
data$col_fct = col_fct

colour_map = c(inferno(10)[6],inferno(10)[8])

ggplot(data, aes(x=FSC.H,y=yATP.H/FSC.H,colour=col_fct)) + 
  geom_point(size=1, alpha=0.8)+
  ylab("yATP output (VL2-H/VL1-H)")+
  xlab("Cell size (FSC-H)")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  theme_bw()+
  xlim(0,270000)+
  ylim(0,0.0003)+ #9
  theme(text = element_text(size=10),
        panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#legend.position = "none"

#ggsave(p)
ggplot(data, aes(x=FSC.H)) + 
  geom_density(size=1, alpha=.4)+
  geom_density(aes(colour=col_fct,fill=col_fct), size=1, alpha=.4)+
  ylab("Density")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  #xlim(0,xlim)+
  #ylim(0,ylim)+
  xlab("Cell size (FSC-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#legend.position = "none"






# Multiple timepoints FBP -------------------------------------------------

data.4h <- read.csv(file = "../source_data/fig6cde/1650_A9_FBPs1_NLIMPRO.csv") %>% trim_below(10) #4h
data.6h <- read.csv(file = "../source_data/fig6cde/1850_C9_FBPs1_NLIMPRO.csv") %>% trim_below(13) #6h
data.8h <- read.csv(file = "../source_data/fig6cde/2050_G9_FBPs1_NLIMPRO.csv") %>% trim_below(13) #8h

data.4h$FBPs1.H <- data.4h$YL2.H/data.4h$BL1.H
data.6h$FBPs1.H <- data.6h$YL2.H/data.6h$BL1.H
data.8h$FBPs1.H <- data.8h$YL2.H/data.8h$BL1.H

data.4h <- data.4h %>% dplyr::select(FSC.H,FBPs1.H) %>% filter(FBPs1.H<20,FBPs1.H>4) 
data.6h <- data.6h %>% dplyr::select(FSC.H,FBPs1.H) %>% filter(FBPs1.H<20,FBPs1.H>4)
data.8h <- data.8h %>% dplyr::select(FSC.H,FBPs1.H) %>% filter(FBPs1.H<20,FBPs1.H>4)

clustered_data.4h <- perform_clustering_Rmixmod(data.4h,"Gaussian_pk_L_I")
clustered_data.6h <- perform_clustering_Rmixmod(data.6h,"Gaussian_pk_L_I")
clustered_data.8h <- perform_clustering_Rmixmod(data.8h,"Gaussian_pk_L_I")

# To save Rmixmod metrics
#ggsave("../figures/figS16_Rmixmod_FBP_8h_pk_L_I_hard.svg", width = 4, height = 4, bg='transparent')

timepoints <- c(rep("4h",nrow(clustered_data.4h)),
                rep("6h",nrow(clustered_data.6h)),
                rep("8h",nrow(clustered_data.8h)))

data <- bind_rows(clustered_data.4h,clustered_data.6h,clustered_data.8h)

data$timepoints <- timepoints

# Violin plot based on clustering
colour_map = c(inferno(10)[8],inferno(10)[6])

data_plot <- data

col_fct = factor(data_plot$classification, c("Low","High")) # Order on plot
data_plot$col_fct = col_fct

ggplot(data_plot,aes(y=FBPs1.H, x=timepoints,color=col_fct, fill=col_fct)) + 
  #geom_point(position=position_jitterdodge(dodge.width=0.9,jitter.width = 0.7), size=1, alpha=0.1) +
  geom_boxplot(aes(y=FBPs1.H, x=timepoints,color=col_fct),colour="black",outlier.colour=NA, position=position_dodge(width=0.9))+
  ylab("FBP sensor output (BL1-H/YL2-H)")+
  ylim(4,17)+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) # legend.position = "none"

#Stats

p1 = clustered_data.8h %>% filter(classification == "Low") %>% pull(FBPs1.H)
p2 = clustered_data.8h %>% filter(classification == "High") %>% pull(FBPs1.H)

t.test(p1, p2, paired = FALSE, alternative = "two.sided")


ggsave("../figures/fig_4c_FBP_box_plot_Rmixmod_Gaussian_pk_L_I_points.svg", width = 4, height = 2, bg='transparent')



##
# SUPP
##

data <- clustered_data.4h

col_fct = factor(data$classification, c("Low","High")) # Order on plot
data$col_fct = col_fct

colour_map = c(inferno(10)[6],inferno(10)[8])

ggplot(data, aes(x=FSC.H,y=FBPs1.H,colour=col_fct)) + 
  geom_point(size=1, alpha=0.8)+
  ylab("FBP sensor output (BL1-H/YL2-H)")+
  xlab("Cell size (FSC-H)")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  theme_bw()+
  theme(text = element_text(size=10),
        panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#legend.position = "none"

#ggsave(p)
ggplot(data, aes(x=FSC.H)) + 
  geom_density(size=1, alpha=.4)+
  geom_density(aes(colour=col_fct,fill=col_fct), size=1, alpha=.4)+
  ylab("Density")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  #xlim(0,xlim)+
  #ylim(0,ylim)+
  xlab("Cell size (FSC-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())#legend.position = "none"

# ATP across nitrogen sources ---------------------------------------------

# Create metadata

# Pull location for each file and convert into a list
file_names <- read.csv('helper_last_runs_yATP.csv') 
file_names <- file_names %>% split(., seq(nrow(.)), drop=T)

# Pull single cell parameters and store in single meta dataframe
metadata <- lapply(file_names,pull_sc_values)
metadata <- bind_rows(metadata)

metadata$ATP.H <- metadata$VL2.H/metadata$VL1.H

# Plotting 

selected_data <- metadata %>% filter(timepoint=="4h")

colour_map = c(inferno(10)[2],inferno(10)[6],inferno(10)[9])

p<- ggplot(selected_data, aes(x=FSC.H, colour = replicate)) + 
  geom_density(aes(fill=NULL), size=1, alpha=.4)+ #after_stat(scaled)
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  #ylim(0, 4.2)+
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

p + facet_wrap(~ condition, ncol=5)


p<- ggplot(selected_data, aes(x=ATP.H, colour = replicate)) + 
  geom_density(aes(fill=NULL), size=1, alpha=.4)+ #after_stat(scaled)
  xlim(0, 4.2)+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  ylab("Density")+
  xlab("ATP sensor output (VL2-H/VL1-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

p + facet_wrap(~ condition, ncol=5)

p <- ggplot(selected_data, aes(x=FSC.H,y=ATP.H,colour=replicate)) + 
  geom_density_2d(bins=10)+
  ylab("ATP sensor output (VL2-H/VL1-H)")+
  xlab("Cell size (FSC.H)")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  theme_bw()+
  ylim(1, 3.5)+
  theme(text = element_text(size=10),
        panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) #legend.position = "none"

p + facet_wrap(vars(condition), ncol=5)

ggsave("../figures/Supp_ATP_scatter_all_conditions.svg", width = 7, height = 5, bg='transparent')



# Zoom on a specific case without density function

zoomed_data <- metadata %>% filter(timepoint=="4h" & condition=="Leu")

ggplot(zoomed_data, aes(x=FSC.H,y=ATP.H,colour=replicate)) + 
  geom_point(alpha=0.2)+
  ylab("ATP sensor output (VL2-H/VL1-H)")+
  xlab("Cell size (FSC.H)")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  theme_bw()+
  ylim(0.5, 5.0)+
  theme(text = element_text(size=10),
        panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#Adding subpopulation classification

selected_data <- metadata %>% filter(timepoint=="4h") %>% 
  filter(ATP.H>0.5 & ATP.H<5.0) %>%
  dplyr::select(FSC.H,ATP.H)
selected_data.clustered <- perform_clustering_Rmixmod(selected_data,"Gaussian_pk_L_I")
selected_data.clustered$condition <- selected_data$condition # Add conditions vector back

# Cluster on whole timepoint and then plot
zoomed_data.clustered <- selected_data.clustered %>% filter(condition=="Trp")

ggplot(zoomed_data.clustered, aes(x=FSC.H,y=ATP.H,colour=as.factor(classification))) + 
  geom_point(alpha=0.2)+
  ylab("ATP sensor output (VL2-H/VL1-H)")+
  xlab("Cell size (FSC.H)")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  theme_bw()+
  ylim(0.5, 5.0)+
  theme(text = element_text(size=10),
        panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Population bar chart 


metadata_individual_points <- metadata %>% filter(ATP.H>1 & ATP.H<5.0) %>%
  filter(condition!="YPD") %>%
  group_by(condition,timepoint,replicate) %>% 
  dplyr::summarize(
    ATP.H = mean(ATP.H),
  )

metadata_grouped <- metadata_individual_points %>%
  group_by(condition,timepoint) %>% 
  dplyr::summarize(
    ATP.mean = mean(ATP.H),
    ATP.sd = sd(ATP.H),
  )

colour_map = c(inferno(10)[9],inferno(10)[2])

# Plot population ATP levels
p <- ggplot(metadata_grouped , aes(x=condition, y=ATP.mean))+
  geom_bar( aes(x= reorder(condition, desc(ATP.mean)), y=ATP.mean), stat="identity", alpha=0.8, width=0.7)+
  geom_point(data=metadata_individual_points, aes(x=condition,y=ATP.H),size=1, alpha=1, colour="grey")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  geom_errorbar(aes(ymin=ATP.mean-ATP.sd, ymax=ATP.mean+ATP.sd), width=.3, color='black')+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        axis.title.x=element_blank(),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

p + facet_grid(~timepoint)


