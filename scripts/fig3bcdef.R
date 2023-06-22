

require(ggplot2)
require(viridis)
require(dplyr)
require(readxl)
require(reshape2)
require(xlsx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Functions

compute_bulk_mean <- function(data) {
  data <- data %>% mutate(
    count_bulk = count_high+count_low+count_none,
    mean_bulk = ((mean_high * count_high) + (mean_low * count_low) + (mean_none * count_none))/count_bulk
  )
} # compute bulk mean for the well (without any population assignment)
join_ratio <- function(pro_data,gln_data){
  data <- pro_data %>% inner_join(gln_data, by=c("TF"="TF")) %>%
    rename("mean_loc_pro"="mean_bulk.x") %>%
    rename("mean_loc_gln"="mean_bulk.y") %>%
    mutate(
      distance_pro_gln = residuals(lm(mean_loc_pro~mean_loc_gln)),
      ratio_pro_gln = mean_loc_pro/mean_loc_gln,
      z_distance_pro_gln = scale(distance_pro_gln),
      z_ratio_pro_gln = scale(ratio_pro_gln),
      pval_z_distance_pro_gln = 2*pnorm(q=abs(z_distance_pro_gln), lower.tail=FALSE),
      pval_z_ratio_pro_gln = 2*pnorm(q=abs(z_ratio_pro_gln), lower.tail=FALSE),
      distance_low_pro_gln = residuals(lm(mean_low.x~mean_low.y)),
      ratio_low_pro_gln = mean_low.x/mean_low.y,
      z_distance_low_pro_gln = scale(distance_low_pro_gln),
      z_ratio_low_pro_gln = scale(ratio_low_pro_gln),
      pval_z_distance_low_pro_gln = 2*pnorm(q=abs(z_distance_low_pro_gln), lower.tail=FALSE),
      pval_z_ratio_low_pro_gln = 2*pnorm(q=abs(z_ratio_low_pro_gln), lower.tail=FALSE),
      differential_pop_pro_gln = residuals(lm(distance_best_fit.x~distance_best_fit.y)), #Regression of the LOW/HIGH PRO vs LOW/HIGH GLN
      ratio_pop_pro_gln = distance_best_fit.x/distance_best_fit.y,
      z_distance_pop_pro_gln = scale(differential_pop_pro_gln),
      z_ratio_pop_pro_gln = scale(ratio_pop_pro_gln),
      pval_z_distance_pop_pro_gln = 2*pnorm(q=abs(z_distance_pop_pro_gln), lower.tail=FALSE),
      pval_z_ratio_pop_pro_gln = 2*pnorm(q=abs(z_ratio_pop_pro_gln), lower.tail=FALSE)
    )
  
} # join dataframes and compute distances
plot_contour_2c <- function(dataframe,xval,yval,colval,xtitle,ytitle,alpha,xlim,ylim,levels){
  
  col_fct = factor(dataframe[[colval]], levels=levels) # Order on plot
  dataframe$col_fct = col_fct
  
  colour_map = c(inferno(10)[9],inferno(10)[2])
  
  p <- ggplot(dataframe, aes(x=.data[[xval]],y=.data[[yval]],colour=col_fct)) + 
    geom_density_2d()+
    ylab(ytitle)+
    xlab(xtitle)+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    theme_bw()+
    xlim(xlim[1],xlim[2])+
    ylim(ylim[1],ylim[2])+
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
plot_hist_2c <- function(dataframe,xval,colval,xtitle,xlim,ylim,levels){
  
  col_fct = factor(dataframe[[colval]], levels=levels) # Order on plot
  dataframe$col_fct = col_fct
  
  colour_map = c(inferno(10)[2],inferno(10)[9])
  
  p <- ggplot(dataframe, aes(x=.data[[xval]],colour=col_fct,fill=col_fct)) + 
    geom_density(size=1, alpha=.4)+
    ylab("Density")+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    xlim(xlim[1],xlim[2])+
    ylim(ylim[1],ylim[2])+
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

# Import TF localization data

PRO.30 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 30 mins Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()
PRO.90 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 1_30 Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()
PRO.150 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 2_30 Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()
PRO.210 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 3_30 Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()

GLN.30 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 30 mins Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()
GLN.90 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 1_30 Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()
GLN.150 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 2_30 Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()
GLN.210 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 3_30 Candidates v5.xlsx",sheet=1) %>% compute_bulk_mean()

master.30 <- join_ratio(PRO.30,GLN.30)
master.90 <- join_ratio(PRO.90,GLN.90)
master.150 <- join_ratio(PRO.150,GLN.150)
master.210 <- join_ratio(PRO.210,GLN.210)

timepoints <- c(rep(30, nrow(master.30)),
                rep(90, nrow(master.90)),
                rep(150, nrow(master.150)),
                rep(210, nrow(master.210)))

data.master <- bind_rows(master.30,
                         master.90,
                         master.150,
                         master.210)

data.master$timepoints = timepoints

# Scatter RPL28 vs pixel size ---------------------------------------------

# Reproduce bimodality seen in the flow

all.PRO <- read.csv("../source_data/fig3bcdef/May 3rd - PRO 30 mins all cells.csv")
all.GLN <- read.csv("../source_data/fig3bcdef/May 3rd - GLN 30 mins all cells.csv")

all.PRO <- read.csv("../source_data/fig3bcdef/May 3rd - PRO 1_30 all cells.csv")
all.GLN <- read.csv("../source_data/fig3bcdef/May 3rd - GLN 1_30 all cells.csv")

all.PRO <- read.csv("../source_data/fig3bcdef/May 3rd - PRO 2_30 all cells.csv")
all.GLN <- read.csv("../source_data/fig3bcdef/May 3rd - GLN 2_30 all cells.csv")

all.PRO <- read.csv("../source_data/fig3bcdef/May 3rd - PRO 3_30 all cells.csv")
all.GLN <- read.csv("../source_data/fig3bcdef/May 3rd - GLN 3_30 all cells.csv")

condition <- c(rep("NLIM-PRO", nrow(all.PRO)),
               rep("NLIM-GLN", nrow(all.GLN)))

all <- bind_rows(all.PRO,
                 all.GLN)

all$condition = condition
# Conversion factor obtained from nd2 file specifying area per pixel
all$size_um = all$size * 0.0336 


plot_contour_2c(all,"size_um","meanRedValue","condition","Cell size (uM)","pRPL28 fluorescence (mean intensity/cell)",.02,c(3,22),c(100,340),c("NLIM-GLN","NLIM-PRO"))
ggsave("../figures/fig3b_scatter_size_RPL28_150min_uM.svg", width = 5, height = 5, bg='transparent')

plot_hist_2c(all,"size_um","condition","Cell size (uM)",c(3,22), c(0,0.20),c("NLIM-PRO","NLIM-GLN"))
ggsave("../figures/fig3b_hist_size_150min_uM.svg", width = 5, height = 5, bg='transparent')

plot_hist_2c(all,"meanRedValue","condition","pRPL28 fluorescence (mean intensity/cell)",c(100,340), c(0,0.03),c("NLIM-PRO","NLIM-GLN"))
ggsave("../figures/fig3b_hist_RPL28_150min_uM.svg", width = 5, height = 5, bg='transparent')


# TF PRO vs GLN and LOW PRO vs HIGH PRO -----------------------------------------------------------

# scatter plot PRO vs GLN for a given timepoint
colour_map = c(inferno(10)[4],inferno(10)[9],inferno(10)[2]) #"grey",
data.master.plot <- data.master %>% filter(timepoints==30)

# Cbf1m Hmo1 and Cyc8 not in frame
ggplot(data.master.plot,aes(x=mean_loc_gln, y=mean_loc_pro, label=TF)) +
  geom_smooth(method='lm', formula= y~x, fill=NA, colour="grey")+
  geom_point(size=1.5, alpha=0.6, colour="black")+
  geom_abline(linetype=2)+
  scale_colour_manual(values=colour_map)+
  xlab("30 min NLIM-GLN - TF localization (a.u.)")+
  ylab("30 min NLIM-PRO - TF localization (a.u.)")+
  xlim(1.03,1.21)+
  ylim(1.03,1.21)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig3c_scatter_30min_PRO_GLN_zoom.svg", width = 3, height = 3, bg='transparent')


# scatter plot LOW vs HIGH for a given timepoint
ggplot(master.30,aes(x=mean_high.x, y=mean_low.x, label=TF)) +
  geom_smooth(method='lm', formula= y~x, fill=NA, colour="grey")+
  geom_point(size=1.5, alpha=0.6)+
  geom_abline(linetype=2)+
  xlim(1.02,1.13)+
  ylim(1.02,1.13)+
  xlab("High - TF localization (a.u.)")+
  ylab("Low - TF localization (a.u.)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig3d_scatter_30min_LOW_HIGH_zoom.svg", width = 3.11, height = 3, bg='transparent')





###### Display TOP targets for HIGH vs LOW over time and PRO LOW vs GLN LOW ######


#### Proline vs Glutamine ####

# 1. Calculate TF rank for each category and timepoint
TF.30 <- master.30[order(master.30$distance_pro_gln,decreasing=TRUE),] %>% pull(TF)
TF.90 <- master.90[order(master.90$distance_pro_gln,decreasing=TRUE),] %>% pull(TF)
TF.150 <- master.150[order(master.150$distance_pro_gln,decreasing=TRUE),] %>% pull(TF)
TF.210 <- master.210[order(master.210$distance_pro_gln,decreasing=TRUE),] %>% pull(TF)

rank.30 <- data.frame(TF=TF.30,rank_30=seq(1,length(TF.30),1))
rank.90 <- data.frame(TF=TF.90,rank_90=seq(1,length(TF.90),1))
rank.150 <- data.frame(TF=TF.150,rank_150=seq(1,length(TF.150),1))
rank.210 <- data.frame(TF=TF.210,rank_210=seq(1,length(TF.210),1))

rank <- rank.30 %>% inner_join(rank.90) %>% inner_join(rank.150) %>% inner_join(rank.210) %>% 
  rowwise() %>%
  mutate(
    max_rank = max(rank_30,rank_90,rank_150,rank_210),
    min_rank = min(rank_30,rank_90,rank_150,rank_210)
  )

write.xlsx(rank,"../output_tables/TF_rankings.xlsx", sheetName = "NLIM-PRO_GLN")

# 3. Add new column TOP target rank label accordingly
top5 <- rank %>% arrange(max_rank) %>% head(5) %>% pull(TF)
bottom5 <- rank %>% arrange(desc(min_rank)) %>% head(5) %>% pull(TF)

data.master.plot <- data.master %>% mutate(
  category = if_else(TF %in% top5, "top", if_else(TF %in% bottom5, "bottom", "none"))
)

# 4. Plot

colour_map = c(inferno(10)[6],inferno(10)[8],inferno(10)[2])

## Tracking TF distance PRO/GLN from regression over time (all)
ggplot(data.master.plot,aes(x=timepoints, y=distance_pro_gln, group = TF, label=TF)) +
  geom_line(data = data.master.plot[data.master.plot$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = data.master.plot[data.master.plot$category == "top",], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  geom_line(data = data.master.plot[data.master.plot$category == "bottom",], aes(color = inferno(10)[6]), alpha=0.8, size=0.5) +
  xlab("Time (min)")+
  ylab("Distance Pro to Gln")+
  geom_label( 
    data=data.master.plot %>% filter(category=="top"),
    aes(label=TF,colour=inferno(10)[2]),
    size=5
  )+
  geom_label( 
    data=data.master.plot %>% filter(category=="bottom"),
    aes(label=TF,colour=inferno(10)[6]),
    size=5
  )+
  scale_colour_manual(values=colour_map)+
  #ylim(-0.02,0.02)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig3e_PRO_GLN_over_time.svg", width = 6, height = 3, bg='transparent')


#### HIGH vs LOW case (Proline) ####

# Distance_best_fit was previously measured as the distance between high and the fit
master.30$distance_low_high = -master.30$distance_best_fit.x
master.90$distance_low_high = -master.90$distance_best_fit.x
master.150$distance_low_high = -master.150$distance_best_fit.x
master.210$distance_low_high = -master.210$distance_best_fit.x

# 1. Calculate TF rank for each category and timepoint
TF.30 <- master.30[order(master.30$distance_low_high,decreasing=TRUE),] %>% pull(TF)
TF.90 <- master.90[order(master.90$distance_low_high,decreasing=TRUE),] %>% pull(TF)
TF.150 <- master.150[order(master.150$distance_low_high,decreasing=TRUE),] %>% pull(TF)
TF.210 <- master.210[order(master.210$distance_low_high,decreasing=TRUE),] %>% pull(TF)

rank.30 <- data.frame(TF=TF.30,rank_30=seq(1,length(TF.30),1))
rank.90 <- data.frame(TF=TF.90,rank_90=seq(1,length(TF.90),1))
rank.150 <- data.frame(TF=TF.150,rank_150=seq(1,length(TF.150),1))
rank.210 <- data.frame(TF=TF.210,rank_210=seq(1,length(TF.210),1))

rank <- rank.30 %>% inner_join(rank.90) %>% inner_join(rank.150) %>% inner_join(rank.210) %>% 
  rowwise() %>%
  mutate(
    max_rank = max(rank_30,rank_90,rank_150,rank_210),
    min_rank = min(rank_30,rank_90,rank_150,rank_210)
  )

write.xlsx(rank,"../output_tables/TF_rankings.xlsx", sheetName = "Low_High_NLIM-PRO", append = TRUE)

# 3. Add new column TOP target rank label accordingly
top5 <- rank %>% arrange(max_rank) %>% head(5) %>% pull(TF)
bottom5 <- rank %>% arrange(desc(min_rank)) %>% head(5) %>% pull(TF)

data.master.plot <- data.master %>% mutate(
  category = if_else(TF %in% top5, "top", if_else(TF %in% bottom5, "bottom", "none"))
)

# 4. Plot

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2])

## Tracking TF distance LOW/HIGH from regression over time (all)
ggplot(data.master.plot,aes(x=timepoints, y=-distance_best_fit.x, group = TF, label=TF)) +
  geom_line(data = data.master.plot[data.master.plot$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = data.master.plot[data.master.plot$category == "top",], aes(color = inferno(10)[6]), alpha=0.8, size=0.5) +
  geom_line(data = data.master.plot[data.master.plot$category == "bottom",], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  xlab("Time (min)")+
  ylab("Distance Low to High")+
  geom_label( 
    data=data.master.plot %>% filter(category=="top"),
    aes(label=TF,colour=inferno(10)[6]),
    size=5
  )+
  geom_label( 
    data=data.master.plot %>% filter(category=="bottom"),
    aes(label=TF,colour=inferno(10)[2]),
    size=5
  )+
  scale_colour_manual(values=colour_map)+
  ylim(-0.051,0.051)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig3f_LOW_HIGH_over_time.svg", width = 6, height = 3, bg='transparent')


