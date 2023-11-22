# KS 20.10.23

require(ggplot2)
require(viridis)
require(tidyr)
require(dplyr)
require(readxl)
require(reshape2)
require(xlsx)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

source("util.R")

FOV_TF_mapping <- read.csv("../source_data/fig5bcdef/FOV_TF_pair.csv")

# TF by condition ---------------------------------------------------------

##
# Compute additional column with p-values between conditions given SE
##

PRO = read.csv("../source_data/fig5bcdef/May 3rd - PRO 30 mins all cells.csv") # Change here
GLN = read.csv("../source_data/fig5bcdef/May 3rd - GLN 30 mins all cells.csv")

#nrow(read.csv("../source_data/fig5bcdef/May 3rd - PRO 30 mins all cells.csv")) + nrow(read.csv("../source_data/fig5bcdef/May 3rd - PRO 1_30 all cells.csv")) + nrow(read.csv("../source_data/fig5bcdef/May 3rd - PRO 2_30 all cells.csv")) + nrow(read.csv("../source_data/fig5bcdef/May 3rd - PRO 3_30 all cells.csv"))
#nrow(read.csv("../source_data/fig5bcdef/May 3rd - GLN 30 mins all cells.csv")) + nrow(read.csv("../source_data/fig5bcdef/May 3rd - GLN 1_30 all cells.csv")) + nrow(read.csv("../source_data/fig5bcdef/May 3rd - GLN 2_30 all cells.csv")) + nrow(read.csv("../source_data/fig5bcdef/May 3rd - GLN 3_30 all cells.csv"))


conditions <- c(rep('PRO',nrow(PRO)),
                rep('GLN',nrow(GLN)))

data <- bind_rows(PRO,GLN) 
data$conditions <- conditions
data <- data %>% inner_join(FOV_TF_mapping)

# Manual check for given TF
sample1 <- data %>% filter(conditions=="PRO" & TF=="Wtm1") %>% pull(nuc_score)
sample2 <- data %>% filter(conditions=="GLN" & TF=="Wtm1") %>% pull(nuc_score)

population1_mean <- data %>% filter(conditions=="PRO") %>% pull(nuc_score) %>% mean(na.rm=TRUE)
population2_mean <- data %>% filter(conditions=="GLN") %>% pull(nuc_score) %>% mean(na.rm=TRUE)

# Given the the biais in TF localization (deviation from isoline, the new mean needs to be calculated for this given timepoint)
t.test(sample1,sample2,paired=FALSE, mu=population1_mean-population2_mean, alternative = "two.sided")


# Join raw data length wise. Compute mean per condition per TF, pivot and perform t-test on all individual cells per group
# Re center mean to account for condition bias when performing t-test
df.pvalues <- data %>% 
  group_by(TF) %>% dplyr::summarise(
    TF_score_mean.PRO = mean(nuc_score[conditions == "PRO"],na.rm=TRUE),
    TF_score_mean.GLN = mean(nuc_score[conditions == "GLN"],na.rm=TRUE),
    pvalue = ifelse(sum(conditions == "PRO") > 1 & sum(conditions == "GLN") > 1, t.test(nuc_score[conditions == "PRO"], nuc_score[conditions == "GLN"],paired=FALSE, mu=population1_mean-population2_mean, alternative = "two.sided")$p.value, NA),
    significance = if_else(pvalue<=0.05,"True","False"),
  ) %>% na.omit()
           
#colour_map = c(inferno(10)[2],inferno(10)[9])
colour_map = c(viridis_pal()(20)[1],viridis_pal()(20)[13])

ggplot(df.pvalues,aes(x=TF_score_mean.GLN, y=TF_score_mean.PRO, label=TF, colour=significance)) +
  #geom_smooth(method='lm', formula= y~x, fill=NA, colour="grey")+
  geom_point(size=1.5, alpha=0.6)+
  geom_abline(linetype=2)+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  geom_text()+
  xlim(250,350)+ #(200,500)
  ylim(250,350)+
  xlab("NLIM - GLN (a.u.)")+
  ylab("NLIM - PRO (a.u.)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") 

#ggsave("../figures/fig5d_scatter_30min_PRO_GLN.svg", width = 3, height = 3, bg='transparent')


## ## ## ## ## ## ## ## ## ## ## ## 
##### Rank most consistant TFs 
## ## ## ## ## ## ## ## ## ## ## ## 

all <- list(
  PRO.30 = read.csv("../source_data/fig5bcdef/May 3rd - PRO 30 mins all cells.csv"),
  GLN.30 = read.csv("../source_data/fig5bcdef/May 3rd - GLN 30 mins all cells.csv"),
  PRO.90 = read.csv("../source_data/fig5bcdef/May 3rd - PRO 1_30 all cells.csv"),
  GLN.90 = read.csv("../source_data/fig5bcdef/May 3rd - GLN 1_30 all cells.csv"),
  PRO.150 = read.csv("../source_data/fig5bcdef/May 3rd - PRO 2_30 all cells.csv"),
  GLN.150 = read.csv("../source_data/fig5bcdef/May 3rd - GLN 2_30 all cells.csv"),
  PRO.210 = read.csv("../source_data/fig5bcdef/May 3rd - PRO 3_30 all cells.csv"),
  GLN.210 = read.csv("../source_data/fig5bcdef/May 3rd - GLN 3_30 all cells.csv"))

populations.TF_scores = lapply(all,condition_TF_score) #compute population-wide TF score (regardless of subpopulation assignment)

#Merge PRO and GLN
pop.TF_scores.30 <- populations.TF_scores[["PRO.30"]] %>% inner_join(populations.TF_scores[["GLN.30"]], by= c("field.of.view"="field.of.view"))
pop.TF_scores.90 <- populations.TF_scores[["PRO.90"]] %>% inner_join(populations.TF_scores[["GLN.90"]], by= c("field.of.view"="field.of.view"))
pop.TF_scores.150 <- populations.TF_scores[["PRO.150"]] %>% inner_join(populations.TF_scores[["GLN.150"]], by= c("field.of.view"="field.of.view"))
pop.TF_scores.210 <- populations.TF_scores[["PRO.210"]] %>% inner_join(populations.TF_scores[["GLN.210"]], by= c("field.of.view"="field.of.view"))

pop.TF_scores.30$pro_gln <- pop.TF_scores.30$TF_score_mean.x / pop.TF_scores.30$TF_score_mean.y
pop.TF_scores.90$pro_gln <- pop.TF_scores.90$TF_score_mean.x / pop.TF_scores.90$TF_score_mean.y
pop.TF_scores.150$pro_gln <- pop.TF_scores.150$TF_score_mean.x / pop.TF_scores.150$TF_score_mean.y
pop.TF_scores.210$pro_gln <- pop.TF_scores.210$TF_score_mean.x / pop.TF_scores.210$TF_score_mean.y

# 1. Calculate TF rank for each category and timepoint
pro_gln.30 <- pop.TF_scores.30[order(pop.TF_scores.30$pro_gln,decreasing=TRUE),] %>% na.omit() %>% pull(TF.x)
pro_gln.90 <- pop.TF_scores.90[order(pop.TF_scores.90$pro_gln,decreasing=TRUE),] %>% na.omit() %>% pull(TF.x)
pro_gln.150 <- pop.TF_scores.150[order(pop.TF_scores.150$pro_gln,decreasing=TRUE),] %>% na.omit() %>% pull(TF.x)
pro_gln.210 <- pop.TF_scores.210[order(pop.TF_scores.210$pro_gln,decreasing=TRUE),] %>% na.omit() %>% pull(TF.x)

rank.30 <- data.frame(TF=pro_gln.30,rank_30=seq(1,length(pro_gln.30),1))
rank.90 <- data.frame(TF=pro_gln.90,rank_90=seq(1,length(pro_gln.90),1))
rank.150 <- data.frame(TF=pro_gln.150,rank_150=seq(1,length(pro_gln.150),1))
rank.210 <- data.frame(TF=pro_gln.210,rank_210=seq(1,length(pro_gln.210),1))

rank.pro_gln <- rank.30 %>% inner_join(rank.90) %>% inner_join(rank.150) %>% inner_join(rank.210) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    highest_rank = min(rank_30,rank_90,rank_150,rank_210),
    lowest_rank = max(rank_30,rank_90,rank_150,rank_210)
  )

#write.xlsx(rank.pro_gln,"../output_tables/TF_rankings.xlsx", sheetName = "NLIM-PRO_GLN")

# 3. Add new column TOP target rank label accordingly
top5 <- rank.pro_gln %>% arrange(lowest_rank) %>% head(5) %>% pull(TF)
bottom5 <- rank.pro_gln %>% arrange(desc(highest_rank)) %>% head(5) %>% pull(TF)

timepoints <- c(rep(30,nrow(pop.TF_scores.30)),
          rep(90,nrow(pop.TF_scores.90)),
          rep(150,nrow(pop.TF_scores.150)),
          rep(210,nrow(pop.TF_scores.210)))

pop.TF_scores.master <- bind_rows(pop.TF_scores.30,pop.TF_scores.90,pop.TF_scores.150,pop.TF_scores.210)
pop.TF_scores.master$timepoints <- timepoints

# Add category to highlight top 5 most consistant TF
pop.TF_scores.master.plot <- pop.TF_scores.master %>% mutate(
  category = if_else(TF.x %in% top5, "top", if_else(TF.x %in% bottom5, "bottom", "none"))
)

# 4. Compute relative score across timepoints 
# Due to intensity variation across timepoints we compute a relative TF score by dividing 
# each score by the mean of the "none" score. This keeps the trend of the data.

relative_pro_gln <- pop.TF_scores.master.plot %>%
  group_by(timepoints) %>%
  mutate(pro_gln_normalized = pro_gln / mean(pro_gln[category == "none"], na.rm=TRUE)) %>%
  ungroup()

# 5. Plot

colour_map = c(inferno(10)[6],inferno(10)[8],inferno(10)[2])

## Tracking TF distance PRO/GLN from regression over time (all)
ggplot(relative_pro_gln,aes(x=timepoints, y=pro_gln_normalized, group = TF.x, label=TF.x)) +
  geom_line(data = relative_pro_gln[relative_pro_gln$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = relative_pro_gln[relative_pro_gln$category == "top",], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  geom_line(data = relative_pro_gln[relative_pro_gln$category == "bottom",], aes(color = inferno(10)[6]), alpha=0.8, size=0.5) +
  #ylim(0.8,1.5)+
  xlab("Time (min)")+
  ylab("Ratio TF nuclear intensity Pro vs Gln")+
  geom_label( 
    data=relative_pro_gln %>% filter(category=="top"),
    aes(label=TF.x,colour=inferno(10)[2]),
    size=5
  )+
  geom_label( 
    data=relative_pro_gln%>% filter(category=="bottom"),
    aes(label=TF.x,colour=inferno(10)[6]),
    size=5
  )+
  scale_colour_manual(values=colour_map)+
  #ylim(-0.02,0.02)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#ggsave("../figures/fig5e_PRO_GLN_over_time.svg", width = 6, height = 3, bg='transparent')

##
# SUPP: Show ranking over time for selected target
##

targets <- c("Put3")
#targets <- c("Gln3","Gat1","Dal80","Gzf3","Dal81","Aro80","Rtg1","Rtg3")

colour_map = c(inferno(10)[8],inferno(10)[2])

ggplot(relative_pro_gln,aes(x=timepoints, y=pro_gln_normalized, group = TF.x, label=TF.x)) +
  geom_line(data = relative_pro_gln[relative_pro_gln$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = relative_pro_gln[relative_pro_gln$TF.x %in% targets,], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  ylim(0.8,1.6)+ 
  xlab("Time (min)")+
  ylab("Ratio TF nuclear intensity Pro vs Gln")+
  geom_label( 
    data=relative_pro_gln[relative_pro_gln$TF.x %in% targets,],
    aes(label=TF.x,colour=inferno(10)[2]),
    size=5
  )+
  scale_colour_manual(values=colour_map)+
  #ylim(-0.02,0.02)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig5e_SUPP_PRO_GLN_Put3_over_time.svg", width = 5, height = 3, bg='transparent')
#ggsave("../figures/fig5e_PRO_GLN_over_time.svg", width = 6, height = 3, bg='transparent')


# TF by subpopulation -----------------------------------------------------

##
# Compute additional column with p-values between conditions given SE
##

PRO = read.csv("../source_data/fig5bcdef/May 3rd - PRO 1_30 all cells.csv") # Change here

data <- PRO %>% inner_join(FOV_TF_mapping)

# Manual check for given TF
sample1 <- data %>% dplyr::filter(population=="low" & TF=="Wtm1") %>% pull(nuc_score)
sample2 <- data %>% dplyr::filter(population=="high" & TF=="Wtm1") %>% pull(nuc_score)

population1_mean <- data %>% dplyr::filter(population=="low") %>% pull(nuc_score) %>% mean(na.rm=TRUE)
population2_mean <- data %>% dplyr::filter(population=="high") %>% pull(nuc_score) %>% mean(na.rm=TRUE)

# Given the the biais in TF localization (deviation from isoline, the new mean needs to be calculated for this given timepoint)
t.test(sample1,sample2,paired=FALSE, mu=population1_mean-population2_mean, alternative = "two.sided")


# Join raw data length wise. Compute mean per condition per TF, pivot and perform t-test on all individual cells per group
# Re center mean to account for condition bias when performing t-test
df.pvalues <- data %>% 
  group_by(TF) %>% summarise(
    TF_score_mean.low = mean(nuc_score[population == "low"],na.rm=TRUE),
    TF_score_mean.high = mean(nuc_score[population == "high"],na.rm=TRUE),
    pvalue = ifelse(sum(population == "low") > 1 & sum(population == "high") > 1, t.test(nuc_score[population == "low"], nuc_score[population == "high"],paired=FALSE, mu=population1_mean-population2_mean, alternative = "two.sided")$p.value, NA),
    significance = if_else(pvalue<=0.05,"True","False"),
  ) %>% na.omit()

colour_map = c(viridis_pal()(20)[1],viridis_pal()(20)[13])

# Individual timepoint
ggplot(df.pvalues,aes(x=TF_score_mean.high, y=TF_score_mean.low, label=TF, colour=significance)) +
  #geom_smooth(method='lm', formula= y~x, fill=NA, colour="grey")+
  geom_point(size=1.5, alpha=0.6)+
  geom_abline(linetype=2)+
  geom_text()+
  xlim(250,350)+
  ylim(250,350)+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  xlab("High - TF intensity (a.u.)")+
  ylab("Low - TF intensity (a.u.)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#ggsave("../figures/fig5e_scatter_210min_High_Low.svg", width = 3, height = 3, bg='transparent')


## ## ## ## ## ## ## ## ## ## ## ## 
##### Rank most consistant TFs 
## ## ## ## ## ## ## ## ## ## ## ## 

# Compute the mean, sd, se TF intensities with number of cells

subpop.TF_scores = lapply(all,subpopulation_TF_score) 

PRO.30 <- subpop.TF_scores[["PRO.30"]][[2]]
PRO.90 <- subpop.TF_scores[["PRO.90"]][[2]]
PRO.150 <- subpop.TF_scores[["PRO.150"]][[2]]
PRO.210 <- subpop.TF_scores[["PRO.210"]][[2]]


# 1. Calculate TF rank for each category and timepoint
low_high.30 <- PRO.30[order(PRO.30$low_high,decreasing=TRUE),] %>% na.omit() %>% pull(TF)
low_high.90 <- PRO.90[order(PRO.90$low_high,decreasing=TRUE),] %>% na.omit() %>% pull(TF)
low_high.150 <- PRO.150[order(PRO.150$low_high,decreasing=TRUE),] %>% na.omit() %>% pull(TF)
low_high.210 <- PRO.210[order(PRO.210$low_high,decreasing=TRUE),] %>% na.omit() %>% pull(TF)

rank.30 <- data.frame(TF=low_high.30,rank_30=seq(1,length(low_high.30),1))
rank.90 <- data.frame(TF=low_high.90,rank_90=seq(1,length(low_high.90),1))
rank.150 <- data.frame(TF=low_high.150,rank_150=seq(1,length(low_high.150),1))
rank.210 <- data.frame(TF=low_high.210,rank_210=seq(1,length(low_high.210),1))

rank.low_high <- rank.30 %>% inner_join(rank.90) %>% inner_join(rank.150) %>% inner_join(rank.210) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    highest_rank = min(rank_30,rank_90,rank_150,rank_210),
    lowest_rank = max(rank_30,rank_90,rank_150,rank_210)
  )

#write.xlsx(rank.low_high,"../output_tables/TF_rankings.xlsx", sheetName = "PRO_LOW_HIGH",append = TRUE)

# 3. Add new column TOP target rank label accordingly
top5 <- rank.low_high %>% arrange(lowest_rank) %>% head(5) %>% pull(TF)
bottom5 <- rank.low_high %>% arrange(desc(highest_rank)) %>% head(5) %>% pull(TF)

timepoints <- c(rep(30,nrow(PRO.30)),
                rep(90,nrow(PRO.90)),
                rep(150,nrow(PRO.150)),
                rep(210,nrow(PRO.210)))

PRO.master <- bind_rows(PRO.30,PRO.90,PRO.150,PRO.210)
PRO.master$timepoints <- timepoints

# Add category to highlight top 5 most consistant TF
PRO.master.plot <- PRO.master %>% mutate(
  category = if_else(TF %in% top5, "top", if_else(TF %in% bottom5, "bottom", "none"))
)


# 4. Compute relative score across timepoints 
# Due to intensity variation across timepoints we compute a relative TF score by dividing 
# each score by the mean of the "none" score. This keeps the trend of the data.

relative_low_high <- PRO.master.plot %>%
  group_by(timepoints) %>%
  mutate(low_high_normalized = low_high / mean(low_high[category == "none"],na.rm=TRUE)) %>%
  ungroup()

# 5. Plot

colour_map = c(inferno(10)[6],inferno(10)[8],inferno(10)[2])

## Tracking TF intensity Low/High 
ggplot(relative_low_high,aes(x=timepoints, y=low_high_normalized, group = TF, label=TF)) +
  geom_line(data = relative_low_high[relative_low_high$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = relative_low_high[relative_low_high$category == "top",], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  geom_line(data = relative_low_high[relative_low_high$category == "bottom",], aes(color = inferno(10)[6]), alpha=0.8, size=0.5) +
  #ylim(0.8,1.2)+
  xlab("Time (min)")+
  ylab("Ratio TF nuclear intensity Low vs High")+
  geom_label( 
    data=relative_low_high %>% filter(category=="top"),
    aes(label=TF,colour=inferno(10)[2]),
    size=5
  )+
  geom_label( 
    data=relative_low_high %>% filter(category=="bottom"),
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


#ggsave("../figures/fig5f_Low_High_over_time.svg", width = 6, height = 3, bg='transparent')

##
# Looking at specific targets
##

targets <- c("Bcy1","Sok2","Mig1")
#targets <- c("Gln3","Gat1","Dal80","Gzf3","Dal81","Aro80","Rtg1","Rtg3","Dig2","Tec1","Ste12")

colour_map = c(inferno(10)[8],inferno(10)[2])

ggplot(relative_low_high,aes(x=timepoints, y=low_high_normalized, group = TF, label=TF)) +
  geom_line(data = relative_low_high[relative_low_high$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = relative_low_high[relative_low_high$TF %in% targets,], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  ylim(0.95,1.05)+ 
  xlab("Time (min)")+
  ylab("Ratio TF nuclear intensity Low vs High")+
  geom_label( 
    data=relative_low_high[relative_low_high$TF %in% targets,],
    aes(label=TF,colour=inferno(10)[2]),
    size=5
  )+
  scale_colour_manual(values=colour_map)+
  #ylim(-0.02,0.02)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#ggsave("../figures/fig5e_PRO_GLN_over_time.svg", width = 6, height = 3, bg='transparent')




# TF by subpopulation LOW vs LOW ------------------------------------------

##
# Compute additional column with p-values between conditions given SE
##

PRO = read.csv("../source_data/fig5bcdef/May 3rd - PRO 3_30 all cells.csv") # Change here
GLN = read.csv("../source_data/fig5bcdef/May 3rd - GLN 3_30 all cells.csv")

conditions <- c(rep('PRO',nrow(PRO)),
                rep('GLN',nrow(GLN)))

data <- bind_rows(PRO,GLN) 
data$conditions <- conditions
data <- data %>% inner_join(FOV_TF_mapping)

# Manual check for given TF
sample1 <- data %>% dplyr::filter(conditions=="PRO" & population=="low" & TF=="Wtm1") %>% pull(nuc_score)
sample2 <- data %>% dplyr::filter(conditions=="GLN" & population=="low" & TF=="Wtm1") %>% pull(nuc_score)

population1_mean <- data %>% dplyr::filter(conditions=="PRO" & population=="low") %>% pull(nuc_score) %>% mean(na.rm=TRUE)
population2_mean <- data %>% dplyr::filter(conditions=="GLN" & population=="low") %>% pull(nuc_score) %>% mean(na.rm=TRUE)

# Given the the biais in TF localization (deviation from isoline, the new mean needs to be calculated for this given timepoint)
t.test(sample1,sample2,paired=FALSE, mu=population1_mean-population2_mean, alternative = "two.sided")


# Join raw data length wise. Compute mean per condition per TF, pivot and perform t-test on all individual cells per group
# Re center mean to account for condition bias when performing t-test
df.pvalues <- data %>% 
  group_by(TF) %>% summarise(
    TF_score_mean.low.pro = mean(nuc_score[conditions=="PRO" & population == "low"],na.rm=TRUE),
    TF_score_mean.low.gln = mean(nuc_score[conditions=="GLN" & population == "low"],na.rm=TRUE),
    pvalue = ifelse(sum(conditions=="PRO" & population == "low") > 1 & sum(conditions=="GLN" & population == "low") > 1, t.test(nuc_score[conditions=="PRO" & population == "low"], nuc_score[conditions=="GLN" & population == "low"],paired=FALSE, mu=population1_mean-population2_mean, alternative = "two.sided")$p.value, NA),
    significance = if_else(pvalue<=0.05,"True","False"),
  ) %>% na.omit()

colour_map = c(viridis_pal()(20)[1],viridis_pal()(20)[13])

# Individual timepoint
ggplot(df.pvalues,aes(x=TF_score_mean.low.gln, y=TF_score_mean.low.pro, label=TF, colour=significance)) +
  #geom_smooth(method='lm', formula= y~x, fill=NA, colour="grey")+
  geom_point(size=1.5, alpha=0.6)+
  geom_abline(linetype=2)+
  geom_text()+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  xlim(200,300)+
  ylim(200,300)+
  xlab("Low - Gln")+
  ylab("Low - Pro")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

#ggsave("../figures/fig5e_SUPP_scatter_210min_Low_Low_zoom.svg", width = 3, height = 3, bg='transparent')

# 1. Calculate TF rank for each category and timepoint

subpop.TF_scores = lapply(all,subpopulation_TF_score) 

subpop.merged.TF_scores.30 <- subpop.TF_scores[["PRO.30"]][[2]] %>% inner_join(subpop.TF_scores[["GLN.30"]][[2]],by=c("TF"="TF"))
subpop.merged.TF_scores.90 <- subpop.TF_scores[["PRO.90"]][[2]] %>% inner_join(subpop.TF_scores[["GLN.90"]][[2]],by=c("TF"="TF"))
subpop.merged.TF_scores.150 <- subpop.TF_scores[["PRO.150"]][[2]] %>% inner_join(subpop.TF_scores[["GLN.150"]][[2]],by=c("TF"="TF"))
subpop.merged.TF_scores.210 <- subpop.TF_scores[["PRO.210"]][[2]] %>% inner_join(subpop.TF_scores[["GLN.210"]][[2]],by=c("TF"="TF"))

subpop.merged.TF_scores.30$low_low <- subpop.merged.TF_scores.30$low.x/subpop.merged.TF_scores.30$low.y #low pro / low gln
subpop.merged.TF_scores.90$low_low <- subpop.merged.TF_scores.90$low.x/subpop.merged.TF_scores.90$low.y #low pro / low gln
subpop.merged.TF_scores.150$low_low <- subpop.merged.TF_scores.150$low.x/subpop.merged.TF_scores.150$low.y #low pro / low gln
subpop.merged.TF_scores.210$low_low <- subpop.merged.TF_scores.210$low.x/subpop.merged.TF_scores.210$low.y #low pro / low gln

low_low.30 <- subpop.merged.TF_scores.30[order(subpop.merged.TF_scores.30$low_low,decreasing=TRUE),] %>% na.omit() %>% pull(TF)
low_low.90 <- subpop.merged.TF_scores.90[order(subpop.merged.TF_scores.90$low_low,decreasing=TRUE),] %>% na.omit() %>% pull(TF)
low_low.150 <- subpop.merged.TF_scores.150[order(subpop.merged.TF_scores.150$low_low,decreasing=TRUE),] %>% na.omit() %>% pull(TF)
low_low.210 <- subpop.merged.TF_scores.210[order(subpop.merged.TF_scores.210$low_low,decreasing=TRUE),] %>% na.omit() %>% pull(TF)

rank.30 <- data.frame(TF=low_low.30,rank_30=seq(1,length(low_low.30),1))
rank.90 <- data.frame(TF=low_low.90,rank_90=seq(1,length(low_low.90),1))
rank.150 <- data.frame(TF=low_low.150,rank_150=seq(1,length(low_low.150),1))
rank.210 <- data.frame(TF=low_low.210,rank_210=seq(1,length(low_low.210),1))

rank.low_low <- rank.30 %>% inner_join(rank.90) %>% inner_join(rank.150) %>% inner_join(rank.210) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(
    highest_rank = min(rank_30,rank_90,rank_150,rank_210),
    lowest_rank = max(rank_30,rank_90,rank_150,rank_210)
  )

#write.xlsx(rank.low_low,"../output_tables/TF_rankings.xlsx", sheetName = "LOW_PRO_LOW_GLN",append = TRUE)

# 3. Add new column TOP target rank label accordingly
top5 <- rank.low_low %>% arrange(lowest_rank) %>% head(5) %>% pull(TF)
bottom5 <- rank.low_low %>% arrange(desc(highest_rank)) %>% head(5) %>% pull(TF)

timepoints <- c(rep(30,nrow(subpop.merged.TF_scores.30)),
                rep(90,nrow(subpop.merged.TF_scores.90)),
                rep(150,nrow(subpop.merged.TF_scores.150)),
                rep(210,nrow(subpop.merged.TF_scores.210)))

subpop.master <- bind_rows(subpop.merged.TF_scores.30,subpop.merged.TF_scores.90,subpop.merged.TF_scores.150,subpop.merged.TF_scores.210)
subpop.master$timepoints <- timepoints

# Add category to highlight top 5 most consistant TF
subpop.master.plot <- subpop.master %>% mutate(
  category = if_else(TF %in% top5, "top", if_else(TF %in% bottom5, "bottom", "none"))
)


# 4. Compute relative score across timepoints 
# Due to intensity variation across timepoints we compute a relative TF score by dividing 
# each score by the mean of the "none" score. This keeps the trend of the data.

relative_low_low <- subpop.master.plot %>%
  dplyr::group_by(timepoints) %>%
  dplyr::mutate(low_low_normalized = low_low / mean(low_low[category == "none"],na.rm=TRUE)) %>%
  ungroup()

# 5. Plot

colour_map = c(inferno(10)[6],inferno(10)[8],inferno(10)[2])

## Tracking TF intensity Low/High 
ggplot(relative_low_low,aes(x=timepoints, y=low_low_normalized, group = TF, label=TF)) +
  geom_line(data = relative_low_low[relative_low_low$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = relative_low_low[relative_low_low$category == "top",], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  geom_line(data = relative_low_low[relative_low_low$category == "bottom",], aes(color = inferno(10)[6]), alpha=0.8, size=0.5) +
  #ylim(0.8,1.2)+
  xlab("Time (min)")+
  ylab("Ratio TF nuclear intensity Low Pro vs Low Gln")+
  geom_label( 
    data=relative_low_low %>% filter(category=="top"),
    aes(label=TF,colour=inferno(10)[2]),
    size=5
  )+
  geom_label( 
    data=relative_low_low %>% filter(category=="bottom"),
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


ggsave("../figures/fig5f_SUPP_Low_Low_over_time.svg", width = 6, height = 3, bg='transparent')


targets <- c("Put3")
#targets <- c("Gln3","Gat1","Dal80","Gzf3","Dal81","Aro80","Rtg1","Rtg3","Rtg1","Rtg3","Snf1","Mig1")

colour_map = c(inferno(10)[8],inferno(10)[2])

ggplot(relative_low_low,aes(x=timepoints, y=low_low_normalized, group = TF, label=TF)) +
  geom_line(data = relative_low_low[relative_low_low$category == "none",], aes(color = inferno(10)[8]), alpha=0.1, size=0.5) +
  geom_line(data = relative_low_low[relative_low_low$TF %in% targets,], aes(color = inferno(10)[2]), alpha=0.8, size=0.5) +
  ylim(0.9,1.3)+ 
  xlab("Time (min)")+
  ylab("Ratio TF nuclear intensity Low Pro vs Low Gln")+
  geom_label( 
    data=relative_low_low[relative_low_low$TF %in% targets,],
    aes(label=TF,colour=inferno(10)[2]),
    size=5
  )+
  scale_colour_manual(values=colour_map)+
  #ylim(-0.02,0.02)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig5e_low_PRO_low_GLN_over_time_Put3.svg", width = 5, height = 3, bg='transparent')
#ggsave("../figures/fig5e_PRO_GLN_over_time.svg", width = 6, height = 3, bg='transparent')



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
ggsave("../figures/fig5b_scatter_size_RPL28_150min_uM.svg", width = 5, height = 5, bg='transparent')

plot_hist_2c(all,"size_um","condition","Cell size (uM)",c(3,22), c(0,0.20),c("NLIM-PRO","NLIM-GLN"))
ggsave("../figures/fig5b_hist_size_150min_uM.svg", width = 5, height = 5, bg='transparent')

plot_hist_2c(all,"meanRedValue","condition","pRPL28 fluorescence (mean intensity/cell)",c(100,340), c(0,0.03),c("NLIM-PRO","NLIM-GLN"))
ggsave("../figures/fig5b_hist_RPL28_150min_uM.svg", width = 5, height = 5, bg='transparent')









# First submission --------------------------------------------------------


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

PRO.30 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 30 mins Candidates v5.xlsx",sheet=1) 
PRO.90 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 1_30 Candidates v5.xlsx",sheet=1) 
PRO.150 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 2_30 Candidates v5.xlsx",sheet=1) 
PRO.210 <- read_xlsx("../source_data/fig3bcdef/May 3rd - PRO 3_30 Candidates v5.xlsx",sheet=1) 

GLN.30 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 30 mins Candidates v5.xlsx",sheet=1) 
GLN.90 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 1_30 Candidates v5.xlsx",sheet=1) 
GLN.150 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 2_30 Candidates v5.xlsx",sheet=1) 
GLN.210 <- read_xlsx("../source_data/fig3bcdef/May 3rd - GLN 3_30 Candidates v5.xlsx",sheet=1) 

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



#



data <- all[["PRO.30"]]

TF.info <- data %>% group_by(field.of.view,population) %>% 
  dplyr::summarize(
    TF_score_mean = mean(nuc_score,na.rm=TRUE),
    TF_score_sd = sd(nuc_score,na.rm=TRUE),
    TF_score_number = n(),
    TF_score_se = TF_score_sd/sqrt(TF_score_number)
  ) %>% inner_join(FOV_TF_mapping) 

TF.score <- TF.info %>%
  dplyr::select(field.of.view,population,TF_score_mean,TF) %>%
  pivot_wider(names_from = population, values_from = TF_score_mean) 

test.data <- subpop.TF_scores[["PRO.30"]][[2]]
test.info <- subpop.TF_scores[["PRO.30"]][[1]]


## ## ## ## ## ## ## ## ## ## ## ## 
##### Pull "QC" TFs and plot difference across conditions
## ## ## ## ## ## ## ## ## ## ## ## 

# Add TF annotations
all.annotated <- lapply(all,inner_join(FOV_TF_mapping))

# Gat1, Dal80, Rtg1, Ure2

# Pull all cells data
# Pull TF
# Box plot (pick whichever timepoint)

data1 <- all[["PRO.90"]]
data2 <- all[["GLN.90"]]

condition <- c(rep("PRO",nrow(data1)),rep("GLN",nrow(data2)))
data.master <- bind_rows(data1,data2)
data.master$condition <- condition

TF.plot <- data.master %>% inner_join(FOV_TF_mapping) %>% filter(TF == "Ure2")

ggplot(TF.plot,aes(y=meanGreenValue, x = factor(condition), color=factor(condition), fill=factor(condition))) + 
  geom_boxplot(aes(y=meanGreenValue, x = factor(condition), color=factor(condition)),colour="black",outlier.colour=NA, position=position_dodge(width=0.9))+
  geom_jitter(aes(y = meanGreenValue, x = factor(condition), color = factor(condition)),
              width = 0.2, alpha = 0.5, color = "blue") +  # Adjust width and color as needed
  ylab("Mean nuclear fluorescence")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")


df.plot <- pop.TF_scores.30 %>% na.omit()
df.plot <- df.plot %>%
  rowwise() %>%
  mutate(
    pvalue = t.test(c(TF_score_mean.x, TF_score_mean.y), 
                    c(TF_score_se.x, TF_score_se.y),
                    alternative = "two.sided", var.equal = FALSE)$p.value,
    significance = if_else(pvalue<=0.05,"True","False"),
  ) %>%
  ungroup()