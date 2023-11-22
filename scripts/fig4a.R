
# This script is divided into two parts. 

# PART 1 : Pre-processing of flow data. Store bimodality score in excel file
#           Note that due to flexmix sometimes stuck in local optima when trying
#           to compute EM. Running the script on multiple time with different seeds
#           might be required. 
# PART 2 : Plotting of the bimodality score obtained form PART 1. 
#          (PART2 can be run without running PART1)


require(ggplot2)
require(diptest)
require(flexmix)
require(dplyr)
require(scales)
require(viridis)
require(data.table)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# PART 1 : Compute bimodality scores --------------------------------------


# Change seed if flexmix stuck in local optima
set.seed(4) 

# Set file locations for each flow file
file_names <- read.csv('helper_file_names_replicates_NLIM.csv') 

# Extract condition names
conditions <- file_names$name

# Convert to list to lapply
file_names <- file_names %>% split(., seq(nrow(.)), drop=T)

# Function merge replicate files, perform analysis and export metrics
# as well as histogram with flexmix parameters for each condition
get_bimodality_stat <- function(file_name){
  
  # Define output for plot
  output.path <- paste("../figures/bimodality_results/bimodality_replicates_",file_name$name,".svg",sep='')
  
  # Define the replicates according to the helper file
  data.rep1 <- read.csv(file = file_name$frep1)
  data.rep2 <- read.csv(file = file_name$frep2)
  data.rep3 <- read.csv(file = file_name$frep3)
  
  #Merge data into one folder with rep name as new column
  
  rep <- c(rep('rep1',nrow(data.rep1)),
           rep('rep2',nrow(data.rep2)),
           rep('rep3',nrow(data.rep3)))
  
  data.master <- bind_rows(data.rep1, data.rep2, data.rep3)
  data.master$Rep <- rep
  
  # Compute dip test score on sizes and store output
  rep1.dip <- dip.test(data.rep1$FSC.H)
  rep2.dip <- dip.test(data.rep2$FSC.H)
  rep3.dip <- dip.test(data.rep3$FSC.H)
  
  dip.scores <- c(rep1.dip$statistic, rep1.dip$p.value,
                  rep2.dip$statistic, rep2.dip$p.value,
                  rep3.dip$statistic, rep3.dip$p.value)
  
  # Fitting two gaussians to our data (depending on dip.test pvalue)
  mo1 <- FLXMRglm(family = "gaussian")
  mo2 <- FLXMRglm(family = "gaussian")
  
  # Fitting data for each replicate and store parameters as well as ratio
  rep1.flexfit <- flexmix(data.rep1$FSC.H ~ 1, data = data.rep1, k = 2, model = list(mo1, mo2))
  rep2.flexfit <- flexmix(data.rep2$FSC.H ~ 1, data = data.rep2, k = 2, model = list(mo1, mo2))
  rep3.flexfit <- flexmix(data.rep3$FSC.H ~ 1, data = data.rep3, k = 2, model = list(mo1, mo2))
  
  rep1.c1 <- parameters(rep1.flexfit, component=1)[[1]]
  rep1.c2 <- parameters(rep1.flexfit, component=2)[[1]]
  rep2.c1 <- parameters(rep2.flexfit, component=1)[[1]]
  rep2.c2 <- parameters(rep2.flexfit, component=2)[[1]]
  rep3.c1 <- parameters(rep3.flexfit, component=1)[[1]]
  rep3.c2 <- parameters(rep3.flexfit, component=2)[[1]]
  
  rep1.ratio=min(rep1.c1[1],rep1.c2[1])/max(rep1.c1[1],rep1.c2[1])
  rep2.ratio=min(rep2.c1[1],rep2.c2[1])/max(rep2.c1[1],rep2.c2[1])
  rep3.ratio=min(rep3.c1[1],rep3.c2[1])/max(rep3.c1[1],rep3.c2[1])
  
  rep1.FC=(min(rep1.c1[1],rep1.c2[1])-max(rep1.c1[1],rep1.c2[1]))/max(rep1.c1[1],rep1.c2[1])
  rep2.FC=(min(rep2.c1[1],rep2.c2[1])-max(rep2.c1[1],rep2.c2[1]))/max(rep2.c1[1],rep2.c2[1])
  rep3.FC=(min(rep3.c1[1],rep3.c2[1])-max(rep3.c1[1],rep3.c2[1]))/max(rep3.c1[1],rep3.c2[1])
  
  params <- c(rep1.c1[1],
              rep1.c2[1],
              rep2.c1[1],
              rep2.c2[1],
              rep3.c1[1],
              rep3.c2[1],
              rep1.c1[2],
              rep1.c2[2],
              rep2.c1[2],
              rep2.c2[2],
              rep3.c1[2],
              rep3.c2[2],
              rep1.ratio,
              rep2.ratio,
              rep3.ratio,
              rep1.FC,
              rep2.FC,
              rep3.FC)
  
  # Visualization replicates (size)
  ggplot(data.master, aes(x=FSC.H, color=Rep)) + 
    geom_density(size=1)+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(300, 500000))+
    ylim(0, 4.2)+
    #scale_colour_manual(values=safe_colorblind_palette) +
    scale_colour_viridis(option='viridis',discrete=TRUE)+
    ylab("Density")+
    xlab("Size (FSC-H)")+
    theme_bw()+
    annotation_logticks(base = 10,sides = "b")+
    geom_vline(xintercept = rep1.c1[1], col = viridis(3)[1], size = 1, alpha=0.5) + 
    geom_vline(xintercept = rep1.c2[1], col = viridis(3)[1], size = 1, alpha=0.5) +
    geom_vline(xintercept = rep2.c1[1], col = viridis(3)[2], size = 1, alpha=0.5) + 
    geom_vline(xintercept = rep2.c2[1], col = viridis(3)[2], size = 1, alpha=0.5) +
    geom_vline(xintercept = rep3.c1[1], col = viridis(3)[3], size = 1, alpha=0.5) + 
    geom_vline(xintercept = rep3.c2[1], col = viridis(3)[3], size = 1, alpha=0.5) +
    theme(panel.background =  element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(color = "black"),
          legend.position = "none")
  
  ggsave(output.path, width = 3, height = 3, bg='transparent')
  
  output=c(dip.scores,params)
  output
}

bimodality_stats <- lapply(file_names, get_bimodality_stat)

header <- c('rep1.stat',
            'rep1.pval',
            'rep2.stat',
            'rep2.pval',
            'rep3.stat',
            'rep3.pval',
            'mean.rep1.c1',
            'mean.rep1.c2',
            'mean.rep2.c1',
            'mean.rep2.c2',
            'mean.rep3.c1',
            'mean.rep3.c2',
            'sd.rep1.c1',
            'sd.rep1.c2',
            'sd.rep2.c1',
            'sd.rep2.c2',
            'sd.rep3.c1',
            'sd.rep3.c2',
            'rep1.ratio',
            'rep2.ratio',
            'rep3.ratio',
            'rep1.FC',
            'rep2.FC',
            'rep3.FC')

# Writing final output
output.table=as.data.frame(bimodality_stats) %>% transpose()
colnames(output.table)=header
output.table$condition=conditions

write.csv(output.table,file='../output_tables/bimodality_scores_6h_seed_4.csv')

# PART 2 : Plot bimodality scores -----------------------------------------

require(ggplot2)
require(dplyr)
require(plyr)
require(readxl)
require(stringr)
require(viridis)
require(reshape2)


# Upload data
scores <- read.csv('../output_tables/bimodality_scores_HO_HLM_6h_triplicates_summary.csv')

# Compute means and sds for metrics representing the bimodality

scores <- scores %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    rep.stat.mean = mean(c_across(c('rep1.stat', 'rep2.stat','rep3.stat'))),
    rep.stat.sd = sd(c_across(c('rep1.stat', 'rep2.stat','rep3.stat'))),
    rep.stat.stderror =  rep.stat.sd/sqrt(3),
    rep.FC.mean = mean(c_across(c('rep1.FC', 'rep2.FC','rep3.FC'))),
    rep.FC.sd = sd(c_across(c('rep1.FC', 'rep2.FC','rep3.FC'))),
    rep.FC.mean.inv = -rep.FC.mean, # FC high to low
    rep.FC.stderror = rep.FC.sd/sqrt(3),
    control = if_else(condition == 'NSTARVE', 'y', 'n'),
    nitrogen = if_else(control=='n',stringr::str_to_sentence(sub('.....', '',condition)),'None'),
  )

saveRDS(scores, file = "../Rdata/bimodality_scores_HO_HLM_triplicates.rds")



# Add boolean marker for p<0.05, p<0.005, p<0.0005

# Add control row


# Separate in two datasets for NLIM and NREP
scores.NLIM <- scores %>% filter(!grepl('NREP', condition))
scores.NREP <- scores %>% filter(!grepl('NLIM', condition))

# show_col(viridis(6))
#colour_map = c(cividis(6)[5],cividis(6)[3])
colour_map = c(inferno(10)[9],inferno(10)[2])

# Bimodality scores NLIM series
ggplot(scores.NLIM, aes(x=nitrogen, y=rep.stat.mean,fill=control))+
  geom_bar(aes(x=reorder(nitrogen, plyr::desc(rep.stat.mean)), y=rep.stat.mean,  fill=control, colour=control), stat="identity", alpha=0.8, width=0.7)+
  geom_point(aes(x=nitrogen,y=rep1.stat),size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep2.stat),size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep3.stat),size=1, alpha=1, colour="grey")+
  ylim(0, 0.05)+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  geom_errorbar(aes(ymin=rep.stat.mean-rep.stat.stderror, ymax=rep.stat.mean+rep.stat.stderror), width=.3, color='black')+ 
  ylab("Bimodality score (a.u.)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig4a_bimodality_bar_chart.svg", width = 6, height = 3, bg='transparent') #4/3

# Bimodality scores NREP series
ggplot(scores.NREP, aes(x=nitrogen, y=rep.stat.mean,fill=control))+
  geom_bar( aes(x= reorder(nitrogen, plyr::desc(rep.stat.mean)), y=rep.stat.mean,  fill=control, colour=control), stat="identity", alpha=0.8, width=0.7)+
  geom_point(aes(x=nitrogen,y=rep1.stat),size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep2.stat),size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep3.stat),size=1, alpha=1, colour="grey")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(0, 0.07)+
  geom_errorbar(aes(ymin=rep.stat.mean-rep.stat.stderror, ymax=rep.stat.mean+rep.stat.stderror), width=.3, color='black')+ 
  ylab("Bimodality score (a.u.)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig4a_SUPP_bimodality_bar_chart_NREP.svg", width = 4, height = 3, bg='transparent')

