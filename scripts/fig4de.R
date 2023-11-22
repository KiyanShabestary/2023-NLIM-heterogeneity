

require(dplyr)
require(xlsx)
require(ggplot2)
require(viridis)
require(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load CLS and transform
CLS.data <- read.xlsx(file = "../source_data/fig4d/CLS_measurements_R.xlsx" ,2, header=TRUE) %>%
  mutate(
    nitrogen = stringr::str_to_sentence(sub('.....', '',Condition)),
  ) 

# Add NLIM and NREP types
CLS.data$type <- rep(c(rep('NLIM',12),
                       rep('NREP',12),
                       rep('NLIM',12),
                       rep('NREP',12),
                       rep('NLIM',12),
                       rep('NLIM',12),
                       rep('NLIM',12),
                       rep('NREP',12)),
                     3)

CLS.data <- CLS.data %>%
  dplyr::select(nitrogen,type,Condition,Survival,Replicate,Subpop,Time_d) %>% 
  pivot_wider(names_from =c('Replicate'),
              values_from ='Survival')

# Add number of replicate to calculate error bars (important if missing value)
CLS.data$rep.n <- apply(CLS.data,1, function(x) (4-sum(is.na(x))))

CLS.data <- CLS.data %>% rowwise() %>%
  dplyr::mutate(
    survival.mean = mean(c_across(c('rep1','rep2','rep3','rep4')),na.rm=TRUE),
    survival.sd = sd(c_across(c('rep1','rep2','rep3','rep4')),na.rm=TRUE),
    survival.stderror = survival.sd/sqrt(rep.n)
  )

saveRDS(CLS.data, file = "../Rdata/230323_CLS_subpop.Rds") 

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2]) #HIGH,UNSORTED,LOW

# CLS over time

CLS.data.amino_acid <- CLS.data %>% filter(nitrogen=='Val',type=='NLIM') # Change here for other conditions

ggplot(CLS.data.amino_acid, aes(x=Time_d, y=survival.mean,fill=Subpop))+
  geom_line( aes(x= Time_d, y=survival.mean, colour=Subpop), stat="identity", alpha=1)+
  geom_point(aes(x=Time_d,y=rep1,colour=Subpop), size=1, alpha=1)+
  geom_point(aes(x=Time_d,y=rep2,colour=Subpop), size=1, alpha=1)+
  geom_point(aes(x=Time_d,y=rep3,colour=Subpop), size=1, alpha=1)+
  geom_point(aes(x=Time_d,y=rep4,colour=Subpop), size=1, alpha=1)+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  geom_errorbar(aes(ymin=survival.mean-survival.stderror, ymax=survival.mean+survival.stderror), width=0.7, size=0.7, color='black')+ 
  ylab("Survival rate (%)")+
  xlab("Time (days)")+
  ylim(0,105)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig4d_CLS_subpop_time_NLIM_VAL.svg", width = 4, height = 3, bg='transparent')

# CLS bar chart for subpop after 20d (why not 30d ?)

CLS.data.time <- CLS.data %>% dplyr::filter(Time_d==30,type=="NLIM")

# Pull out mean of unsorted for graph ordering
CLS.order <- CLS.data.time %>% dplyr::filter(Subpop=="UNSORTED") %>% 
  dplyr::select(nitrogen,survival.mean) 
CLS.data.time <- CLS.data.time %>% inner_join(CLS.order,by=c('nitrogen'='nitrogen')) %>%
  dplyr::rename(order=survival.mean.y,survival.mean=survival.mean.x)

ggplot(CLS.data.time, aes(x=nitrogen, y=survival.mean,fill=Subpop))+
  geom_bar( aes(x= reorder(nitrogen, desc(order)), y=survival.mean,  fill=Subpop, colour=Subpop), alpha=0.8, position="dodge", stat="identity", width=0.7)+
  geom_point(aes(x=nitrogen,y=rep1), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep2), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep3), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep4), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_errorbar(aes(ymin=survival.mean-survival.stderror, ymax=survival.mean+survival.stderror),position=position_dodge(.7), width=.3, color='black')+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylab("Survival rate after 30d (%)")+
  ylim(0,105)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig4e_CLS_subpop_30d.svg", width = 4, height = 3, bg='transparent')


# To calculate significance scores

CLS_long <- CLS.data %>% pivot_longer(cols=c('rep1','rep2','rep3','rep4'),
                                      names_to = "replicates",
                                      values_to = "survival")

x<-CLS_long %>% filter(Time_d=="30", Condition=="NLIM-PHE", Subpop=="LOW") %>% pull(survival)
y<-CLS_long %>% filter(Time_d=="30", Condition=="NLIM-PHE", Subpop=="HIGH") %>% pull(survival)

t.test(x, y, paired = FALSE, alternative = "two.sided")

