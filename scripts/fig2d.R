

require(tidyr)
require(dplyr)
require(xlsx)
require(ggplot2)
require(viridis)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load CLS and transform
CLS.data <- read.xlsx(file = "../source_data/fig2d/CLS_measurements_R.xlsx" ,1, header=TRUE) %>%
  mutate(
    control = if_else(Condition == 'NSTARVE', 'y', 'n'),
    nitrogen = if_else(control=='n',stringr::str_to_sentence(sub('.....', '',Condition)),'None'),
  ) %>% 
  filter(Time_d==20) %>%
  filter(!grepl('NREP', Condition)) %>%
  select(nitrogen,Survival,Replicate,control) %>% 
  pivot_wider(names_from =c('Replicate'),
              values_from ='Survival')

CLS.data$rep.n <- apply(CLS.data,1, function(x) (3-sum(is.na(x))))

CLS.data <- CLS.data %>% rowwise() %>%
  mutate(
    survival.mean = mean(c_across(c('rep1', 'rep2','rep3')),na.rm=TRUE),
    survival.sd = sd(c_across(c('rep1', 'rep2','rep3')),na.rm=TRUE),
    survival.stderror =  survival.sd/sqrt(rep.n),
  )

saveRDS(CLS.data, file = '../Rdata/lifespan_scores.rds')


colour_map = c(inferno(10)[9],inferno(10)[2])

# Bimodality scores NLIM series
ggplot(CLS.data, aes(x=nitrogen, y=survival.mean,fill=control))+
  geom_bar( aes(x= reorder(nitrogen, desc(survival.mean)), y=survival.mean,  fill=control, colour=control), stat="identity", alpha=0.8, width=0.7)+
  geom_point(aes(x=nitrogen,y=rep1),size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep2),size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep3),size=1, alpha=1, colour="grey")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  geom_errorbar(aes(ymin=survival.mean-survival.stderror, ymax=survival.mean+survival.stderror), width=.3, color='black')+ 
  ylab("Survival rate after 20d (%)")+
  ylim(0,105)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig2d_CLS_NLIM_20d_TECAN.svg", width = 4, height = 3, bg='transparent')











