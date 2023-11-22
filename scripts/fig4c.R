

require(dplyr)
require(ggplot2)
require(viridis)
require(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load bimodality scores (NLIM,NREP,NSTARVE)
bimodality_scores <- readRDS(file = '../Rdata/bimodality_scores_HO_HLM_triplicates.rds')
bimodality_scores$name <- bimodality_scores$condition
bimodality_scores$condition <- c(rep("NLIM",24),'NSTARVE',rep("NREP",15))

# Load growth scores (NLIM,NSTARVE)
growth_scores.NLIM <- readRDS(file = '../Rdata/230112_growth_params_by_group_NLIM.Rds')
growth_scores.NLIM$condition <- c(rep("NLIM",15),'NSTARVE',rep("NLIM",9))

# Load growth scores (NREP)
growth_scores.NREP <- readRDS(file = '../Rdata/230118_growth_params_by_group_NREP.Rds')
growth_scores.NREP$condition <- rep("NREP",16)

# Merge growth scores
growth_scores <- rbind(growth_scores.NLIM,growth_scores.NREP)

# Merge datasets
master <- bimodality_scores %>% 
  inner_join(growth_scores, by = c('nitrogen'='name',"condition"="condition")) %>%
  filter(max_mu_log.mean>0.01)

# Plot bimodality vs growth rate
colour_map = c(inferno(10)[9],inferno(10)[5],inferno(10)[2])
shape_map =c(1,2,5)

ggplot(master, aes(x=rep.stat.mean, y=max_mu_log.mean,shape=condition))+
  geom_smooth(aes(colour=condition),method='lm', se=FALSE, formula = y ~ x)+
  xlim(0.001,0.04)+
  geom_point(aes(colour=condition), size=3, alpha=1)+
  scale_colour_manual(values=colour_map)+
  scale_shape_manual(values=shape_map)+
  xlab("Bimodality score (a.u.)")+
  ylab(expression(paste("Maximal growth rate (h"^"-1"*")")))+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig4c_bimodality_max_growth_curve.svg", width = 4, height = 3, bg='transparent')


# Correlation between maximal growth rate and bimodality score

Condition="NREP"

x <- master %>% filter(condition==Condition) %>% pull(rep.stat.mean)
y <- master %>% filter(condition==Condition) %>% pull(max_mu_log.mean)

cor(x,y, method = "spearman")


# Correlation NREP vs NLIM for bimodality and growth scores

bimodality_NREP_NLIM <- master %>% select(condition,nitrogen,rep.stat.mean) %>%
  tidyr::pivot_wider(names_from = condition, values_from = rep.stat.mean) %>%
  select(-NSTARVE) %>% na.omit()

growth_NREP_NLIM <- master %>% select(condition,nitrogen,max_mu_log.mean) %>%
  tidyr::pivot_wider(names_from = condition, values_from = max_mu_log.mean) %>%
  select(-NSTARVE) %>% na.omit()


ggplot(bimodality_NREP_NLIM, aes(x=NLIM, y=NREP, label=nitrogen))+
  geom_smooth(method='lm', se=FALSE, formula = y ~ x, colour = "grey")+
  stat_function(fun=function(x) x, linetype="dashed")+
  geom_point(alpha=1)+
  geom_text(hjust=-0.1, vjust=-0.1)+
  xlab("Bimodality score (a.u.) - NLIM")+
  ylab("Bimodality score (a.u.) - NREP")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/NLIM_NREP_comparison/growth_NLIM_NREP.svg", width = 4, height = 3, bg='transparent')

ggplot(growth_NREP_NLIM, aes(x=NLIM, y=NREP, label=nitrogen))+
  geom_smooth(method='lm', se=FALSE, formula = y ~ x, colour = "grey")+
  stat_function(fun=function(x) x, linetype="dashed")+
  geom_point(alpha=1)+
  geom_text(hjust=-0.1, vjust=-0.1)+
  xlim(0,0.16)+
  xlab(expression(paste("Maximal growth rate (h"^"-1"*") - NLIM")))+
  ylab(expression(paste("Maximal growth rate (h"^"-1"*") - NREP")))+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/NLIM_NREP_comparison/bimodality_NLIM_NREP.svg", width = 4, height = 3, bg='transparent')

cor(bimodality_NREP_NLIM$NLIM,bimodality_NREP_NLIM$NREP, method="spearman")
cor(growth_NREP_NLIM$NLIM,growth_NREP_NLIM$NREP, method="spearman")

