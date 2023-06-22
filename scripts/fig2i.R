

require(dplyr)
require(ggplot2)
require(viridis)
require(ggpmisc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Load lag times
lag_times <- readRDS(file = '../Rdata/230222_t_lags.rds')

# Load survival rates
survival_rates <- readRDS(file='../Rdata/230323_CLS_subpop.Rds') %>% filter(Time_d==10)

# Add identifier as condintion/name+subpop for merging
lag_times$ID <- with(lag_times, paste0(name, subpop))
survival_rates$ID <- with(survival_rates, paste0(Condition, Subpop))

# Merge both datasets
master <- lag_times %>% inner_join(survival_rates, by=c("ID"="ID"))

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2])
shape_map =c(1,2,5)

ggplot(master, aes(x=1/t_lag.mean, y=survival.mean))+
  geom_smooth(method='lm',colour="black", se=FALSE, formula = y ~ x)+
  geom_point(aes(colour=subpop,shape=type.x),size=3, alpha=1)+
  scale_colour_manual(values=colour_map)+
  scale_shape_manual(values=shape_map)+
  xlab(expression(paste("1/lag time (h"^"-1"*")")))+
  ylab("Survival (%)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2i_survival_lag_time_tradeoff.svg", width = 4.2, height = 3.3, bg='transparent')


x <- master %>% pull(t_lag.mean)
y <- master %>% pull(survival.mean)

cor(x,y, method = "spearman")



