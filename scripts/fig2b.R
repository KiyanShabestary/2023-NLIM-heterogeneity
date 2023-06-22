
# Load libraries
require(dplyr)
require(xlsx)
require(ggplot2)
require(viridis)
require(growthcurver)
require(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

get_local_growth_params <- function(OD,time,trim_at_time,min_t){
  # Function that computes local growth parameters for a given column of a 
  # dataframe. To be used with apply.
  # Input:
  #       - column representing time in hours
  #       - each other column representing the timecourse of a given well, already blanked
  #       - trim_at_time value in hours. This is the the upper time value for fitting 
  #       - min_t in hours. This is the minimal time under which max is not computed
  # Output:
  #       - vector containing local (mu_lin) and exponential (mu_log) growth 
  #         estimates as well as doubling time (from mu_log).
  
  
  
  # Compute fit to get smooth version of the curve
  gc_fit <- SummarizeGrowth(data_t = time, 
                            data_n = OD,
                            t_trim = trim_at_time,
                            bg_correct = "none")
  
  print(OD)
  
  # Extract the fit and binds it to the time column
  fit <- cbind(gc_fit$data$t, predict(gc_fit$model)) %>%
    as.data.frame() %>% dplyr::rename(time = V1, OD_fit = V2)
  rownames(fit)<-NULL
  
  # Compute log of the predicted fitted data
  fit$ln_OD_fit <- log(fit$OD_fit) # In R, log function is (logically) the natural logarithm
  
  # Only consider timepoints above the treshhold
  fit <- fit %>% filter(time>min_t)
  
  # Calculate differences between OD and time at each timestep 
  # (note that in R: diff_i <- step_i+1 - step_i)
  local_rates = diff(as.matrix(fit)) %>% as.data.frame()
  
  local_rates$mu_lin <- local_rates$OD_fit/local_rates$time
  local_rates$mu_log <- local_rates$ln_OD_fit/local_rates$time
  
  max_mu_lin <- max(local_rates$mu_lin)
  max_mu_log <- max(local_rates$mu_log)
  
  min_doubling_time <- log(2)/max_mu_log
  
  c(max_mu_lin,max_mu_log,min_doubling_time)
}


groups <- data.frame (well  = c("A2","A3","A4","A5","A6","A7","A8","A9","A10",
                                "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12",
                                "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
                                "D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12",
                                "E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12",
                                "F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12",
                                "G1","G2","G3","G4","G5","G6"),
                      name = c("Pro","Pro","Pro","Gln","Gln","Gln","Glu","Glu","Glu",
                               "Nh4","Nh4","Nh4","Urea","Urea","Urea","Ala","Ala","Ala","Asp","Asp","Asp",
                               "Trp","Trp","Trp","Met","Met","Met","Phe","Phe","Phe","Arg","Arg","Arg",
                               "Lys","Lys","Lys","His","His","His","Thr","Thr","Thr","Gly","Gly","Gly",
                               "Val","Val","Val","Leu","Leu","Leu","Cys","Cys","Cys","Asn","Asn","Asn",
                               "Ile","Ile","Ile","Tyr","Tyr","Tyr","Ser","Ser","Ser","Gaba","Gaba","Gaba",
                               "Orn","Orn","Orn","None","None","None"),
                      control = c("n","n","n","n","n","n","n","n","n",
                                  "n","n","n","n","n","n","n","n","n","n","n","n",
                                  "n","n","n","n","n","n","n","n","n","n","n","n",
                                  "n","n","n","n","n","n","n","n","n","n","n","n",
                                  "n","n","n","n","n","n","n","n","n","n","n","n",
                                  "n","n","n","n","n","n","n","n","n","n","n","n",
                                  "n","n","n","y","y","y"))


data <- read.xlsx( "../source_data/fig2b/20230112_HO_HLUM_NLIM_aa.xlsx", sheetIndex = 2) %>% 
  mutate(time=Time..s./3600) %>% 
  select_if(~ !any(is.na(.))) %>% 
  select(time, everything())

# Set blank and substract
blank <- data$A11
data <- data %>% select(-G7,-G8,-G9,-G10,-G11,-A11,-Cycle.Nr.,-Time..s.,-Temp....C.)
#data[,2:ncol(data)] <- data[,2:ncol(data)] - blank # This messes up the predict function

# Create visualization plot to visualize fit and detect outliers (optional)
gc_out <- SummarizeGrowthByPlate(data, plot_fit = TRUE, 
                                 plot_file = "230112_NLIM_gc_overview.pdf") 
head(gc_out)

# Compute local growth parameters for each well

growth_params <- data %>% 
  select(-time) %>% 
  apply(MARGIN=2, FUN= get_local_growth_params, data$time, 20, 5) %>%
  t() %>% as.data.frame() %>% dplyr::rename(max_mu_lin=V1,max_mu_log=V2,min_td=V3)
growth_params$well <- rownames(growth_params)
rownames(growth_params)<-NULL

growth_params <- growth_params %>%
  inner_join(groups, by=c('well'='well'))

# Removing outliers with sudden drop in OD 
outlier = c("D12","F7","F12","G1")

growth_params <- growth_params %>%
  filter (!is.element(well,outlier))

# None growth curve is used as comparison for NREP
saveRDS(growth_params,file="../Rdata/230112_growth_params.Rds")

# Compute mean and sd per condition
growth_scores_by_group <- growth_params %>% 
  group_by(name,control) %>% 
  summarize(
    max_mu_lin.mean = mean(max_mu_lin),
    max_mu_lin.sd = sd(max_mu_lin),
    max_mu_log.mean = mean(max_mu_log),
    max_mu_log.sd = sd(max_mu_log),
    min_td.mean = mean(min_td),
    min_td.sd = sd(min_td),
  )

saveRDS(growth_scores_by_group, file = "../Rdata/230112_growth_params_by_group_NLIM.Rds")


# Plot

colour_map = c(inferno(10)[9],inferno(10)[2])

# Maximal growth rate for all conditions
ggplot(growth_scores_by_group, aes(x=name, y=max_mu_log.mean,fill=control))+
  geom_bar( aes(x= reorder(name, desc(max_mu_log.mean)), y=max_mu_log.mean,  fill=control, colour=control), stat="identity", alpha=0.8, width=0.7)+
  geom_point(data=growth_params, aes(x=name,y=max_mu_log),size=1, alpha=1, colour="grey")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  geom_errorbar(aes(ymin=max_mu_log.mean-max_mu_log.sd, ymax=max_mu_log.mean+max_mu_log.sd), width=.3, color='black')+ 
  ylab(expression(paste("Maximal growth rate (h"^"-1"*")")))+
  ylim(0,0.062)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        axis.title.x=element_blank(),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2b_230112_NLIM_max_growth.svg", width = 4, height = 3, bg='transparent')
