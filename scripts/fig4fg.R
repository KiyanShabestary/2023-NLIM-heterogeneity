

require(dplyr)
require(ggplot2)
require(viridis)
require(growthcurver)
require(reshape2)
require(xlsx)
require(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Set well information
groups <- data.frame (well  = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12",
                                "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12",
                                "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
                                "D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12",
                                "E1","E2","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12",
                                "F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12",
                                "G1","G2","G3","G4","G5","G6","G7","G8","G9","G10","G11",
                                "H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11"),
                      name = c("NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO","NLIM-PRO",
                               "NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO","NREP-PRO",
                               "NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA","NLIM-UREA",
                               "NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA","NREP-UREA",
                               "NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE","NLIM-PHE",
                               "NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL","NLIM-VAL",
                               "NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR","NLIM-THR",
                               "NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR","NREP-THR"),
                      subpop = c("LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED",
                                 "LOW","LOW","LOW","LOW","HIGH","HIGH","HIGH","HIGH","UNSORTED","UNSORTED","UNSORTED"))

# Get data
data <- read.xlsx( "../source_data/fig4fg/20230222_FACS.xlsx", sheetIndex = 2) %>% 
  dplyr::mutate(time=Time..s./3600) %>% 
  dplyr::select_if(~ !any(is.na(.))) %>% 
  dplyr::select(time, everything())

# Set blank and substract
blank <- data$G12
data <- data %>% dplyr::select(-G12,-H12,-Time..s.,-Temp....C.)
data[,2:ncol(data)] <- data[,2:ncol(data)] - blank

# Create visualization plot to visualize fit (induvidual trajectories)
gc_out <- SummarizeGrowthByPlate(data, plot_fit = TRUE, 
                                 plot_file = "230222_gc_overview_FACS.pdf") 
head(gc_out)

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2])

# Plotting summarized data

# Melt data for ggplot
data.melted <- data %>% melt(id="time") %>% 
  inner_join(groups, by=c('variable'='well')) %>%
  dplyr::mutate(
    nitrogen = stringr::str_to_sentence(sub('.....', '',name)),
  )

# Adding mean and standard error ffor graph
data.melted.summarized <- data.melted %>% 
  group_by(time,subpop,name) %>%
  dplyr::summarize(
    OD.mean= mean(value),
    n=n(),
    OD.sd= sd(value),
    OD.stderror= OD.sd/sqrt(n),
  )

# Plotting individual points with confidence interval
data.plot <- data.melted %>% filter(name=="NLIM-PHE",time<13)

ggplot(data.plot, aes(x=time, y=value, color=subpop, fill=subpop))+
  geom_point(size=0.8)+
  geom_smooth(method="loess", formula = 'y ~ exp(x)')+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(NA,1.5)+
  ylab(expression("OD"["600"]*""))+
  xlab("Time (hours)")+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig4f_230222_growthcurve_NLIM_PHE.svg", width = 4, height = 3, bg='transparent')


# Plotting barchart of lag-time t_lag, defined as time required to reach two 
# doublings (which is equivalent to reach OD 0.2)

# Extract time when OD shoots over OD 0.2

# set threshold value
thresh <- 0.2

# get column indices for the growth columns of interest (e.g. "column2", "column3")
growth_cols <- colnames(data[,2:ncol(data)])
growth_col_indices <- sapply(growth_cols, function(x) which(colnames(data) == x))

# initialize list to store results
results <- list()

# loop through growth columns and find time points where growth exceeds threshold
for (i in seq_along(growth_col_indices)) {
  growth_col_index <- growth_col_indices[i]
  time_point <- which(data[, growth_col_index] > thresh)[1]
  col_name <- colnames(data)[growth_col_index]
  results[[col_name]] <- data[time_point, 1]
}

# Plot bar chart for all NLIM

df.t_lag <- results %>% as.data.frame()

df_long <- reshape(df.t_lag, 
                   idvar = "variable", 
                   varying = colnames(df.t_lag)[1:ncol(df.t_lag)], 
                   v.names = "value", 
                   times = colnames(df.t_lag)[1:ncol(df.t_lag)], 
                   direction = "long") %>% dplyr::rename(well=time,t_lag=value)

df_long$replicate <- c("rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3",
                       "rep1","rep2","rep3","rep4","rep1","rep2","rep3","rep4","rep1","rep2","rep3")

t_lags <- groups %>% inner_join(df_long, by=c('well'='well')) %>%
  dplyr::select(-well)%>%
  tidyr::pivot_wider(names_from =c('replicate'),
                     values_from ='t_lag')

# Add number of replicate to calculate error bars (important if missing value)
t_lags$rep.n <- apply(t_lags,1, function(x) (4-sum(is.na(x))))

t_lags <- t_lags %>% 
  rowwise() %>%
  dplyr::mutate(
    t_lag.mean = mean(c_across(c('rep1', 'rep2','rep3','rep4')),na.rm=TRUE),
    t_lag.sd = sd(c_across(c('rep1', 'rep2','rep3','rep4')),na.rm=TRUE),
    t_lag.stderror =  t_lag.sd/sqrt(rep.n),
    nitrogen = stringr::str_to_sentence(sub('.....', '',name)),
    type = substr(name, 1, 4),      
  )

saveRDS(t_lags, file = "../Rdata/230222_t_lags.Rds") 

t_lags.plot <- t_lags %>% dplyr::filter(type=="NLIM")
# Pull out mean of unsorted for graph ordering
t_lags.order <- t_lags.plot %>% dplyr::filter(subpop=="UNSORTED") %>% 
  dplyr::select(nitrogen,t_lag.mean) 
t_lags.plot <- t_lags.plot %>% dplyr::inner_join(t_lags.order,by=c('nitrogen'='nitrogen')) %>%
  dplyr::rename(order=t_lag.mean.y,t_lag.mean=t_lag.mean.x)

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2])

ggplot(t_lags.plot, aes(x=nitrogen, y=t_lag.mean,fill=subpop))+
  geom_bar( aes(x= reorder(nitrogen, desc(order)), y=t_lag.mean,  fill=subpop, colour=subpop), alpha=0.8, position="dodge", stat="identity", width=0.7)+
  geom_point(aes(x=nitrogen,y=rep1), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep2), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep3), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_point(aes(x=nitrogen,y=rep4), position=position_dodge(.7), size=1, alpha=1, colour="grey")+
  geom_errorbar(aes(ymin=t_lag.mean-t_lag.stderror, ymax=t_lag.mean+t_lag.stderror),position=position_dodge(.7), width=.3, color='black')+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylab("Lag time (hours)")+
  ylim(0,19)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig4g_230222_t_lag_NLIM.svg", width = 4, height = 3, bg='transparent')


# To calculate significance scores

t_lags.individual <- groups %>% inner_join(df_long, by=c('well'='well'))

x<-t_lags.individual %>% filter(name=="NLIM-THR",subpop=="LOW") %>% pull(t_lag)
y<-t_lags.individual %>% filter(name=="NLIM-THR",subpop=="HIGH") %>% pull(t_lag)

t.test(x, y, paired = FALSE, alternative = "two.sided")



