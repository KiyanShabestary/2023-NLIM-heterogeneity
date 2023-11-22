
require(ggplot2)
require(viridis)
require(readxl)
require(dplyr)
require(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

source("util.R")

# fig2a -------------------------------------------------------------------

PRO.preshift <- read.csv(file = '../source_data/fig2ab/1230_H9_pre_pRPL28_1.csv')
GLN.preshift <- read.csv(file = '../source_data/fig2ab/1230_H10_pre_pRPL28_2.csv')
PRO.2h <- read.csv(file = '../source_data/fig2ab/221121_1630_A1_PRO_GFP.csv')
GLN.2h <- read.csv(file = '../source_data/fig2ab/221121_1630_A2_GLN_GFP.csv')
PRO.4h <- read.csv(file = '../source_data/fig2ab/221121_1830_E1_PRO_GFP.csv')
GLN.4h <- read.csv(file = '../source_data/fig2ab/221121_1830_E2_GLN_GFP.csv')
PRO.6h <- read.csv(file = '../source_data/fig2ab/221121_2030_A1_PRO_GFP.csv')
GLN.6h <- read.csv(file = '../source_data/fig2ab/221121_2030_A2_GLN_GFP.csv')
PRO.8h <- read.csv(file = '../source_data/fig2ab/221121_2230_E1_PRO_GFP.csv')
GLN.8h <- read.csv(file = '../source_data/fig2ab/221121_2230_E2_GLN_GFP.csv')
PRO2.2h <- read.csv(file = '../source_data/fig2ab/1430_C2_pRPL28_NLIMPRO.csv')
GLN2.2h <- read.csv(file = '../source_data/fig2ab/1430_C3_pRPL28_NLIMGLN.csv')
PRO2.4h <- read.csv(file = '../source_data/fig2ab/1630_F2_pRPL28_NLIMPRO.csv')
GLN2.4h <- read.csv(file = '../source_data/fig2ab/1630_F3_pRPL28_NLIMGLN.csv')
PRO2.6h <- read.csv(file = '../source_data/fig2ab/1830_C2_pRPL28_NLIMPRO.csv')
GLN2.6h <- read.csv(file = '../source_data/fig2ab/1830_C3_pRPL28_NLIMGLN.csv')
PRO2.8h <- read.csv(file = '../source_data/fig2ab/2030_F2_pRPL28_NLIMPRO.csv')
GLN2.8h <- read.csv(file = '../source_data/fig2ab/2030_F3_pRPL28_NLIMGLN.csv')
PRO3.preshift <- read.csv(file = '../source_data/fig2ab/231012_preshift_1.csv')
GLN3.preshift <- read.csv(file = '../source_data/fig2ab/231012_preshift_2.csv')
PRO3.2h <- read.csv(file = '../source_data/fig2ab/231012_1440_NLIMPRO.csv')
GLN3.2h <- read.csv(file = '../source_data/fig2ab/231012_1440_NLIMGLN.csv')
PRO3.4h <- read.csv(file = '../source_data/fig2ab/231012_1640_NLIMPRO.csv')
GLN3.4h <- read.csv(file = '../source_data/fig2ab/231012_1640_NLIMGLN.csv')
PRO3.6h <- read.csv(file = '../source_data/fig2ab/231012_1840_NLIMPRO.csv')
GLN3.6h <- read.csv(file = '../source_data/fig2ab/231012_1840_NLIMGLN.csv')
PRO3.8h <- read.csv(file = '../source_data/fig2ab/231012_2040_NLIMPRO.csv')
GLN3.8h <- read.csv(file = '../source_data/fig2ab/231012_2040_NLIMGLN.csv')


conditions <- c(rep('PRO',nrow(PRO.preshift)),
                rep('GLN',nrow(GLN.preshift)),
                rep('PRO',nrow(PRO.2h)),
                rep('GLN',nrow(GLN.2h)),
                rep('PRO',nrow(PRO.4h)),
                rep('GLN',nrow(GLN.4h)),
                rep('PRO',nrow(PRO.6h)),
                rep('GLN',nrow(GLN.6h)),
                rep('PRO',nrow(PRO.8h)),
                rep('GLN',nrow(GLN.8h)),
                rep('PRO',nrow(PRO2.2h)),
                rep('GLN',nrow(GLN2.2h)),
                rep('PRO',nrow(PRO2.4h)),
                rep('GLN',nrow(GLN2.4h)),
                rep('PRO',nrow(PRO2.6h)),
                rep('GLN',nrow(GLN2.6h)),
                rep('PRO',nrow(PRO2.8h)),
                rep('GLN',nrow(GLN2.8h)),
                rep('PRO',nrow(PRO3.preshift)),
                rep('GLN',nrow(GLN3.preshift)),
                rep('PRO',nrow(PRO3.2h)),
                rep('GLN',nrow(GLN3.2h)),
                rep('PRO',nrow(PRO3.4h)),
                rep('GLN',nrow(GLN3.4h)),
                rep('PRO',nrow(PRO3.6h)),
                rep('GLN',nrow(GLN3.6h)),
                rep('PRO',nrow(PRO3.8h)),
                rep('GLN',nrow(GLN3.8h)))

time <- c(rep('preshift',nrow(PRO.preshift)),
          rep('preshift',nrow(GLN.preshift)),
          rep('2h',nrow(PRO.2h)),
          rep('2h',nrow(GLN.2h)),
          rep('4h',nrow(PRO.4h)),
          rep('4h',nrow(GLN.4h)),
          rep('6h',nrow(PRO.6h)),
          rep('6h',nrow(GLN.6h)),
          rep('8h',nrow(PRO.8h)),
          rep('8h',nrow(GLN.8h)),
          rep('2h',nrow(PRO2.2h)),
          rep('2h',nrow(GLN2.2h)),
          rep('4h',nrow(PRO2.4h)),
          rep('4h',nrow(GLN2.4h)),
          rep('6h',nrow(PRO2.6h)),
          rep('6h',nrow(GLN2.6h)),
          rep('8h',nrow(PRO2.8h)),
          rep('8h',nrow(GLN2.8h)),
          rep('preshift',nrow(PRO3.preshift)),
          rep('preshift',nrow(GLN3.preshift)),
          rep('2h',nrow(PRO3.2h)),
          rep('2h',nrow(GLN3.2h)),
          rep('4h',nrow(PRO3.4h)),
          rep('4h',nrow(GLN3.4h)),
          rep('6h',nrow(PRO3.6h)),
          rep('6h',nrow(GLN3.6h)),
          rep('8h',nrow(PRO3.8h)),
          rep('8h',nrow(GLN3.8h)))

replicate <- c(rep('rep1',nrow(PRO.preshift)),
          rep('rep1',nrow(GLN.preshift)),
          rep('rep1',nrow(PRO.2h)),
          rep('rep1',nrow(GLN.2h)),
          rep('rep1',nrow(PRO.4h)),
          rep('rep1',nrow(GLN.4h)),
          rep('rep1',nrow(PRO.6h)),
          rep('rep1',nrow(GLN.6h)),
          rep('rep1',nrow(PRO.8h)),
          rep('rep1',nrow(GLN.8h)),
          rep('rep2',nrow(PRO2.2h)),
          rep('rep2',nrow(GLN2.2h)),
          rep('rep2',nrow(PRO2.4h)),
          rep('rep2',nrow(GLN2.4h)),
          rep('rep2',nrow(PRO2.6h)),
          rep('rep2',nrow(GLN2.6h)),
          rep('rep2',nrow(PRO2.8h)),
          rep('rep2',nrow(GLN2.8h)),
          rep('rep3',nrow(PRO3.preshift)),
          rep('rep3',nrow(GLN3.preshift)),
          rep('rep3',nrow(PRO3.2h)),
          rep('rep3',nrow(GLN3.2h)),
          rep('rep3',nrow(PRO3.4h)),
          rep('rep3',nrow(GLN3.4h)),
          rep('rep3',nrow(PRO3.6h)),
          rep('rep3',nrow(GLN3.6h)),
          rep('rep3',nrow(PRO3.8h)),
          rep('rep3',nrow(GLN3.8h)))

data <- bind_rows(PRO.preshift, GLN.preshift, PRO.2h, GLN.2h, PRO.4h, GLN.4h, PRO.6h, GLN.6h, PRO.8h, GLN.8h,
                  PRO2.2h, GLN2.2h, PRO2.4h, GLN2.4h, PRO2.6h, GLN2.6h, PRO2.8h, GLN2.8h,
                  PRO3.preshift, GLN3.preshift, PRO3.2h, GLN3.2h, PRO3.4h, GLN3.4h, PRO3.6h, GLN3.6h, PRO3.8h, GLN3.8h)

data$conditions <- conditions
data$time <- time
data$replicate <- replicate

# Supp figure - Micro-heterogeneity - CV

# Summary of SDs and mean for all population
bulk_summary <- data %>% 
  group_by(conditions,time,replicate) %>% 
  summarize(mean_pRPL28 = mean(BL1.H),
            sd_pRPL28 = sd(BL1.H),
            CV_pRPL28 = sd_pRPL28/mean_pRPL28,
            mean_size = mean(FSC.H),
            sd_size = sd(FSC.H),
            CV_size = sd_size/mean_size)

time_mapping <- c("2h" = 2, "4h" = 4, "6h" = 6, "8h" = 8,  "preshift" = 0)

# Use match to convert time values
bulk_summary$numeric_time <- time_mapping[as.character(bulk_summary$time)]

colour_map = c(inferno(10)[9],inferno(10)[2])

ggplot(bulk_summary, aes(x=numeric_time, y=CV_size, colour=conditions)) + 
  geom_point()+
  xlab("Time (h)")+
  ylab("Size CV")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Define the desired order for the time variable
time_order <- c("preshift", "2h", "4h", "6h", "8h")

# Convert time to a factor with the desired order
bulk_summary$time <- factor(bulk_summary$time, levels = time_order)

# Assuming df is your dataframe with columns conditions, time, replicate, count_high, count_low, and ratio_low_to_high
mean_bulk_summary <- bulk_summary %>%
  group_by(time, conditions) %>%
  summarize(
    mean_mean_pRPL28 = mean(mean_pRPL28), # Mean across replicates (mean of the means of the individual samples)
    mean_sd_pRPL28 = mean(sd_pRPL28),
    mean_CV_pRPL28 = mean(CV_pRPL28),
    mean_mean_size = mean(mean_size),
    mean_sd_size = mean(sd_size),
    mean_CV_size = mean(CV_size),
    se_mean_pRPL28 = sd(mean_pRPL28)/ sqrt(n()), 
    se_sd_pRPL28 = sd(sd_pRPL28)/ sqrt(n()),
    se_CV_pRPL28 = sd(CV_pRPL28)/ sqrt(n()),
    se_mean_size = sd(mean_size)/ sqrt(n()),
    se_sd_size = sd(sd_size)/ sqrt(n()),
    se_CV_size = sd(CV_size)/ sqrt(n()))

colour_map = c(inferno(10)[9],inferno(10)[2])

# Bar chart of size CV
ggplot(mean_bulk_summary, aes(x = time, y = mean_CV_size, fill = conditions)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),  alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin = mean_CV_size - se_CV_size, ymax = mean_CV_size + se_CV_size), position = position_dodge(width = 0.9), width = 0.4) +
  geom_point(data = bulk_summary, aes(x = time, y = CV_size, group = conditions), position = position_dodge(width = 0.9), size=1, alpha=1, colour="grey") +
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(NA,0.7)+
  labs(y = "Cell size CV") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig2a_SUPP_CV_size.svg", width = 3, height = 3, bg='transparent')

# Statistics
p1 = bulk_summary %>% filter(time == "8h" & conditions =="PRO") %>% pull(CV_size)
p2 = bulk_summary %>% filter(time == "8h" & conditions =="GLN") %>% pull(CV_size)
t.test(p1, p2, paired = FALSE, alternative = "greater")


# Bar chart of pRPL28 CV
ggplot(mean_bulk_summary, aes(x = time, y = mean_CV_pRPL28, fill = conditions)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),  alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin = mean_CV_pRPL28 - se_CV_pRPL28, ymax = mean_CV_pRPL28 + se_CV_pRPL28), position = position_dodge(width = 0.9), width = 0.4) +
  geom_point(data = bulk_summary, aes(x = time, y = CV_pRPL28, group = conditions), position = position_dodge(width = 0.9), size=1, alpha=1, colour="grey") +
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(NA,0.7)+
  labs(y = "pRPL28 CV") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

# Statistics
p1 = bulk_summary %>% filter(time == "6h" & conditions =="PRO") %>% pull(CV_pRPL28)
p2 = bulk_summary %>% filter(time == "6h" & conditions =="GLN") %>% pull(CV_pRPL28)
t.test(p1, p2, paired = FALSE, alternative = "greater")

ggsave("../figures/fig2a_SUPP_CV_pRPL28.svg", width = 3, height = 3, bg='transparent')

# Order colour for plotting
col_fct = factor(data$conditions, levels=c("GLN","PRO")) # Order on plot
data$col_fct = col_fct

colour_map = c(inferno(10)[9],inferno(10)[2])

ggplot(data%>%filter(replicate=="rep1"), aes(x=FSC.H, y=BL1.H, colour = col_fct)) + 
  #geom_point(alpha=0.01)+
  geom_density_2d()+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  xlim(8000,200000)+  
  scale_y_continuous(labels = scientific)+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  theme_bw()+
  facet_grid(conditions~factor(time, levels=c('preshift', '2h', '4h', '6h', '8h')))+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2a_time_course.svg", width = 6.8, height = 3, bg='transparent')

ggplot(data%>%filter(replicate=="rep1"), aes(x=BL1.H, colour = col_fct)) +
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  #scale_colour_viridis(option='inferno',discrete=TRUE)+ #Should be colour map
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  ylab("Density (a.u.)")+
  xlab("pRPL28 fluorescence (BL1-H)")+
  theme_classic()+
  facet_grid(conditions~factor(time, levels=c('preshift', '2h', '4h', '6h', '8h')))+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave("../figures/fig2a_SUPP_time_course_BL1.svg", width = 6.8, height = 3, bg='transparent')

ggplot(data%>%filter(replicate=="rep1"), aes(x=FSC.H, colour = col_fct)) + 
  geom_density(size=1)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  #scale_colour_viridis(option='inferno',discrete=TRUE)+ # Should be colour map
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  ylab("Density (a.u.)")+
  xlab("Cell size (FSC-H)")+
  theme_classic()+
  facet_grid(conditions~factor(time, levels=c('preshift', '2h', '4h', '6h', '8h')))+
  annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave("../figures/fig2a_SUPP_time_course_FSC.svg", width = 6.8, height = 3, bg='transparent')


# fig2b and 2c -------------------------------------------------------------------

clustered_data <- flexmix_MV_clustering(data%>%filter(time=="4h" & conditions=="PRO" & replicate=="rep1"))

# Clustered scatter and histograms
colour_map = c(inferno(10)[8],inferno(10)[6], inferno(10)[2])

clustered_data_plot <- clustered_data %>% filter(classification == "High")

ggplot(clustered_data_plot, aes(x=FSC.H, y=BL1.H)) + 
  geom_density2d(aes(colour=classification), bins=6, alpha=0.6)+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  xlim(8000,170000)+ 
  scale_y_continuous(labels = scientific, limits = c(0,140000))+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2b_clustered_scatter_PRO_HIGH.svg", width = 2, height = 2, bg='transparent')

ggplot(clustered_data, aes(x=BL1.H)) +
  geom_density(aes(y = after_stat(count)),size=1)+
  geom_density(aes(y = after_stat(count), fill=classification, group=classification), size=1, alpha=0.5)+
  #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x)),
  #              limits= c(300, 500000))+
  #xlim(8000,170000)+ #FSC-H
  xlim(NA,140000)+ #BL1-H
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  #scale_y_continuous(labels=function(x)x/10000,lim=c(NA,35000))+
  ylab("Density (a.u.)")+
  xlab("Cell size (FSC-H)")+
  theme_classic()+
  #annotation_logticks(base = 10,sides = "b")+
  theme(panel.background =  element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks = element_line(color = "black"),
        legend.position = "none")

ggsave("../figures/fig2b_clustered_BL1_linear_scale.svg", width = 2.5, height = 2.5, bg='transparent')

###### Computing separation line between Low and High

# Manually retrieving flexmix centroids 

mo1 <- FLXMCmvnorm()
mo2 <- FLXMCmvnorm()

data_mv_fit <- data%>%filter(time=="4h" & conditions=="PRO" & replicate=="rep1") %>% 
  dplyr::select(all_of(c("FSC.H","BL1.H")))

set.seed(123)

flexfit <- flexmix(as.matrix(data_mv_fit) ~ 1, data = data_mv_fit, k = 2, model = list(mo1,mo2))

# Retrieve fit parameters
c1 <- parameters(flexfit, component=1)[[1]]
c2 <- parameters(flexfit, component=2)[[1]]

#### Computing separation line

c_low = c(c1[1],c1[2])
c_high = c(c2[1],c2[2])

slope = (c_high[2]-c_low[2])/(c_high[1]-c_low[1])

# mid point is the median of the unassigned data
summary(clustered_data %>% filter(classification == "Unknown"))
mid_point = c(72621,33895)

inverse_slope = -1/slope
y_intercept = mid_point[2]-inverse_slope*mid_point[1]
##

ggplot(clustered_data, aes(x=FSC.H, y=BL1.H)) + 
  geom_density2d(bins=6, alpha=0.6)+
  geom_function(fun = function(x) inverse_slope*x+y_intercept)+
  geom_point(aes(x=c_low[1], y=c_low[2]), colour="blue")+
  geom_point(aes(x=c_high[1], y=c_high[2]), colour="blue")+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  xlim(8000,170000)+ 
  scale_y_continuous(labels = scientific, limits = c(0,140000))+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2b_clustered_scatter_slope.svg", width = 2, height = 2, bg='transparent')


##### Calculating ratio Low/High over time

# Hard assignment of subpopulation based on hard tresshold defined for NLIM-PRO 4h Cell size/GFP fluorescence

tresholded_data <- data %>% mutate(
  hard_assign = ifelse(BL1.H<inverse_slope*FSC.H+y_intercept,"Low","High")
)

# QC: Check how clustering performed (geom_point) on all datapoint / replicate / conditions

#tresholded_plot <- tresholded_data %>% filter(time!="preshift")

sampled_data <- tresholded_plot[sample(nrow(tresholded_plot), 40000), ]

ggplot(sampled_data%>%filter(replicate=="rep1"), aes(x=FSC.H, y=BL1.H)) + 
  geom_point(aes(colour= hard_assign),alpha=0.05)+
  geom_density_2d(size=0.5, colour=inferno(10)[2],alpha=0.4)+
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  #xlim(8000,200000)+ #170000
  scale_y_continuous(labels = scientific, limits = c(0,140000))+
  scale_x_continuous(labels = scientific, limits = c(8000,200000))+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  facet_grid(factor(conditions,levels=c("PRO","GLN"))~factor(time, levels=c('preshift','2h', '4h', '6h', '8h')))+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2b_SUPP_tresholded_data.svg", width = 6.8, height = 3, bg='transparent')

timecourse_ratio <- tresholded_data %>% 
  dplyr::select(c("conditions","time","replicate","hard_assign")) %>%
  group_by(conditions,time,replicate) %>%
  summarize(
    count_high = sum(hard_assign == "High"),
    count_low = sum(hard_assign == "Low")
  ) %>%
  mutate(
    ratio_low_to_high = count_low / count_high
  )

# Use match to convert time values
time_mapping <- c("2h" = 2, "4h" = 4, "6h" = 6, "8h" = 8,  "preshift" = 0)
timecourse_ratio$numeric_time <- time_mapping[as.character(timecourse_ratio$time)]

# Define the desired order for the time variable
time_order <- c("preshift", "2h", "4h", "6h", "8h")

# Convert time to a factor with the desired order
timecourse_ratio$time <- factor(timecourse_ratio$time, levels = time_order)

# Assuming df is your dataframe with columns conditions, time, replicate, count_high, count_low, and ratio_low_to_high
mean_ratios <- timecourse_ratio %>%
  group_by(time, conditions) %>%
  summarize(
    mean_ratio = mean(ratio_low_to_high),
    se = sd(ratio_low_to_high) / sqrt(n())
  )

colour_map = c(inferno(10)[9],inferno(10)[2])

# Create the bar chart with error bars
ggplot(mean_ratios, aes(x = time, y = mean_ratio, fill = conditions)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),  alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin = mean_ratio - se, ymax = mean_ratio + se), position = position_dodge(width = 0.9), width = 0.4) +
  geom_point(data = timecourse_ratio, aes(x = time, y = ratio_low_to_high, group = conditions), position = position_dodge(width = 0.9), size=1, alpha=1, colour="grey") +
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  ylim(NA,1.5)+
  labs(y = "Ratio Low to High") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig2c_timecourse_ratio.svg", width = 2.1, height = 2, bg='transparent')

p1 = timecourse_ratio %>% filter(time == "4h" & conditions =="PRO") %>% pull(ratio_low_to_high)
p2 = timecourse_ratio %>% filter(time == "4h" & conditions =="GLN") %>% pull(ratio_low_to_high)
t.test(p1, p2, paired = FALSE, alternative = "greater")

# fig2d -------------------------------------------------------------------
# Subpopulation RNA sequencing

deg_list <- read_xlsx("../source_data/fig2c/CORE_LOWvsEXT_HIGH_deg.xlsx",sheet=1)

deg_list <- deg_list %>% mutate(
  significance = if_else(((padj<=0.05) & (abs(log2FoldChange) >=1)),"True","False"),
  log10_padj = -log10(padj)
)

#colour_map = c(inferno(10)[2],inferno(10)[9])
colour_map = c(viridis_pal()(20)[1],viridis_pal()(20)[13])

ggplot(deg_list,aes(x=log2FoldChange,y=log10_padj, colour=significance, label=gene_id)) + 
  geom_point(size=1.4, alpha=0.4) +
  geom_text()+
  xlab("log2 fold change")+
  ylab("-log10 p-value")+
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2c_volcano_HIGH_EXT_CORE_LOW.svg", width = 5, height = 3.5, bg='transparent')


# Supp figures -------------------------------------------------------------------

### ###
### Overlapping subpop RNAseq with scRNAseq
### ###

# Importing scRNAseq DESeq2 table

scRNAseq_data <- read_xlsx("../output_tables/DESeq2_scRNAseq.xlsx")

# Merge both datasets
# Only select entries whose padj < 0.05
data <- deg_list %>% inner_join(scRNAseq_data, by=c("gene_id"="locus")) %>%
  filter(padj.x < 0.05 & padj.y < 0.05)

# Display scatter plot
ggplot(data,aes(x=log2FoldChange,y=log2FC, label=gene.standardName)) + 
  geom_point(size=1.4, alpha=0.6) +
  geom_smooth(method='lm', se=FALSE, formula = y ~ x, colour="black")+
  xlab("log2 fold change - popRNAseq")+
  ylab("log2 fold change - scRNAseq")+
  geom_label()+
  ylim(-4,6)+
  scale_x_continuous(n.breaks = 6)+
  theme_bw()+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2d_subpopRNASeq_scRNAseq.svg", width = 5, height = 3.7, bg='transparent')


# Compute correlation / stats Wilcoxon rank sum test ?
popRNAseq = data %>% pull(log2FoldChange)
scRNAseq = data %>% pull(log2FC)

cor(popRNAseq,scRNAseq, method = "spearman")

### ###
### Dynamics of sorted fractions
### ###

# PART1 : Check how sorted fractions evolve in GFP fluorescence and cell size

PRO.LOW.2h.rep1 <- read.csv(file = '../source_data/x_fig2x/2h_G1_PRO_LOW.csv')
PRO.HIGH.2h.rep1 <- read.csv(file = '../source_data/x_fig2x/2h_G2_PRO_HIGH.csv')
PRO.ALL.2h.rep1 <- read.csv(file = '../source_data/x_fig2x/2h_G3_PRO_ALL.csv')
GLN.LOW.2h.rep1 <- read.csv(file = '../source_data/x_fig2x/2h_G4_GLN_LOW.csv')
GLN.HIGH.2h.rep1 <- read.csv(file = '../source_data/x_fig2x/2h_G5_GLN_HIGH.csv')
GLN.ALL.2h.rep1 <- read.csv(file = '../source_data/x_fig2x/2h_G6_GLN_ALL.csv')
PRO.LOW.2h.rep2 <- read.csv(file = '../source_data/x_fig2x/2h_G7_PRO_LOW.csv')
PRO.HIGH.2h.rep2 <- read.csv(file = '../source_data/x_fig2x/2h_G8_PRO_HIGH.csv')
PRO.ALL.2h.rep2 <- read.csv(file = '../source_data/x_fig2x/2h_G9_PRO_ALL.csv')
GLN.LOW.2h.rep2 <- read.csv(file = '../source_data/x_fig2x/2h_G10_GLN_LOW.csv')
GLN.HIGH.2h.rep2 <- read.csv(file = '../source_data/x_fig2x/2h_G11_GLN_HIGH.csv')
GLN.ALL.2h.rep2 <- read.csv(file = '../source_data/x_fig2x/2h_G12_GLN_ALL.csv')

PRO.LOW.4h.rep1 <- read.csv(file = '../source_data/x_fig2x/4h_H1_PRO_LOW.csv')
PRO.HIGH.4h.rep1 <- read.csv(file = '../source_data/x_fig2x/4h_H2_PRO_HIGH.csv')
PRO.ALL.4h.rep1 <- read.csv(file = '../source_data/x_fig2x/4h_H3_PRO_ALL.csv')
GLN.LOW.4h.rep1 <- read.csv(file = '../source_data/x_fig2x/4h_H4_GLN_LOW.csv')
GLN.HIGH.4h.rep1 <- read.csv(file = '../source_data/x_fig2x/4h_H5_GLN_HIGH.csv')
GLN.ALL.4h.rep1 <- read.csv(file = '../source_data/x_fig2x/4h_H6_GLN_ALL.csv')
PRO.LOW.4h.rep2 <- read.csv(file = '../source_data/x_fig2x/4h_H7_PRO_LOW.csv')
PRO.HIGH.4h.rep2 <- read.csv(file = '../source_data/x_fig2x/4h_H8_PRO_HIGH.csv')
PRO.ALL.4h.rep2 <- read.csv(file = '../source_data/x_fig2x/4h_H9_PRO_ALL.csv')
GLN.LOW.4h.rep2 <- read.csv(file = '../source_data/x_fig2x/4h_H10_GLN_LOW.csv')
GLN.HIGH.4h.rep2 <- read.csv(file = '../source_data/x_fig2x/4h_H11_GLN_HIGH.csv')
GLN.ALL.4h.rep2 <- read.csv(file = '../source_data/x_fig2x/4h_H12_GLN_ALL.csv')

PRO.LOW.6h.rep1 <- read.csv(file = '../source_data/x_fig2x/6h_C1_PRO_LOW.csv')
PRO.HIGH.6h.rep1 <- read.csv(file = '../source_data/x_fig2x/6h_C2_PRO_HIGH.csv')
PRO.ALL.6h.rep1 <- read.csv(file = '../source_data/x_fig2x/6h_C3_PRO_ALL.csv')
GLN.LOW.6h.rep1 <- read.csv(file = '../source_data/x_fig2x/6h_C4_GLN_LOW.csv')
GLN.HIGH.6h.rep1 <- read.csv(file = '../source_data/x_fig2x/6h_C5_GLN_HIGH.csv')
GLN.ALL.6h.rep1 <- read.csv(file = '../source_data/x_fig2x/6h_C6_GLN_ALL.csv')
PRO.LOW.6h.rep2 <- read.csv(file = '../source_data/x_fig2x/6h_C7_PRO_LOW.csv')
PRO.HIGH.6h.rep2 <- read.csv(file = '../source_data/x_fig2x/6h_C8_PRO_HIGH.csv')
PRO.ALL.6h.rep2 <- read.csv(file = '../source_data/x_fig2x/6h_C9_PRO_ALL.csv')
GLN.LOW.6h.rep2 <- read.csv(file = '../source_data/x_fig2x/6h_C10_GLN_LOW.csv')
GLN.HIGH.6h.rep2 <- read.csv(file = '../source_data/x_fig2x/6h_C11_GLN_HIGH.csv')
GLN.ALL.6h.rep2 <- read.csv(file = '../source_data/x_fig2x/6h_C12_GLN_ALL.csv')

conditions <- c(rep('PRO',nrow(PRO.LOW.2h.rep1)),#2h
                rep('PRO',nrow(PRO.HIGH.2h.rep1)),
                rep('PRO',nrow(PRO.ALL.2h.rep1)),
                rep('GLN',nrow(GLN.LOW.2h.rep1)),
                rep('GLN',nrow(GLN.HIGH.2h.rep1)),
                rep('GLN',nrow(GLN.ALL.2h.rep1)),
                rep('PRO',nrow(PRO.LOW.2h.rep2)),
                rep('PRO',nrow(PRO.HIGH.2h.rep2)),
                rep('PRO',nrow(PRO.ALL.2h.rep2)),
                rep('GLN',nrow(GLN.LOW.2h.rep2)),
                rep('GLN',nrow(GLN.HIGH.2h.rep2)),
                rep('GLN',nrow(GLN.ALL.2h.rep2)),
                rep('PRO',nrow(PRO.LOW.4h.rep1)),#4h
                rep('PRO',nrow(PRO.HIGH.4h.rep1)),
                rep('PRO',nrow(PRO.ALL.4h.rep1)),
                rep('GLN',nrow(GLN.LOW.4h.rep1)),
                rep('GLN',nrow(GLN.HIGH.4h.rep1)),
                rep('GLN',nrow(GLN.ALL.4h.rep1)),
                rep('PRO',nrow(PRO.LOW.4h.rep2)),
                rep('PRO',nrow(PRO.HIGH.4h.rep2)),
                rep('PRO',nrow(PRO.ALL.4h.rep2)),
                rep('GLN',nrow(GLN.LOW.4h.rep2)),
                rep('GLN',nrow(GLN.HIGH.4h.rep2)),
                rep('GLN',nrow(GLN.ALL.4h.rep2)),
                rep('PRO',nrow(PRO.LOW.6h.rep1)),#6h
                rep('PRO',nrow(PRO.HIGH.6h.rep1)),
                rep('PRO',nrow(PRO.ALL.6h.rep1)),
                rep('GLN',nrow(GLN.LOW.6h.rep1)),
                rep('GLN',nrow(GLN.HIGH.6h.rep1)),
                rep('GLN',nrow(GLN.ALL.6h.rep1)),
                rep('PRO',nrow(PRO.LOW.6h.rep2)),
                rep('PRO',nrow(PRO.HIGH.6h.rep2)),
                rep('PRO',nrow(PRO.ALL.6h.rep2)),
                rep('GLN',nrow(GLN.LOW.6h.rep2)),
                rep('GLN',nrow(GLN.HIGH.6h.rep2)),
                rep('GLN',nrow(GLN.ALL.6h.rep2)))

time <- c(rep('2h',nrow(PRO.LOW.2h.rep1)),#2h
                rep('2h',nrow(PRO.HIGH.2h.rep1)),
                rep('2h',nrow(PRO.ALL.2h.rep1)),
                rep('2h',nrow(GLN.LOW.2h.rep1)),
                rep('2h',nrow(GLN.HIGH.2h.rep1)),
                rep('2h',nrow(GLN.ALL.2h.rep1)),
                rep('2h',nrow(PRO.LOW.2h.rep2)),
                rep('2h',nrow(PRO.HIGH.2h.rep2)),
                rep('2h',nrow(PRO.ALL.2h.rep2)),
                rep('2h',nrow(GLN.LOW.2h.rep2)),
                rep('2h',nrow(GLN.HIGH.2h.rep2)),
                rep('2h',nrow(GLN.ALL.2h.rep2)),
                rep('4h',nrow(PRO.LOW.4h.rep1)),#4h
                rep('4h',nrow(PRO.HIGH.4h.rep1)),
                rep('4h',nrow(PRO.ALL.4h.rep1)),
                rep('4h',nrow(GLN.LOW.4h.rep1)),
                rep('4h',nrow(GLN.HIGH.4h.rep1)),
                rep('4h',nrow(GLN.ALL.4h.rep1)),
                rep('4h',nrow(PRO.LOW.4h.rep2)),
                rep('4h',nrow(PRO.HIGH.4h.rep2)),
                rep('4h',nrow(PRO.ALL.4h.rep2)),
                rep('4h',nrow(GLN.LOW.4h.rep2)),
                rep('4h',nrow(GLN.HIGH.4h.rep2)),
                rep('4h',nrow(GLN.ALL.4h.rep2)),
                rep('6h',nrow(PRO.LOW.6h.rep1)),#6h
                rep('6h',nrow(PRO.HIGH.6h.rep1)),
                rep('6h',nrow(PRO.ALL.6h.rep1)),
                rep('6h',nrow(GLN.LOW.6h.rep1)),
                rep('6h',nrow(GLN.HIGH.6h.rep1)),
                rep('6h',nrow(GLN.ALL.6h.rep1)),
                rep('6h',nrow(PRO.LOW.6h.rep2)),
                rep('6h',nrow(PRO.HIGH.6h.rep2)),
                rep('6h',nrow(PRO.ALL.6h.rep2)),
                rep('6h',nrow(GLN.LOW.6h.rep2)),
                rep('6h',nrow(GLN.HIGH.6h.rep2)),
                rep('6h',nrow(GLN.ALL.6h.rep2)))



replicate <- c(rep('rep1',nrow(PRO.LOW.2h.rep1)),#2h
               rep('rep1',nrow(PRO.HIGH.2h.rep1)),
               rep('rep1',nrow(PRO.ALL.2h.rep1)),
               rep('rep1',nrow(GLN.LOW.2h.rep1)),
               rep('rep1',nrow(GLN.HIGH.2h.rep1)),
               rep('rep1',nrow(GLN.ALL.2h.rep1)),
               rep('rep2',nrow(PRO.LOW.2h.rep2)),
               rep('rep2',nrow(PRO.HIGH.2h.rep2)),
               rep('rep2',nrow(PRO.ALL.2h.rep2)),
               rep('rep2',nrow(GLN.LOW.2h.rep2)),
               rep('rep2',nrow(GLN.HIGH.2h.rep2)),
               rep('rep2',nrow(GLN.ALL.2h.rep2)),
               rep('rep1',nrow(PRO.LOW.4h.rep1)),#4h
               rep('rep1',nrow(PRO.HIGH.4h.rep1)),
               rep('rep1',nrow(PRO.ALL.4h.rep1)),
               rep('rep1',nrow(GLN.LOW.4h.rep1)),
               rep('rep1',nrow(GLN.HIGH.4h.rep1)),
               rep('rep1',nrow(GLN.ALL.4h.rep1)),
               rep('rep2',nrow(PRO.LOW.4h.rep2)),
               rep('rep2',nrow(PRO.HIGH.4h.rep2)),
               rep('rep2',nrow(PRO.ALL.4h.rep2)),
               rep('rep2',nrow(GLN.LOW.4h.rep2)),
               rep('rep2',nrow(GLN.HIGH.4h.rep2)),
               rep('rep2',nrow(GLN.ALL.4h.rep2)),
               rep('rep1',nrow(PRO.LOW.6h.rep1)),#6h
               rep('rep1',nrow(PRO.HIGH.6h.rep1)),
               rep('rep1',nrow(PRO.ALL.6h.rep1)),
               rep('rep1',nrow(GLN.LOW.6h.rep1)),
               rep('rep1',nrow(GLN.HIGH.6h.rep1)),
               rep('rep1',nrow(GLN.ALL.6h.rep1)),
               rep('rep2',nrow(PRO.LOW.6h.rep2)),
               rep('rep2',nrow(PRO.HIGH.6h.rep2)),
               rep('rep2',nrow(PRO.ALL.6h.rep2)),
               rep('rep2',nrow(GLN.LOW.6h.rep2)),
               rep('rep2',nrow(GLN.HIGH.6h.rep2)),
               rep('rep2',nrow(GLN.ALL.6h.rep2)))

subpopulation <- c(rep('Low',nrow(PRO.LOW.2h.rep1)),#2h
               rep('High',nrow(PRO.HIGH.2h.rep1)),
               rep('Unsorted',nrow(PRO.ALL.2h.rep1)),
               rep('Low',nrow(GLN.LOW.2h.rep1)),
               rep('High',nrow(GLN.HIGH.2h.rep1)),
               rep('Unsorted',nrow(GLN.ALL.2h.rep1)),
               rep('Low',nrow(PRO.LOW.2h.rep2)),
               rep('High',nrow(PRO.HIGH.2h.rep2)),
               rep('Unsorted',nrow(PRO.ALL.2h.rep2)),
               rep('Low',nrow(GLN.LOW.2h.rep2)),
               rep('High',nrow(GLN.HIGH.2h.rep2)),
               rep('Unsorted',nrow(GLN.ALL.2h.rep2)),
               rep('Low',nrow(PRO.LOW.4h.rep1)),#4h
               rep('High',nrow(PRO.HIGH.4h.rep1)),
               rep('Unsorted',nrow(PRO.ALL.4h.rep1)),
               rep('Low',nrow(GLN.LOW.4h.rep1)),
               rep('High',nrow(GLN.HIGH.4h.rep1)),
               rep('Unsorted',nrow(GLN.ALL.4h.rep1)),
               rep('Low',nrow(PRO.LOW.4h.rep2)),
               rep('High',nrow(PRO.HIGH.4h.rep2)),
               rep('Unsorted',nrow(PRO.ALL.4h.rep2)),
               rep('Low',nrow(GLN.LOW.4h.rep2)),
               rep('High',nrow(GLN.HIGH.4h.rep2)),
               rep('Unsorted',nrow(GLN.ALL.4h.rep2)),
               rep('Low',nrow(PRO.LOW.6h.rep1)),#6h
               rep('High',nrow(PRO.HIGH.6h.rep1)),
               rep('Unsorted',nrow(PRO.ALL.6h.rep1)),
               rep('Low',nrow(GLN.LOW.6h.rep1)),
               rep('High',nrow(GLN.HIGH.6h.rep1)),
               rep('Unsorted',nrow(GLN.ALL.6h.rep1)),
               rep('Low',nrow(PRO.LOW.6h.rep2)),
               rep('High',nrow(PRO.HIGH.6h.rep2)),
               rep('Unsorted',nrow(PRO.ALL.6h.rep2)),
               rep('Low',nrow(GLN.LOW.6h.rep2)),
               rep('High',nrow(GLN.HIGH.6h.rep2)),
               rep('Unsorted',nrow(GLN.ALL.6h.rep2)))

sorted_data <- bind_rows(PRO.LOW.2h.rep1,PRO.HIGH.2h.rep1,PRO.ALL.2h.rep1,GLN.LOW.2h.rep1,GLN.HIGH.2h.rep1,GLN.ALL.2h.rep1,
                  PRO.LOW.2h.rep2,PRO.HIGH.2h.rep2,PRO.ALL.2h.rep2,GLN.LOW.2h.rep2,GLN.HIGH.2h.rep2,GLN.ALL.2h.rep2,
                  PRO.LOW.4h.rep1,PRO.HIGH.4h.rep1,PRO.ALL.4h.rep1,GLN.LOW.4h.rep1,GLN.HIGH.4h.rep1,GLN.ALL.4h.rep1,
                  PRO.LOW.4h.rep2,PRO.HIGH.4h.rep2,PRO.ALL.4h.rep2,GLN.LOW.4h.rep2,GLN.HIGH.4h.rep2,GLN.ALL.4h.rep2,
                  PRO.LOW.6h.rep1,PRO.HIGH.6h.rep1,PRO.ALL.6h.rep1,GLN.LOW.6h.rep1,GLN.HIGH.6h.rep1,GLN.ALL.6h.rep1,
                  PRO.LOW.6h.rep2,PRO.HIGH.6h.rep2,PRO.ALL.6h.rep2,GLN.LOW.6h.rep2,GLN.HIGH.6h.rep2,GLN.ALL.6h.rep2) 

sorted_data$conditions <- conditions
sorted_data$time <- time
sorted_data$replicate <- replicate
sorted_data$subpopulation <- subpopulation

colour_map = c(inferno(10)[9],inferno(10)[2])

cond_fct = factor(sorted_data$conditions, levels=c("GLN","PRO")) # Order on plot
sorted_data$cond_fct = cond_fct

subpop_fct = factor(sorted_data$subpopulation, levels=c("High","Low","Unsorted")) # Order on plot
sorted_data$subpop_fct = subpop_fct

# PLotting one of the replicate
ggplot(sorted_data%>%dplyr::filter(replicate=="rep1",conditions=="GLN"), aes(x=FSC.H, y=BL1.H)) + 
  geom_point(alpha=0.01)+
  geom_density_2d(colour=inferno(10)[2])+ # inferno(10)[9] GLN
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  #xlim(8000,200000)+  
  scale_y_continuous(labels = scientific)+
  scale_x_continuous(labels = scientific,lim=c(8000,200000))+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  theme_bw()+
  facet_grid(subpopulation~factor(time, levels=c('2h', '4h', '6h')))+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2_SUPP_time_course_sorted_fraction_GLN.svg", width = 6, height = 4, bg='transparent')

# Compare cell size resumption Low

sorted_data_summary <- sorted_data %>% filter(replicate=="rep1") %>% group_by(conditions,time,subpopulation) %>%
  summarize(mean_pRPL28 = mean(BL1.H),
            sd_pRPL28 = sd(BL1.H),
            se_pRPL28 = sd_pRPL28/sqrt(n()),
            mean_size = mean(FSC.H),
            sd_size = sd(FSC.H),
            se_size = sd_size/sqrt(n()))
            

colour_map = c(inferno(10)[8],inferno(10)[6],inferno(10)[2])

# Cell size
ggplot(sorted_data_summary, aes(x = conditions, y = mean_size, group = subpopulation, fill= subpopulation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),  alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin = mean_size - se_size, ymax = mean_size + se_size), position = position_dodge(width = 0.9), width = 0.4) +
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  scale_y_continuous(labels = scientific,limits = c(NA,150000))+
  facet_grid(~factor(time, levels=c('2h', '4h', '6h')))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig2_SUPP_time_course_sorted_fraction_mean_size.svg", width = 5, height = 3.7, bg='transparent')


# pRPL28 intensity
ggplot(sorted_data_summary, aes(x = conditions, y = mean_pRPL28, group = subpopulation, fill= subpopulation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9),  alpha=0.8, width=0.7) +
  geom_errorbar(aes(ymin = mean_pRPL28 - se_pRPL28, ymax = mean_pRPL28 + se_pRPL28), position = position_dodge(width = 0.9), width = 0.4) +
  scale_colour_manual(values=colour_map)+
  scale_fill_manual(values=colour_map)+
  scale_y_continuous(labels = scientific,limits = c(NA,80000))+
  facet_grid(~factor(time, levels=c('2h', '4h', '6h')))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5),
        panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

ggsave("../figures/fig2_SUPP_time_course_sorted_fraction_mean_pRPL28.svg", width = 5, height = 3.7, bg='transparent')


p1 = sorted_data %>% filter(time == "6h" & conditions =="PRO" & subpopulation =="Low") %>% pull(FSC.H)
p2 = sorted_data %>% filter(time == "6h" & conditions =="GLN" & subpopulation =="Low") %>% pull(FSC.H)
t.test(p1, p2, paired = FALSE, alternative = "two.sided")

### ###
### Effect on PBS wash on heterogeneity
### ###

PRO.2h <- read.csv(file = '../source_data/x_fig2x/231012_1440_NLIMPRO.csv')
GLN.2h <- read.csv(file = '../source_data/x_fig2x/231012_1440_NLIMGLN.csv')
PRO.4h <- read.csv(file = '../source_data/x_fig2x/231012_1640_NLIMPRO.csv')
GLN.4h <- read.csv(file = '../source_data/x_fig2x/231012_1640_NLIMGLN.csv')
PRO.6h <- read.csv(file = '../source_data/x_fig2x/231012_1840_NLIMPRO.csv')
GLN.6h <- read.csv(file = '../source_data/x_fig2x/231012_1840_NLIMGLN.csv')

PRO.PBS.2h <- read.csv(file = '../source_data/x_fig2x/231012_1640_NLIMPRO_2h_PBS.csv')
GLN.PBS.2h <- read.csv(file = '../source_data/x_fig2x/231012_1640_NLIMGLN_2h_PBS.csv')
PRO.PBS.4h <- read.csv(file = '../source_data/x_fig2x/231012_1840_NLIMPRO_2h_PBS.csv')
GLN.PBS.4h <- read.csv(file = '../source_data/x_fig2x/231012_1840_NLIMGLN_2h_PBS.csv')
PRO.PBS.6h <- read.csv(file = '../source_data/x_fig2x/231012_2040_NLIMPRO_2h_PBS.csv')
GLN.PBS.6h <- read.csv(file = '../source_data/x_fig2x/231012_2040_NLIMGLN_2h_PBS.csv')

# Add time
time <- c(rep('2h',nrow(PRO.2h)+nrow(GLN.2h)),
          rep('4h',nrow(PRO.4h)+nrow(GLN.4h)),
          rep('6h',nrow(PRO.6h)+nrow(GLN.6h)),
          rep('2h',nrow(PRO.PBS.2h)+nrow(GLN.PBS.2h)),
          rep('4h',nrow(PRO.PBS.4h)+nrow(GLN.PBS.4h)),
          rep('6h',nrow(PRO.PBS.6h)+nrow(GLN.PBS.6h)))

# Add condition
conditions <- c(rep('PRO',nrow(PRO.2h)),
                rep('GLN',nrow(GLN.2h)),
                rep('PRO',nrow(PRO.4h)),
                rep('GLN',nrow(GLN.4h)),
                rep('PRO',nrow(PRO.6h)),
                rep('GLN',nrow(GLN.6h)),
                rep('PRO',nrow(PRO.PBS.2h)),
                rep('GLN',nrow(GLN.PBS.2h)),
                rep('PRO',nrow(PRO.PBS.4h)),
                rep('GLN',nrow(GLN.PBS.4h)),
                rep('PRO',nrow(PRO.PBS.6h)),
                rep('GLN',nrow(GLN.PBS.6h)))
                
# Add PBS parameter
PBS <- c(rep('Normal',nrow(PRO.2h)+nrow(GLN.2h)+nrow(PRO.4h)+nrow(GLN.4h)+nrow(PRO.6h)+nrow(GLN.6h)),
         rep('2h PBS',nrow(PRO.PBS.2h)+nrow(GLN.PBS.2h)+nrow(PRO.PBS.4h)+nrow(GLN.PBS.4h)+nrow(PRO.PBS.6h)+nrow(GLN.PBS.6h)))

linetype <- c(rep('solid',nrow(PRO.2h)+nrow(GLN.2h)+nrow(PRO.4h)+nrow(GLN.4h)+nrow(PRO.6h)+nrow(GLN.6h)),
         rep('dashed',nrow(PRO.PBS.2h)+nrow(GLN.PBS.2h)+nrow(PRO.PBS.4h)+nrow(GLN.PBS.4h)+nrow(PRO.PBS.6h)+nrow(GLN.PBS.6h)))
             
PBS_data <- bind_rows(PRO.2h,GLN.2h,PRO.4h,GLN.4h,PRO.6h,GLN.6h,
                      PRO.PBS.2h,GLN.PBS.2h,PRO.PBS.4h,GLN.PBS.4h,PRO.PBS.6h,GLN.PBS.6h)

PBS_data$time <- time
PBS_data$conditions <- conditions
PBS_data$PBS <- PBS
PBS_data$linetype <- linetype

colour_map = c(inferno(10)[9],inferno(10)[2])

ggplot(PBS_data, aes(x=FSC.H, y=BL1.H, colour=conditions)) + 
  geom_density_2d(aes(linetype=linetype))+#contour_var="ndensity" // "dashed"
  scale_fill_manual(values=colour_map)+
  scale_colour_manual(values=colour_map)+
  #xlim(8000,200000)+  
  scale_y_continuous(labels = scientific)+
  scale_x_continuous(labels = scientific)+
  xlab("Cell size (FSC-H)")+
  ylab("pRPL28 fluorescence (BL1-H)")+
  theme_bw()+
  facet_grid(factor(conditions, levels=c("GLN","PRO"))~factor(time, levels=c('2h', '4h', '6h')))+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("../figures/fig2_SUPP_effect_PBS_wash.svg", width = 6, height = 4, bg='transparent')

### ###
### Effect on different concentrations on heterogeneity profile
### ###

PRO.2h.08 <- read.csv(file = '../source_data/x_fig2x/2h_0_8mM_PRO.csv')
PRO.2h.2 <- read.csv(file = '../source_data/x_fig2x/2h_2mM_PRO.csv')
PRO.2h.5 <- read.csv(file = '../source_data/x_fig2x/2h_5mM_PRO.csv')
PRO.2h.10 <- read.csv(file = '../source_data/x_fig2x/2h_10mM_PRO.csv')

PRO.4h.08 <- read.csv(file = '../source_data/x_fig2x/4h_0_8mM_PRO.csv')
PRO.4h.2 <- read.csv(file = '../source_data/x_fig2x/4h_2mM_PRO.csv')
PRO.4h.5 <- read.csv(file = '../source_data/x_fig2x/4h_5mM_PRO.csv')
PRO.4h.10 <- read.csv(file = '../source_data/x_fig2x/4h_10mM_PRO.csv')

PRO.6h.08 <- read.csv(file = '../source_data/x_fig2x/6h_0_8mM_PRO.csv')
PRO.6h.2 <- read.csv(file = '../source_data/x_fig2x/6h_2mM_PRO.csv')
PRO.6h.5 <- read.csv(file = '../source_data/x_fig2x/6h_5mM_PRO.csv')
PRO.6h.10 <- read.csv(file = '../source_data/x_fig2x/6h_10mM_PRO.csv')

PRO.8h.08 <- read.csv(file = '../source_data/x_fig2x/8h_0_8mM_PRO.csv')
PRO.8h.2 <- read.csv(file = '../source_data/x_fig2x/8h_2mM_PRO.csv')
PRO.8h.5 <- read.csv(file = '../source_data/x_fig2x/8h_5mM_PRO.csv')
PRO.8h.10 <- read.csv(file = '../source_data/x_fig2x/8h_10mM_PRO.csv')


# Add time
time <- c(rep('2h',nrow(PRO.2h.08)+nrow(PRO.2h.2)+nrow(PRO.2h.5)+nrow(PRO.2h.10)),
          rep('4h',nrow(PRO.4h.08)+nrow(PRO.4h.2)+nrow(PRO.4h.5)+nrow(PRO.4h.10)),
          rep('6h',nrow(PRO.6h.08)+nrow(PRO.6h.2)+nrow(PRO.6h.5)+nrow(PRO.6h.10)),
          rep('8h',nrow(PRO.8h.08)+nrow(PRO.8h.2)+nrow(PRO.8h.5)+nrow(PRO.8h.10)))

# Add condition
concentration <- c(rep("0.8 mM Proline",nrow(PRO.2h.08)),
                   rep("2 mM Proline",nrow(PRO.2h.2)),
                   rep("5 mM Proline",nrow(PRO.2h.5)),
                   rep("10 mM Proline",nrow(PRO.2h.10)),
                   rep("0.8 mM Proline",nrow(PRO.4h.08)),
                   rep("2 mM Proline",nrow(PRO.4h.2)),
                   rep("5 mM Proline",nrow(PRO.4h.5)),
                   rep("10 mM Proline",nrow(PRO.4h.10)),
                   rep("0.8 mM Proline",nrow(PRO.6h.08)),
                   rep("2 mM Proline",nrow(PRO.6h.2)),
                   rep("5 mM Proline",nrow(PRO.6h.5)),
                   rep("10 mM Proline",nrow(PRO.6h.10)),
                   rep("0.8 mM Proline",nrow(PRO.8h.08)),
                   rep("2 mM Proline",nrow(PRO.8h.2)),
                   rep("5 mM Proline",nrow(PRO.8h.5)),
                   rep("10 mM Proline",nrow(PRO.8h.10)))
                   
conc_data <- bind_rows(PRO.2h.08,PRO.2h.2,PRO.2h.5,PRO.2h.10,
                       PRO.4h.08,PRO.4h.2,PRO.4h.5,PRO.4h.10,
                       PRO.6h.08,PRO.6h.2,PRO.6h.5,PRO.6h.10,
                       PRO.8h.08,PRO.8h.2,PRO.8h.5,PRO.8h.10)

conc_data$time <- time
conc_data$concentration <- concentration

ggplot(conc_data, aes(x=FSC.H, colour=concentration)) + 
  geom_line(aes(color=concentration), stat="density", size=1, alpha=0.8)+
  scale_colour_viridis(option='viridis',discrete=TRUE)+
  scale_y_continuous(labels = scientific)+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(300, 500000))+
  xlab("Cell size (FSC-H)")+
  ylab("Density (a.u)")+
  annotation_logticks(base = 10,sides = "b")+
  theme_bw()+
  facet_grid(~factor(time, levels=c('2h', '4h', '6h', '8h')))+
  theme(panel.background =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave("../figures/fig2_SUPP_effect_concentration_FSC_H.svg", width = 3.3, height = 2.5, bg='transparent')
