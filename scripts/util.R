
require(flexmix)
require(ggplot2)

flexmix_MV_clustering <-function(data,p_tresh=0.95,dimensions=c("FSC.H","BL1.H"),k=2,cluster=NULL){
  # Perform multivariate clustering
  # data : dataframe with flow data
  # p_tresh : value for the treshold to assign cells to cluster (default : 0.95)
  # dimensions : dimensions to perform clustering on (default : c("FSC.H","BL1.H"))
  
  
  mo1 <- FLXMCmvnorm()
  mo2 <- FLXMCmvnorm()
  
  data_mv_fit <- data %>% dplyr::select(all_of(dimensions))
  
  set.seed(123)
  # Fitting data
  
  #flexfit return an error if prior is NULL
  if (is.null(cluster)) {
    flexfit <- flexmix(as.matrix(data_mv_fit) ~ 1, data = data_mv_fit, k = k, model = list(mo1,mo2))
  } else{
    flexfit <- flexmix(as.matrix(data_mv_fit) ~ 1, data = data_mv_fit, k = k, model = list(mo1,mo2), cluster=cluster)
    print("test")
  }
  
  # Posterior probabilities
  proba <- posterior(flexfit)
  
  clustered_data <- cbind(data,proba)
  
  # Retrieve fit parameters
  c1 <- parameters(flexfit, component=1)[[1]]
  c2 <- parameters(flexfit, component=2)[[1]]
  
  cluster_means <- c(parameters(flexfit, component=1)[[1]][1],parameters(flexfit, component=2)[[1]][1])
  
  # Rename proba column based on which cluster is high or low
  if (which.min(cluster_means)==1) {
    clustered_data <- dplyr::rename(clustered_data, "proba_low" = "1")
    clustered_data <- dplyr::rename(clustered_data, "proba_high" = "2")
  } else {
    clustered_data <- dplyr::rename(clustered_data, "proba_low" = "2")
    clustered_data <- dplyr::rename(clustered_data, "proba_high" = "1")
  }
  
  clustered_data <- clustered_data %>% mutate(
    classification = if_else(proba_low>p_tresh,"Low",if_else(proba_high>p_tresh,"High","Unknown"))
  )
  
  print(table(clustered_data$classification))
  clustered_data
}

flexmix_1D_clustering <- function(data,p_tresh=0.95,dimensions=c("FSC.H"),k=2,cluster=NULL){
  
  
  data_1D_fit <- data %>% dplyr::select(all_of(dimensions))
  
  # Fitting two gaussians to our data (depending on dip.test pvalue)
  mo1 <- FLXMRglm(family = "gaussian")
  mo2 <- FLXMRglm(family = "gaussian")
  
  # Fitting data
  flexfit <- flexmix(as.matrix(data_1D_fit) ~ 1, data = data, k = 2, model = list(mo1,mo2))
  
  # Posterior probabilities
  proba <- posterior(flexfit)
  
  clustered_data <- cbind(data,proba)
  
  # Retrieve fit parameters
  c1 <- parameters(flexfit, component=1)[[1]] #Mean
  c2 <- parameters(flexfit, component=2)[[1]] #Mean
  
  cluster_means <- c(parameters(flexfit, component=1)[[1]][1],parameters(flexfit, component=2)[[1]][1])
  
  # Rename proba column based on which cluster is high or low
  if (which.min(cluster_means)==1) {
    clustered_data <- dplyr::rename(clustered_data, "proba_low" = "1")
    clustered_data <- dplyr::rename(clustered_data, "proba_high" = "2")
  } else {
    clustered_data <- dplyr::rename(clustered_data, "proba_low" = "2")
    clustered_data <- dplyr::rename(clustered_data, "proba_high" = "1")
  }
  
  clustered_data <- clustered_data %>% mutate(
    classification = if_else(proba_low>p_tresh,"Low",if_else(proba_high>p_tresh,"High","Unknown"))
  )
  
  print(table(clustered_data$classification))
  
  clustered_data
}

plot_clustered_contour <- function(clustered_data,dimensions,classification){
  clustered_data_plot <- clustered_data %>% filter(classification == "High")
  
  p <- ggplot(clustered_data_plot, aes(x=FSC.H, y=BL1.H)) + 
    geom_density2d(aes(colour=classification), alpha=0.6)+
    #scale_colour_viridis(option='B')+
    scale_fill_manual(values=colour_map)+
    scale_colour_manual(values=colour_map)+
    xlim(8000,200000)+  
    scale_y_continuous(labels = scientific)+
    xlab("Cell size (FSC-H)")+
    ylab("pRPL28 fluorescence (BL1-H)")+
    theme_bw()+
    theme(panel.background =  element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

subpopulation_TF_score <- function(data){
  # Computes TF score per subpopulation for a given
  # condition/timepoint df (data)
  
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
  
  TF.score$low_high = TF.score$low / TF.score$high
  TF.score$none_low = TF.score$none/ TF.score$low
  TF.score$none_high = TF.score$none / TF.score$high
  
  list(TF.info,TF.score)
  
}

condition_TF_score <- function(data){
  # Computes TF score per subpopulation for a given
  # condition/timepoint df (data)
  
  TF.info <- data %>% group_by(field.of.view) %>% 
    dplyr::summarize(
      TF_score_mean = mean(nuc_score,na.rm=TRUE),
      TF_score_sd = sd(nuc_score,na.rm=TRUE),
      TF_score_number = n(),
      TF_score_se = TF_score_sd/sqrt(TF_score_number)
    ) %>% inner_join(FOV_TF_mapping) 
  
  TF.info
  
}

condition_TF_score_v2 <- function(data){
  # Computes TF score per subpopulation for a given
  # condition/timepoint df (data)
  # Now keep subpopulation classifying
  
  TF.info <- data %>% group_by(field.of.view) %>% 
    dplyr::summarize(
      TF_score_mean = mean(nuc_score,na.rm=TRUE),
      TF_score_sd = sd(nuc_score,na.rm=TRUE),
      TF_score_number = n(),
      TF_score_se = TF_score_sd/sqrt(TF_score_number)
    ) %>% inner_join(FOV_TF_mapping) 
  
  TF.info
  
}