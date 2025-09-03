source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 

library(posterior)
posterior_result <- as_draws_df(sampling_result)
mu_dx_fut_pred_draws <- subset_draws(posterior_result, variable = "mu_dx_mat_fut")

plot_dx_national_df = matrix(NA, nrow = 22, ncol = 2+3*3+3) %>% data.frame()
ci_names = c("lwr", "med", "upr")
colnames(plot_dx_national_df) <-  c("age_group", "age_idx", 
                                    paste0("y2020_", ci_names), 
                                    paste0("y2021_", ci_names), 
                                    paste0("y2022_", ci_names), 
                                    "y2020_obs", "y2021_obs", "y2022_obs")

plot_dx_national_df[,1] <- c(lt_national_for_arriaga[[paste0(y, ".Total")]]$Age[-22], "100+")
plot_dx_national_df[,2] <- 1:22

dx_2020 = matrix(NA, nrow=22, ncol=4000) %>% data.frame()
dx_2021 = matrix(NA, nrow=22, ncol=4000) %>% data.frame()
dx_2022 = matrix(NA, nrow=22, ncol=4000) %>% data.frame()

set.seed(1)

for(n in 1:4000){
  
  matrix(mu_dx_fut_pred_draws[n,1:3102], 
          nrow=47*3,
          ncol=22, byrow=T)%>%data.frame()-> dx_mat_tmp
  
  dx_mat_tmp[1+3*(0:46),]%>% 
    sapply(as.numeric) %>% 
    colSums()%>%as.vector()-> dx_2020[, n]
  dx_mat_tmp[2+3*(0:46),]%>% 
    sapply(as.numeric) %>% 
    colSums()%>%as.vector()-> dx_2021[, n]
  dx_mat_tmp[3+3*(0:46),]%>% 
    sapply(as.numeric) %>% 
    colSums()%>%as.vector()-> dx_2022[, n]

  rpois(22, lambda = dx_2020[,n]) -> dx_2020[,n]
  rpois(22, lambda = dx_2021[,n]) -> dx_2021[,n]
  rpois(22, lambda = dx_2022[,n]) -> dx_2022[,n]
  
}

for(a in 1:22){
    # a=1
    dx_2020_ci_tmp <- dx_2020[a,] %>% unlist() %>% quantile(prob=c(.025, .5, .975))
    dx_2021_ci_tmp <- dx_2021[a,] %>% unlist() %>% quantile(prob=c(.025, .5, .975))
    dx_2022_ci_tmp <- dx_2022[a,] %>% unlist() %>% quantile(prob=c(.025, .5, .975))
    
    plot_dx_national_df[a, 3:5] <- dx_2020_ci_tmp
    plot_dx_national_df[a, 6:8] <- dx_2021_ci_tmp
    plot_dx_national_df[a, 9:11] <- dx_2022_ci_tmp
}

etr_df_fut[, 1+3*(0:46)] %>% rowSums() %>% as.vector() -> etr_2020
etr_df_fut[, 2+3*(0:46)] %>% rowSums() %>% as.vector() -> etr_2021
etr_df_fut[, 3+3*(0:46)] %>% rowSums() %>% as.vector() -> etr_2022

plot_log_mx_national_df <- plot_dx_national_df

log(plot_log_mx_national_df[["y2020_lwr"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2020_lwr"]]
log(plot_log_mx_national_df[["y2020_med"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2020_med"]]
log(plot_log_mx_national_df[["y2020_upr"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2020_upr"]]

log(plot_log_mx_national_df[["y2021_lwr"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2021_lwr"]]
log(plot_log_mx_national_df[["y2021_med"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2021_med"]]
log(plot_log_mx_national_df[["y2021_upr"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2021_upr"]]

log(plot_log_mx_national_df[["y2022_lwr"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2022_lwr"]]
log(plot_log_mx_national_df[["y2022_med"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2022_med"]]
log(plot_log_mx_national_df[["y2022_upr"]]) - log(etr_2020) -> plot_log_mx_national_df[["y2022_upr"]]


plot_log_mx_national_df[["y2020_obs"]] <- log(lt_national_for_arriaga[["2020.Total"]][["mx"]]) %>% unlist()
plot_log_mx_national_df[["y2021_obs"]] <- log(lt_national_for_arriaga[["2021.Total"]][["mx"]]) %>% unlist()
plot_log_mx_national_df[["y2022_obs"]] <- log(lt_national_for_arriaga[["2022.Total"]][["mx"]]) %>% unlist()

##################################################################################

###
### PLOT
###

log_mx_2019 <- log(lt_national_for_arriaga[["2019.Total"]][["mx"]]) %>% unlist()

library(ggplot2)

palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

age_group <- plot_log_mx_national_df$age_group


plot_raw = list()
plot_diff = list()

plot_label = c("(A)", "(B)", "(C)")

for(year in 2020:2022){
  # year = 2020
  
  plot_df_tmp <-matrix(NA, nrow=22, ncol=6)%>%data.frame()
  colnames(plot_df_tmp)<-c("age_group", "age_idx", "lwr", "med", "upr", "obs")
  plot_df_tmp[,1:2]<-plot_log_mx_national_df[, 1:2]
  
  plot_df_tmp[["lwr"]] <-plot_log_mx_national_df[[paste0("y", year, "_lwr")]]
  plot_df_tmp[["med"]] <-plot_log_mx_national_df[[paste0("y", year, "_med")]]
  plot_df_tmp[["upr"]] <-plot_log_mx_national_df[[paste0("y", year, "_upr")]]
  plot_df_tmp[["obs"]] <-plot_log_mx_national_df[[paste0("y", year, "_obs")]]
  
  plot_df_tmp%>%
    ggplot(aes(x = age_idx, y = med)) +
    geom_errorbar(
      aes(ymin = lwr, ymax = upr),
      width = 0.2, 
      color = palette[2]
    ) +
    geom_point(
      size = 2, 
      color = palette[2]
    ) +
    geom_point(
      aes(x= age_idx, y = obs),
      shape = 4,
      size = 2
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )+
    scale_x_continuous(
      breaks = 1:22,
      labels = age_group,
      limits = c(0, 23),  
      expand = c(0, 0.5)  
    ) +
    labs(
      title = paste0(plot_label[year-2019], " Mortality Rate by Age Group, ", year),
      subtitle = "Projected vs Observed",
      x = "Age Group (year)",
      y = "log (Mortality Rate)"
    ) -> plot_raw_tmp
  
  plot_df_tmp%>%
    ggplot(aes(x = age_idx, y = obs-med)) +
    geom_errorbar(
      aes(ymin = obs-upr, ymax = obs-lwr, color = "vs Projected"),
      width = 0.3
    ) +
    geom_point(
      aes(color = "vs Projected"),
      size = 2
    ) +

    scale_color_manual(
      values = c("vs Projected" = palette[2], 
                 "vs 2019" = palette[4]),
      name = NULL  
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.position = "none"
    )+
    scale_x_continuous(
      breaks = 1:22,
      labels = age_group,
      limits = c(0.5, 22.5),  
      expand = c(0, 0.5)  
    ) +
    scale_y_continuous(
      limits = c(-0.5, 0.5),  
      breaks = -5:5*.1
    ) +
    labs(
      title = paste0(plot_label[year-2019], " Mortality Rate by Age Group, ", year),
      subtitle = "log(Observed) - log(Projected)",
      x = "Age Group (year)",
      y = "Gap"
    )+
    geom_hline(aes(yintercept=0), linetype = "dashed", linewidth = 0.25) -> plot_diff_tmp
  
  plot_raw_tmp -> plot_raw[[paste0("raw_",year)]]
  plot_diff_tmp -> plot_diff[[paste0("diff_",year)]]
}


library(patchwork)
plot_diff[["diff_2020"]]/plot_diff[["diff_2021"]]/plot_diff[["diff_2022"]]

tiff("./CODES/RESULTS/Fig2_diff_log_mx.tiff", width = 9, height = 15, units = "in", res = 1000, compression = "lzw")
plot_diff[["diff_2020"]]/plot_diff[["diff_2021"]]/plot_diff[["diff_2022"]]
dev.off()
