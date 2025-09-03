source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 
# load(file="./CODES/data/stan_result_mean5y_re2015_2019.RData")

library(posterior)
posterior_result <- as_draws_df(sampling_result)
mu_dx_fut_draws <- subset_draws(posterior_result, variable = "mu_dx_mat_fut")
#
dx_fut_pref = matrix(nrow=47, ncol = 14)%>%data.frame()
ci_names = c("lwr", "med", "upr")
colnames(dx_fut_pref) <- c("pref", "pref_idx", 
                           paste0("y2020_", ci_names), 
                           paste0("y2021_", ci_names), 
                           paste0("y2022_", ci_names), 
                           "y2020_obs", "y2021_obs", "y2022_obs")
dx_fut_pref$pref <- pref.info$pref_list 
dx_fut_pref$pref_idx <- pref.info$pref_code

set.seed(1) 

for(i in 1:47){
  print(paste0("pref = ", i))
  pref=pref_list[i]
  
  idx_pref_start = (i-1)*66 +1
  idx_pref_end = i*66
  
  for(y in 2020:2022){
    idx_start = idx_pref_start+(y-2020)*22
    idx_end = idx_start+21
    
    dx_fut_vec=c()
     for(n in 1:4000){
       dx_fut_tmp <- rpois(1, sum(mu_dx_fut_draws[n, c(idx_start:idx_end)]))
      
       dx_fut_vec = append(dx_fut_vec, dx_fut_tmp)
     }
    
    dx_fut_ci <- quantile(dx_fut_vec, c(.025, .5, .975))
    
    dx_fut_pref[i, 2+(y-2020)*3+1:3] <- dx_fut_ci
  }
}

for(i in 1:47){
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  death_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  
 for(y in 2020:2022){
    dx_fut_pref[[paste0("y", y, "_obs")]][i] <- (death_pref%>%filter(Year == y))$Total %>% sum()
  }
  
}


dx_fut_pref%>%readr::write_csv(file="CODES/RESULTS/dx_fut_pref.csv")

dx_fut_pref<-readr::read_csv(file="CODES/RESULTS/dx_fut_pref.csv")


library(ggplot2)

palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

plot_raw = list()
plot_diff = list()

library(stringr)
pref_name <-dx_fut_pref$pref%>%str_to_title()


dashboard_df <- readr::read_csv(file = "./data_exdeath/dashboard_summarized.csv")

plot_label = c("(A)", "(B)", "(C)")

plot_df_list = list()

for(year in 2020:2022){
  plot_df_tmp <- matrix(NA, nrow=47, ncol=10)%>%data.frame()
  colnames(plot_df_tmp) <- c("pref", "pref_idx", "lwr", "med", "upr", "obs", "db_lwr", "db_med", "db_upr", "db_obs")
  plot_df_tmp[,1:2] <- dx_fut_pref[, 1:2]
  plot_df_tmp[,1] <- str_to_title(plot_df_tmp[,1])
  
  plot_df_tmp[["lwr"]] <- dx_fut_pref[[paste0("y", year, "_lwr")]]
  plot_df_tmp[["med"]] <- dx_fut_pref[[paste0("y", year, "_med")]]
  plot_df_tmp[["upr"]] <- dx_fut_pref[[paste0("y", year, "_upr")]]
  plot_df_tmp[["obs"]] <- dx_fut_pref[[paste0("y", year, "_obs")]]
  
  plot_df_tmp[["db_lwr"]] <- dashboard_df[[paste0("y", year, "_lwr")]]
  plot_df_tmp[["db_med"]] <- dashboard_df[[paste0("y", year, "_med")]]
  plot_df_tmp[["db_upr"]] <- dashboard_df[[paste0("y", year, "_upr")]]
  plot_df_tmp[["db_obs"]] <- dashboard_df[[paste0("y", year, "_obs")]]
  
  
  etr_pref_tmp <- etr_df_fut[, year-2019+3*0:46]%>%colSums()
  
  for(col_idx in 3:10){
    plot_df_tmp[, col_idx] <- c(plot_df_tmp[, col_idx]) /etr_pref_tmp*1e5
  }
  
  plot_df_tmp_2 <- plot_df_tmp
  plot_df_list[[paste0(year)]] <- plot_df_tmp_2
  
  
  plot_df_tmp_2%>%
    ggplot(aes(x = med, y = pref_idx)) +
    geom_errorbar(
      aes(xmin = lwr, xmax = upr),
      width = 0.5, 
      color = palette[2]
    ) +
    geom_point(
      size = 2, 
      color = palette[2]
    ) +
    geom_point(
      aes(x= obs, y = pref_idx),
      shape = 4,
      size = 2,
    ) +
    # from dashboard
    geom_errorbar(
      aes(y = pref_idx, xmin = db_lwr, xmax = db_upr),
      width = 0.5, 
      color = palette[7]
    ) +
    geom_point(
      aes(x = db_med, y = pref_idx -.2),
      size = 2, 
      color = palette[7]
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )+
    scale_y_reverse( 
      breaks = 1:47,
      labels = pref_name,
      limits = c(47.5, 0.5), 
      expand = c(0, 0.5)
    ) +
    labs(
      title = paste0(plot_label[year-2019], " Total Death Count by Prefecture, ", year),
      subtitle = "Projected vs Observed",
      x = "Death Count",
      y = "Prefecture"
    ) -> plot_raw_tmp
  
  
  
  plot_df_tmp%>%
    ggplot(aes(x = obs - med, y = pref_idx)) +
    geom_errorbar(
      aes(xmin = obs - upr, xmax = obs - lwr, color = "This Study"),
      width = 0.5
    ) +
    geom_point(
      aes(color = "This Study"), 
      size = 2
    ) +
    geom_point(
      aes(x = db_obs - db_med, y = pref_idx, color = "Dashboard"),
      shape = 5,
      size = 2
    ) +
    scale_color_manual(
      values = c("This Study" = palette[2], 
                 "Dashboard" = palette[4]),
      name = NULL  
    )+
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )+
    scale_y_reverse( 
      breaks = 1:47,
      labels = pref_name,
      limits = c(47.5, 0.5), 
      expand = c(0, 0.5)
    ) +
    scale_x_continuous(
      breaks = -5:5*.05*1e3,
      limits = c(-0.25*1e3, .25*1e3),
    )+
    labs(
      title = paste0(plot_label[year-2019], " Mortality Gap vs Projected, ", year),
      x = "Mortality Gap per 100k",
      y = "Prefecture"
    )+
    geom_vline(aes(xintercept=0), linetype = "dashed", linewidth = 0.25)-> plot_diff_tmp
  
  plot_raw_tmp -> plot_raw[[paste0("raw_",year)]]
  plot_diff_tmp -> plot_diff[[paste0("diff_",year)]]
}

plot_df_list[["2020"]]%<>%
  mutate(gap_med = obs - med, 
         gap_lwr = obs - upr, 
         gap_upr = obs - lwr)
plot_df_list[["2021"]]%<>%
  mutate(gap_med = obs - med, 
         gap_lwr = obs - upr, 
         gap_upr = obs - lwr)
plot_df_list[["2022"]]%<>%
  mutate(gap_med = obs - med, 
         gap_lwr = obs - upr, 
         gap_upr = obs - lwr)

plot_df_list[["2022"]]

library(patchwork)
plot_diff[["diff_2020"]]|plot_diff[["diff_2021"]]|plot_diff[["diff_2022"]]

tiff("./CODES/RESULTS/Fig5_Death_pct_diff.tiff", width = 18, height = 9, units = "in", res = 1000, compression = "lzw")
plot_diff[["diff_2020"]]|plot_diff[["diff_2021"]]|plot_diff[["diff_2022"]]
dev.off()
