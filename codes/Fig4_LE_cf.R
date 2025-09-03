source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 
# load(file="./CODES/data/stan_result_mean5y_re2015_2019.RData")

library(posterior)
posterior_result <- as_draws_df(sampling_result)
mx_cf_draws <- subset_draws(posterior_result, variable = "mx_mat_fut")
dx_fut_pred_draws <- subset_draws(posterior_result, variable = "dx_fut_predicted")

age_labels <- c(lt_national_for_arriaga[[paste0(2000, ".Total")]]$Age[-22], "100+")

##########################
le_cf_pref = matrix(nrow=47, ncol = 14) %>% data.frame()
ci_names = c("lwr", "med", "upr")
colnames(le_cf_pref) <- c("pref", "pref_idx", paste0("y2020_", ci_names), paste0("y2021_", ci_names), paste0("y2022_", ci_names), 
                          "y2020_obs", "y2021_obs", "y2022_obs")
le_cf_pref$pref <- pref.info$pref_list 
le_cf_pref$pref_idx <- pref.info$pref_code


for(i in 1:47){
  # i=1
  print(i)
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  lt_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/bltper_5x1.txt"), skip=2, header=T)
  
  idx_pref_start = (i-1)*66 +1
  idx_pref_end = i*66
  
  for(y in 2020:2022){
    # y=2020
    lt_y <- lt_pref %>% filter(Year == y)
    etr_fut_tmp <- etr_df_fut[, 3*(i-1)+(y-2019)]
    
    idx_start = idx_pref_start+(y-2020)*22
    idx_end = idx_start+21
    
    le_tmp = c()
    for(n in 1:4000){
      # n=1
      dx_fut_pred_tmp <- dx_fut_pred_draws[n, c(idx_start:idx_end)] %>% unlist()
      
      le_cf <- (lt_calc(lt_y, dx_fut_pred_tmp, etr_fut_tmp))$ex[1]
      
      le_tmp = append(le_tmp, le_cf)
    }
    
    le_ci <- quantile(le_tmp, c(.025, .5, .975))
    
    le_cf_pref[i, 2+(y-2020)*3+1:3] <- le_ci
  }
}


for(i in 1:47){
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  lt_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/bltper_5x1.txt"), skip=2, header=T)
  death_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  etr_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Exposures_5x1.txt"), skip=2, header=T)
  
  for(y in 2020:2022){
    lt_y <- lt_pref %>% filter(Year == y)
    death_vec <- (death_pref%>%filter(Year == y))$Total
    etr_vec <- (etr_pref%>%filter(Year == y))$Total
    
    le_cf_pref[[paste0("y", y, "_obs")]][i] <- (lt_calc(lt_y, death_vec, etr_vec))$ex[1]
    
  }
  
}

le_cf_pref%<>%
  mutate(gap_2022_med = y2022_obs - y2022_med, 
         gap_2022_upr = y2022_obs - y2022_lwr, 
         gap_2022_lwr = y2022_obs - y2022_upr
  )

le_cf_pref %>% readr::write_csv(file="CODES/RESULTS/le_cf_pref.csv")

readr::read_csv(file="CODES/RESULTS/le_cf_pref.csv") -> le_df_pref



library(ggplot2)

palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

plot_raw = list()
plot_diff = list()

plot_label = c("(A)", "(B)", "(C)")

library(stringr)
pref_name <- le_df_pref$pref%>%str_to_title()

for(year in 2020:2022){
  
  plot_df_tmp <-matrix(NA, nrow=47, ncol=6)%>%data.frame()
  colnames(plot_df_tmp)<-c("pref", "pref_idx", "lwr", "med", "upr", "obs")
  plot_df_tmp[,1:2]<-le_df_pref[, 1:2]
  plot_df_tmp[,1] <- str_to_title(plot_df_tmp[,1])
  
  
  plot_df_tmp[["lwr"]] <-le_df_pref[[paste0("y", year, "_lwr")]]
  plot_df_tmp[["med"]] <-le_df_pref[[paste0("y", year, "_med")]]
  plot_df_tmp[["upr"]] <-le_df_pref[[paste0("y", year, "_upr")]]
  plot_df_tmp[["obs"]] <-le_df_pref[[paste0("y", year, "_obs")]]
  
  
  plot_df_tmp%>%
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
      breaks = 82+.5*0:10,
      limits = c(82, 87),  
      expand = c(0, 0.5)  
    )+
    labs(
      title = paste0(plot_label[year-2019], " Gap in life Expectancy, ", year),
      subtitle = "Projected vs Observed",
      x = "Gap (year)",
      y = "Prefecture"
    ) -> plot_raw_tmp
  

  
  plot_df_tmp%>%
    ggplot(aes(x = obs - med, y = pref_idx)) +
    geom_errorbar(
      aes(xmin = obs - upr, xmax = obs - lwr),
      width = 0.5, 
      color = palette[2]
    ) +
    geom_point(
      size = 2, 
      color = palette[2]
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
    scale_x_continuous(
      breaks = 0+.5*-6:6,
      limits = c(-3, 3),  
      expand = c(0, 0.5)  
    )+
    labs(
      title = paste0(plot_label[year-2019], " Gap in Life Expectancy, ", year),
      subtitle = "Obsereved and Projected",
      x = "Gap (year)",
      y = "Prefecture"
    )+
    geom_vline(aes(xintercept=0), linetype = "dashed", linewidth = 0.25)-> plot_diff_tmp
  
  plot_raw_tmp -> plot_raw[[paste0("raw_",year)]]
  plot_diff_tmp -> plot_diff[[paste0("diff_",year)]]
}

plot_raw[["raw_2020"]]
plot_raw[["raw_2021"]]
plot_raw[["raw_2022"]]

plot_diff[["diff_2020"]]
plot_diff[["diff_2021"]]
plot_diff[["diff_2022"]]

library(patchwork)
plot_diff[["diff_2020"]]|plot_diff[["diff_2021"]]|plot_diff[["diff_2022"]]

tiff("./CODES/RESULTS/Fig4_LE_diff.tiff", width = 18, height = 9, units = "in", res = 1000, compression = "lzw")
plot_diff[["diff_2020"]]|plot_diff[["diff_2021"]]|plot_diff[["diff_2022"]]
dev.off()

