source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 
# load(file="./CODES/data/stan_result_mean5y_re2015_2019.RData")

library(posterior)
posterior_result <- as_draws_df(sampling_result)
mx_cf_draws <- subset_draws(posterior_result, variable = "mx_mat_fut")

###
logmx_list=list()
mx_list=list()

logmx_list[["hm2020"]] <-logmx_list[["hm2021"]]<- logmx_list[["hm2022"]] <- matrix(nrow=47, ncol=22)%>%data.frame()
mx_list[["hm2020"]] <-mx_list[["hm2021"]]<- mx_list[["hm2022"]] <- matrix(nrow=47, ncol=22)%>%data.frame()

for(i in 1:47){
  print(i)
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  # lt_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/bltper_5x1.txt"), skip=2, header=T)
  death_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  etr_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Exposures_5x1.txt"), skip=2, header=T)
  
  
  idx_pref_start = (i-1)*66 +1
  idx_pref_end = i*66
  
  for(year in 2020:2022){
    idx_start = idx_pref_start+(year-2020)*22
    idx_end = idx_start+21
    
    mx_cf_draws[, c(idx_start:idx_end)] %>% 
      data.frame() %>% 
      apply(MARGIN=2, FUN=quantile, prob=.5) -> mx_tmp
    
    death_vec <- (death_pref%>%filter(Year == y))$Total
    etr_vec <- (etr_pref%>%filter(Year == y))$Total
    mx_obs <- pmax(1e-5, c(death_vec[1:21], sum(death_vec[22:24])) / c(etr_vec[1:21], sum(etr_vec[22:24])))

    log(mx_obs) - log(mx_tmp) -> logmx_list[[paste0("hm", year)]][i, ]
    mx_obs - mx_tmp -> mx_list[[paste0("hm", year)]][i, ]
  }
}

colnames(logmx_list[["hm2020"]]) <- colnames(logmx_list[["hm2021"]]) <- colnames(logmx_list[["hm2022"]]) <- paste0("ag", 1:22)
colnames(mx_list[["hm2020"]]) <- colnames(mx_list[["hm2021"]]) <- colnames(mx_list[["hm2022"]]) <- paste0("ag", 1:22)



#####################################
library(stringr)
age_labels <- c(lt_national_for_arriaga[[paste0(2000, ".Total")]]$Age[-22], "100+")


plot_logmx_gap = list()
plot_mx_gap = list()


mx_list[["hm2020"]]%>%unlist()%>%quantile(probs=c(.01, .025, .5, .975, .99))
mx_list[["hm2021"]]%>%unlist()%>%quantile(probs=c(.01, .025, .5, .975, .99))
mx_list[["hm2022"]]%>%unlist()%>%quantile(probs=c(.01, .025, .5, .975, .99))

plot_label = c("(A)", "(B)", "(C)")


for(year in 2020:2022){
  # year=2020
  logmx_list[[paste0("hm", year)]]%>%
    mutate(prefecture = factor(
      str_to_title(pref_list),
      levels = str_to_title(pref_list)
    )) %>%
    tidyr::pivot_longer(
      cols = starts_with("ag"), 
      names_to = "age_group", 
      values_to = "diff_log_mx"
    )%>%
    mutate(age_group = case_when(
      age_group == "ag1"  ~ age_labels[1],
      age_group == "ag2"  ~ age_labels[2],
      age_group == "ag3"  ~ age_labels[3],
      age_group == "ag4"  ~ age_labels[4],
      age_group == "ag5"  ~ age_labels[5],
      age_group == "ag6"  ~ age_labels[6],
      age_group == "ag7"  ~ age_labels[7],
      age_group == "ag8"  ~ age_labels[8],
      age_group == "ag9"  ~ age_labels[9],
      age_group == "ag10" ~ age_labels[10],
      age_group == "ag11" ~ age_labels[11],
      age_group == "ag12" ~ age_labels[12],
      age_group == "ag13" ~ age_labels[13],
      age_group == "ag14" ~ age_labels[14],
      age_group == "ag15" ~ age_labels[15],
      age_group == "ag16" ~ age_labels[16],
      age_group == "ag17" ~ age_labels[17],
      age_group == "ag18" ~ age_labels[18],
      age_group == "ag19" ~ age_labels[19],
      age_group == "ag20" ~ age_labels[20],
      age_group == "ag21" ~ age_labels[21],
      age_group == "ag22" ~ age_labels[22]
    ))%>%
    mutate(age_group = factor(age_group, levels = age_labels)) -> logmx_df_long
  
  mx_list[[paste0("hm", year)]]%>%
    mutate(prefecture = factor(
      str_to_title(pref_list),
      levels = str_to_title(pref_list)
    )) %>%
    tidyr::pivot_longer(
      cols = starts_with("ag"), 
      names_to = "age_group", 
      values_to = "diff_mx"
    )%>%
    mutate(age_group = case_when(
      age_group == "ag1"  ~ age_labels[1],
      age_group == "ag2"  ~ age_labels[2],
      age_group == "ag3"  ~ age_labels[3],
      age_group == "ag4"  ~ age_labels[4],
      age_group == "ag5"  ~ age_labels[5],
      age_group == "ag6"  ~ age_labels[6],
      age_group == "ag7"  ~ age_labels[7],
      age_group == "ag8"  ~ age_labels[8],
      age_group == "ag9"  ~ age_labels[9],
      age_group == "ag10" ~ age_labels[10],
      age_group == "ag11" ~ age_labels[11],
      age_group == "ag12" ~ age_labels[12],
      age_group == "ag13" ~ age_labels[13],
      age_group == "ag14" ~ age_labels[14],
      age_group == "ag15" ~ age_labels[15],
      age_group == "ag16" ~ age_labels[16],
      age_group == "ag17" ~ age_labels[17],
      age_group == "ag18" ~ age_labels[18],
      age_group == "ag19" ~ age_labels[19],
      age_group == "ag20" ~ age_labels[20],
      age_group == "ag21" ~ age_labels[21],
      age_group == "ag22" ~ age_labels[22]
    ))%>%
    mutate(age_group = factor(age_group, levels = age_labels)) ->mx_df_long
  
  
  
  
  ###
  ### plot
  ###
  
  library(RColorBrewer)
  
  logmx_df_long %>%
    ggplot(aes(x = age_group, y = prefecture, fill=diff_log_mx))+
    geom_tile()+
    scale_fill_gradientn(
      colors = rev(brewer.pal(11, "RdBu")),
      limits = c(-0.5, 0.5),
      oob = scales::squish,
      name = "Gap"
    ) +
    labs(x = "Age Group (year)", y = "Prefecture", 
         title = paste0(plot_label[year-2019], " Gap in Log-scaled Mortality Rates, ", year), 
         subtitle = "Observed vs Projected", 
         fill = paste0("Gap"))+
    scale_x_discrete(
      breaks = unique(logmx_df_long$age_group), 
      labels = age_labels  
    )+
    scale_y_discrete(
      breaks = unique(logmx_df_long$prefecture),
      limits = rev(unique(logmx_df_long$prefecture)) 
    ) +
    theme_bw()+
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.text=element_text(size = 16), 
          axis.title=element_text(size = 16), 
          axis.title.x=element_text(vjust=-5),
          axis.ticks.x=element_line(color="black"), 
          axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(size = 20), 
          plot.subtitle = element_text(size = 14), 
          plot.margin = unit(c(1,1,2,1), "lines")
    )->plot_logmx_gap[[paste0(year)]]
 
}

library(patchwork)

tiff("./CODES/RESULTS/Fig3_diff_log_mx_pref.tiff", width = 32, height = 12, units = "in", res = 500, compression = "lzw")
plot_logmx_gap[["2020"]]|plot_logmx_gap[["2021"]]|plot_logmx_gap[["2022"]]
dev.off()
