source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 
# load(file="./CODES/data/stan_result_mean5y_re2015_2019.RData")

library(posterior)
posterior_result <- as_draws_df(sampling_result)
mu_dx_draws <- subset_draws(posterior_result, variable = "mu_dx_mat")

age_labels <- c(lt_national_for_arriaga[[paste0(y, ".Total")]]$Age[-22], "100+")

ppc_death_pref_list = list()

pref_name <- stringr::str_to_title(pref_list)

set.seed(9999) 

for(i in 1:47){
  print(i)
  
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  death_jmd_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  
  idx_pref_start = (i-1) * (22*20) +1
  idx_pref_end = i * (22*20)
  
  death_pref = matrix(nrow = 22*20, ncol = 8) %>% data.frame()
  colnames(death_pref) = c("pref_idx", "pref_name", "year", "age_group", "95PrI_lwr", "95PrI_med", "95PrI_upr", "obs")
  death_pref$pref_idx <- i
  death_pref$pref_name <- pref_name[i]
  
  for(y in 2000:2019){
    # y=2000
    idx_start = idx_pref_start+(y-2000)*22
    idx_end = idx_start+21
    
    death_matrix = matrix(nrow = 4000, ncol = 22)
    for(n in 1:4000){
      death_matrix[n, ] <- rpois(22, mu_dx_draws[n, c(idx_start:idx_end)] %>% unlist())
    }
    death_ci <- death_matrix %>% apply(quantile, MARGIN = 2, probs = c(.025, .5, .975))
    
    death_pref[1:22+(y-2000)*22, 5:7] <- death_ci %>% round() %>% t()
    death_pref[1:22+(y-2000)*22, 4]   <- age_labels
    death_pref[1:22+(y-2000)*22, 3]   <- y
    
    death_jmd_vec <- (death_jmd_pref %>% filter(Year == y))$Total
    
    death_pref[1:22+(y-2000)*22, 8] <- c(death_jmd_vec[1:21], sum(death_jmd_vec[22:24])) %>% round()
    if(i == 3 & y == 2011){
      death_pref[1:22+(y-2000)*22, 8] <- death_pref[1:22+(y-2000)*22, 8]  - death_eq_3[2:21]
    }
    if(i == 4 & y == 2011){
      death_pref[1:22+(y-2000)*22, 8] <- death_pref[1:22+(y-2000)*22, 8]  - death_eq_4[2:21]
    }
    if(i == 7 & y == 2011){
      death_pref[1:22+(y-2000)*22, 8] <- death_pref[1:22+(y-2000)*22, 8]  - death_eq_7[2:21]
    }
    
  }
  
  death_pref %<>% mutate(in95 = (obs >= `95PrI_lwr`) & (obs <= `95PrI_upr`))
  
  
  ppc_death_pref_list[[paste0("pref", i)]] <- death_pref
}


ppc_df_pref <- bind_rows(ppc_death_pref_list)

ppc_df_pref %>% 
  readr::write_csv(paste0("./CODES/RESULTS/", "ppc_death.csv"))
