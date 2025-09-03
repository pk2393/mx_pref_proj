source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 

library(posterior)
posterior_result <- as_draws_df(sampling_result)
mu_draws <- subset_draws(posterior_result, variable = "mu_dx_mat_fut")

age_labels <- c(lt_national_for_arriaga[[paste0(y, ".Total")]]$Age[-22], "100+")


set.seed(9999) 

mu_draws_national_fut = matrix(0, nrow = 4000, ncol = 22*3)
death_jmd_national = matrix(0, nrow = 22, ncol = 3)

for(i in 1:47){
  print(i)
  
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  death_jmd_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  
  idx_pref_start = (i-1) * (22*3) +1
  idx_pref_end = i * (22*3)
  
  mu_draws_national_fut <- mu_draws_national_fut + mu_draws[, idx_pref_start:idx_pref_end]

  for(y in 2020:2022){
    
    death_jmd_vec <- (death_jmd_pref %>% filter(Year == y))$Total
    
    death_jmd_national[, y-2019] <- death_jmd_national[, y-2019] + c(death_jmd_vec[1:21], sum(death_jmd_vec[22:24])) %>% round() 
  }
  
}


mu_draws_national_fut %>%
  apply(quantile, MARGIN = 2, probs = c(.025, .5, .975)) -> mu_draws_quantile

mu_draws_quantile %>% 
  t() %>%
  data.frame() %>%
  cbind((death_jmd_national %>% c())) -> death_fut_national

colnames(death_fut_national) <- c("95CrI_lwr", "95CrI_med", "95CrI_upr", "obs")

death_fut_national %<>%
  mutate(year = rep(2020:2022, each = 22), .before = '95CrI_lwr') %>%
  mutate(age = rep(age_labels, 3), .before = '95CrI_lwr')

death_fut_national %<>%
  mutate(
    excess_exiguous = case_when(
      obs > `95CrI_upr` ~ obs - `95CrI_upr`,
      obs < `95CrI_lwr` ~ obs - `95CrI_lwr`,
      TRUE              ~ 0)
    )

rownames(death_fut_national) <- NULL

death_fut_national %>% readr::write_csv(paste0("./CODES/RESULTS/", "death_gap_covid_age.csv"))

mu_draws_national_fut[, 1:22] %>%
  apply(sum, MARGIN = 1) %>% quantile(probs = c(.5, .025, .975))
(death_jmd_national %>% c())[1:22] %>% sum()

mu_draws_national_fut[, 22+1:22] %>%
  apply(sum, MARGIN = 1) %>% quantile(probs = c(.5, .025, .975))
(death_jmd_national %>% c())[22+1:22] %>% sum()

mu_draws_national_fut[, 44+1:22] %>%
  apply(sum, MARGIN = 1) %>% quantile(probs = c(.5, .025, .975))
(death_jmd_national %>% c())[44+1:22] %>% sum()

# > mu_draws_national_fut[, 1:22] %>%
#   +   apply(sum, MARGIN = 1) %>% quantile(probs = c(.5, .025, .975))
# 50%    2.5%   97.5% 
# 1409908 1404466 1415079 
# > mu_draws_national_fut[, 22+1:22] %>%
#   +   apply(sum, MARGIN = 1) %>% quantile(probs = c(.5, .025, .975))
# 50%    2.5%   97.5% 
# 1434308 1425102 1443189 
# > mu_draws_national_fut[, 44+1:22] %>%
#   +   apply(sum, MARGIN = 1) %>% quantile(probs = c(.5, .025, .975))
# 50%    2.5%   97.5% 
# 1453905 1440707 1466725 

