source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 

library(posterior)
posterior_result <- as_draws_df(sampling_result)
sigma_beta_time_draws <- subset_draws(posterior_result, variable = "sigma_beta_time")

###
###
###
sigma_beta_time_draws[,1]%>%unlist()%>%quantile(probs = c(.5, .025, .975))
sigma_beta_time_draws[,2]%>%unlist()%>%quantile(probs = c(.5, .025, .975))
sigma_beta_time_draws[,3]%>%unlist()%>%quantile(probs = c(.5, .025, .975))


###
###
###
sigma_re_beta_pref_draws <- subset_draws(posterior_result, variable = "sigma_re_beta_pref")

sigma_re_beta_pref_draws[, 1:60]%>%
  apply(quantile, MARGIN=2, probs = c(.5, .025, .975))%>%
  t()%>%data.frame() -> sigma_re_beta_pref_res

sigma_re_beta_pref_df = matrix(nrow=20, ncol=3)%>%data.frame()
for(y in 1:20){
  sigma_re_beta_pref_res[3*(y-1)+1, 1:3] -> sigma_re_beta_pref_df[y, 1:3]
  sigma_re_beta_pref_res[3*(y-1)+2, 1:3] -> sigma_re_beta_pref_df[y, 4:6]
  sigma_re_beta_pref_res[3*(y-1)+3, 1:3] -> sigma_re_beta_pref_df[y, 7:9]
}

colnames(sigma_re_beta_pref_df) <- c("p1_median", "p1_lower", "p1_upper", 
                                      "p2_median", "p2_lower", "p2_upper", 
                                      "p3_median", "p3_lower", "p3_upper"
                                      )
sigma_re_beta_pref_df%>%
  round(digits = 3)%>%
  readr::write_csv(paste0("./CODES/RESULTS/", "sigma_re_beta_results.csv"))



###
###
###
re_intercept_draws <- subset_draws(posterior_result, variable = "re_intercept")
ncol(re_intercept_draws)-3 -> ncol_max

re_intercept_draws[, 1:ncol_max] %>%
  apply(quantile, MARGIN=2, probs = c(.5, .025, .975)) %>%
  t() %>% data.frame() -> re_intercept_draws_res

colnames(re_intercept_draws_res) <- c("median", "lower", "upper")

ag_list <- lt_national_for_arriaga[["2000.Total"]]["Age"] %>% unlist()
ag_list[22] <- "100+"

library(stringr)
re_intercept_draws_res %<>%
  mutate(age_idx = rep(1:22, 47)) %>%
  mutate(pref_idx = rep(1:47, each = 22)) %>%
  mutate(pref_name = str_to_title(pref_list[pref_idx]), 
         age_group = ag_list[age_idx])

re_intercept_draws_res %<>%
  select(-c(age_idx, pref_idx))

re_intercept_draws_res %>%
  mutate(median = round(median, digits = 3), 
         lower = round(lower, digits = 3), 
         upper = round(upper, digits = 3), 
  ) %>%
  readr::write_csv(paste0("./CODES/RESULTS/", "re_intercept_draws_res.csv"))

###
###
###
re_eq2011_3_04_draws <- subset_draws(posterior_result, variable = "re_eq2011_3_04")

re_eq2011_3_04_draws[, 1:2]%>%
  apply(quantile, MARGIN=2, probs = c(.5, .025, .975)) %>%
  t() %>% data.frame()

re_eq2011_4_04_draws <- subset_draws(posterior_result, variable = "re_eq2011_4_04")
re_eq2011_4_04_draws[, 1:2] %>%
  apply(quantile, MARGIN=2, probs = c(.5, .025, .975)) %>%
  t() %>% data.frame()


re_eq2011_7_04_draws <- subset_draws(posterior_result, variable = "re_eq2011_7_04")
re_eq2011_7_04_draws[, 1:2] %>%
  apply(quantile, MARGIN=2, probs = c(.5, .025, .975)) %>%
  t() %>% data.frame()

###
###
###
re_beta_pref_draws <- subset_draws(posterior_result, variable = "re_beta_pref")

re_beta_pref_draws[, 1:(ncol(re_beta_pref_draws)-3)] %>%
  apply(quantile, MARGIN=2, probs = c(.5, .025, .975)) %>%
  t() %>% data.frame() -> re_beta_pref_draws_res

colnames(re_beta_pref_draws_res) <- c("median", "lower", "upper")

re_beta_pref_df <- matrix(nrow = 20*47, ncol = 9)%>%data.frame()
colnames(re_beta_pref_df) <- c("p1_median", "p1_lower", "p1_upper", 
                               "p2_median", "p2_lower", "p2_upper", 
                               "p3_median", "p3_lower", "p3_upper"
                               )
re_beta_pref_df %<>%mutate(pref = NA, year = NA)

for(pref_idx in 1:47){
  
  for(year in 2000:2020){
    
    re_beta_pref_df[(pref_idx-1)*20 + year - 1999, 1:3] <-   re_beta_pref_draws_res[(pref_idx-1)*60 + 3*(year -2000) +1, 1:3] 
    re_beta_pref_df[(pref_idx-1)*20 + year - 1999, 4:6] <-   re_beta_pref_draws_res[(pref_idx-1)*60 + 3*(year -2000) +2, 1:3] 
    re_beta_pref_df[(pref_idx-1)*20 + year - 1999, 7:9] <-   re_beta_pref_draws_res[(pref_idx-1)*60 + 3*(year -2000) +3, 1:3] 
    
    re_beta_pref_df[(pref_idx-1)*20 + year - 1999, 10] <- pref_idx
    re_beta_pref_df[(pref_idx-1)*20 + year - 1999, 11] <- year
    
  }
}

re_beta_pref_df %<>%
  mutate(pref_name = stringr::str_to_title(pref_list[pref]))

re_beta_pref_df %>%
  readr::write_csv(paste0("./CODES/RESULTS/", "re_beta_pref_res.csv"))
###
