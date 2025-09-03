source("./CODES/GH_data_prep.R")

dir="./CODES/"

for(proj_ref_year in c(1, 5)){
  # proj_ref_year = 1
  stanfile = paste0(dir, "GH_proj_ref", proj_ref_year, "y.stan")
  model <- cmdstanr::cmdstan_model(stanfile, force_recompile=T)
  
  niter <- 1e3
  w <- .5
  
  fit <-model$sample(
    data=data_stan, 
    iter_warmup=niter*w, 
    iter_sampling=niter, 
    chains=4,
    parallel_chains = 4, 
    refresh=niter*.01,
    max_treedepth = 20,
    adapt_delta = 0.8, 
    seed = 1
  )
  
  fit$cmdstan_diagnose()
  
  sampling_result <- fit$draws()
  
  if(proj_ref_year ==1){
    save(sampling_result, file="./CODES/data/stan_result_mean1y_re2019.RData") 
  }
  if(proj_ref_year ==5){
    save(sampling_result, file="./CODES/data/stan_result_mean5y_re2015_2019.RData") 
  }
  
}
