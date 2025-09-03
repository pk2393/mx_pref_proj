data{
  int Ntime; // 20: 2000-2019
  int Nage; // 22
  int Npref; // 47
  int Ntime_fut; // 3
  int NtimeNpref; // 20*47
  int Ntime_futNpref; 
  matrix[Nage, 3] svd_vec; // v<-udv (svd)
  matrix[3, Ntime] beta_vec_prior; // beta=ud <-udv (svd) : mean of the prior distr of beta_mean
  matrix[Nage, NtimeNpref] etr_mat; // exposure to risk population
  array[Nage, NtimeNpref] int death_mat; // death counts 
  matrix[Nage, NtimeNpref] mx_obs_mat; // mortality table 
  matrix[Nage, Ntime_futNpref] etr_mat_fut; // exposure to risk population
  array[Nage, Ntime_futNpref] int death_mat_fut; // death counts in 2020-2022

  array[Nage-1] int death_eq_3;
  array[Nage-1] int death_eq_4;
  array[Nage-1] int death_eq_7;
  
  vector<lower=0>[Nage] re_eq2011_3_prior; 
  vector<lower=0>[Nage] re_eq2011_4_prior; 
  vector<lower=0>[Nage] re_eq2011_7_prior;
  
}

parameters{
  matrix [3, Ntime] beta_mean; //mean over time 
  vector<lower=0>[3] beta_mean_std; // std around beta_mean_prior
  vector<lower=0>[3] sigma_beta_time; // std: assuming temporal smoothness
  
  matrix [3, Ntime*(Npref-1)] re_beta_pref_init; // random_effect of beta by prefecture
  matrix<lower=0> [3, Ntime] sigma_re_beta_pref; // variability of beta_pref
  
  matrix [Nage, Npref-1] re_intercept_init; // age specific RE of log(m_(age, pref, time))
  vector<lower=0>[Nage] sigma_re_intercept;  // variability by age
  
  vector<lower=0>[2] re_eq2011_3_04;
  vector<lower=0>[2] re_eq2011_4_04;
  vector<lower=0>[2] re_eq2011_7_04;
}

transformed parameters{
  matrix [Nage, Npref] re_intercept;
  for(a in 1:Nage){
    real sum_re_tmp = 0;
    for(b in 1:(Npref-1)){
      re_intercept[a, b] = re_intercept_init[a, b];
      sum_re_tmp += re_intercept_init[a, b];
    }
    re_intercept[a, Npref] = -sum_re_tmp;
  }
  
  matrix[3, Ntime * Npref] re_beta_pref;
  for (p in 1:3) {
    for (t in 1:Ntime) {
      real sum_tmp = 0;
      for (n in 1:(Npref-1)) {
        re_beta_pref[p, t + (n-1)*Ntime] = re_beta_pref_init[p, t + (n-1)*Ntime];
        sum_tmp += re_beta_pref_init[p, t + (n-1)*Ntime];
      }
      re_beta_pref[p, t + (Npref-1)*Ntime] = - sum_tmp;
    }
  }
  
  
  vector[Nage] re_eq2011_3;
  re_eq2011_3[1:2] = re_eq2011_3_04;
  re_eq2011_3[3:Nage] = rep_vector(0, (Nage-2));
  vector[Nage] re_eq2011_4;
  re_eq2011_4[1:2] = re_eq2011_4_04;
  re_eq2011_4[3:Nage] = rep_vector(0, (Nage-2));
  vector[Nage] re_eq2011_7;
  re_eq2011_7[1:2] = re_eq2011_7_04;
  re_eq2011_7[3:Nage] = rep_vector(0, (Nage-2));
  
  
  // define the baseline structure for every prefecture (just replication)
  matrix [3, NtimeNpref] beta_mean_mat; 
  for(n in 1:Npref){
    for(t in 1:Ntime){
     beta_mean_mat[1, t+(n-1)*Ntime] = beta_mean[1, t];
     beta_mean_mat[2, t+(n-1)*Ntime] = beta_mean[2, t];
     beta_mean_mat[3, t+(n-1)*Ntime] = beta_mean[3, t];
    }
  }
  
  // beta_pref by prefecture: beta_mean + re_beta = beta_pref
  matrix [3, NtimeNpref] beta_pref=beta_mean_mat+re_beta_pref; 
  matrix [Nage, NtimeNpref] re_intercept_mat;
  for(n in 1:Npref){
    for(a in 1:Nage){
        re_intercept_mat[a, ((n-1)*Ntime+1):(n*Ntime)] = rep_row_vector(re_intercept[a, n], Ntime);
    }
  }
  
  matrix [Nage, NtimeNpref] eq2011_mat = rep_matrix(0, Nage, NtimeNpref);
  eq2011_mat[:, 12+(3-1)*20] = re_eq2011_3;
  eq2011_mat[:, 12+(4-1)*20] = re_eq2011_4;
  eq2011_mat[:, 12+(7-1)*20] = re_eq2011_7;
  
  matrix[Nage, Ntime*Npref] log_mx_mat = svd_vec * beta_pref + re_intercept_mat + eq2011_mat;
  matrix[Nage, Ntime*Npref] mx_mat = exp(log_mx_mat);
  matrix[Nage, Ntime*Npref] mx_mat_no_eq = exp(log_mx_mat - eq2011_mat);
  
  matrix[Nage, Ntime*Npref] mu_dx_mat = mx_mat.* etr_mat;
  matrix[Nage, Ntime*Npref] mu_dx_mat_eq = (mx_mat - mx_mat_no_eq) .* etr_mat;

}

model{
  // beta_mean around prior given by national-level SVD 
  beta_mean_std ~ inv_gamma(2,2);
  for(p in 1:3){
      target+=normal_lpdf(beta_mean[p,1:Ntime] | beta_vec_prior[p,], beta_mean_std[p]);
  }

  // re_beta_pref
  for(p in 1:3){
    for(t in 1:Ntime){
      sigma_re_beta_pref[p, t] ~ inv_gamma(2,2);
    }
  }
  for(t in 1:Ntime){
    for(n in 1:Npref){
      target += normal_lpdf(re_beta_pref[1,t+(n-1)*Ntime] |0, sigma_re_beta_pref[1, t]);
      target += normal_lpdf(re_beta_pref[2,t+(n-1)*Ntime] |0, sigma_re_beta_pref[2, t]);
      target += normal_lpdf(re_beta_pref[3,t+(n-1)*Ntime] |0, sigma_re_beta_pref[3, t]);
    }
  }

  // re_intercept
  sigma_re_intercept ~ inv_gamma(2,2); // prior of sigma_re_intercept
  
  // likelihood for re_intercept
  for(a in 1:Nage){
    target += normal_lpdf(re_intercept[a, :]|rep_vector(0, Npref), sigma_re_intercept[a]);
  }
  
  // // temporal smoothness of beta_mean : baseline = RW2
  sigma_beta_time ~ cauchy(0,1);
  for(p in 1:3){
    target += normal_lpdf(beta_mean[p, 3:Ntime] | 2*beta_mean[p,2:(Ntime-1)] - beta_mean[p,1:(Ntime-2)], sigma_beta_time[p]);
  }
  
  // likelihood for deaths in each age gruop: assume Poisson 
  for(a in 1:Nage){
    target += poisson_lpmf(death_mat[a, :] | mu_dx_mat[a, :] );
  }

  // EQ mortality
  re_eq2011_3_04 ~ normal(re_eq2011_3_prior[1:2], .1);
  re_eq2011_4_04 ~ normal(re_eq2011_4_prior[1:2], .1);
  re_eq2011_7_04 ~ normal(re_eq2011_7_prior[1:2], .1);
  target += poisson_lpmf(death_eq_3[2-1] | mu_dx_mat_eq[2-1, (3-1)*Ntime+12] + mu_dx_mat_eq[2, (3-1)*Ntime+12]);
  target += poisson_lpmf(death_eq_4[2-1] | mu_dx_mat_eq[2-1, (4-1)*Ntime+12] + mu_dx_mat_eq[2, (4-1)*Ntime+12]);
  target += poisson_lpmf(death_eq_7[2-1] | mu_dx_mat_eq[2-1, (7-1)*Ntime+12] + mu_dx_mat_eq[2, (7-1)*Ntime+12]);
}

generated quantities {
  matrix[Nage, Ntime*Npref] mx_mat_resid = mx_mat -mx_obs_mat;
  
  /////////////////////////
  /// Counterfactual mx ///
  /////////////////////////
  
  // RE_beta_pref: average from 2015-2019
  matrix[3, Ntime_fut*Npref] re_beta_pref_fut;
  for (n in 1:Npref) {
    re_beta_pref_fut[1, (n-1)*Ntime_fut + 1] = mean(re_beta_pref[1, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    re_beta_pref_fut[2, (n-1)*Ntime_fut + 1] = mean(re_beta_pref[2, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    re_beta_pref_fut[3, (n-1)*Ntime_fut + 1] = mean(re_beta_pref[3, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    
    re_beta_pref_fut[1, (n-1)*Ntime_fut + 2] = mean(re_beta_pref[1, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    re_beta_pref_fut[2, (n-1)*Ntime_fut + 2] = mean(re_beta_pref[2, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    re_beta_pref_fut[3, (n-1)*Ntime_fut + 2] = mean(re_beta_pref[3, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    
    re_beta_pref_fut[1, (n-1)*Ntime_fut + 3] = mean(re_beta_pref[1, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    re_beta_pref_fut[2, (n-1)*Ntime_fut + 3] = mean(re_beta_pref[2, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
    re_beta_pref_fut[3, (n-1)*Ntime_fut + 3] = mean(re_beta_pref[3, ((n-1)*Ntime+16):((n-1)*Ntime+20)]);
 }

  // extend the drift of 2014->2019 up to 2022
  vector[3] beta_mean_drift;
  for(p in 1:3){
    beta_mean_drift[p] = (beta_mean[p, Ntime] - beta_mean[p, Ntime-6])/5;
  }
  
  // longer version of beta_mean up to 2022
  matrix[3, Ntime+Ntime_fut] beta_mean_long;
  for (t in 1:Ntime) {
      beta_mean_long[1, t] = beta_mean[1, t];
      beta_mean_long[2, t] = beta_mean[2, t];
      beta_mean_long[3, t] = beta_mean[3, t];
    }
  for (t in (Ntime+1):(Ntime+Ntime_fut)){
    beta_mean_long[1, t] = beta_mean_long[1, t-1] + beta_mean_drift[1];
    beta_mean_long[2, t] = beta_mean_long[2, t-1] + beta_mean_drift[2];
    beta_mean_long[3, t] = beta_mean_long[3, t-1] + beta_mean_drift[3];
  }
  
  //  beta_pref future projection from 2020 to 2022
  matrix[3, Ntime_fut*Npref] beta_pref_fut;
  for (n in 1:Npref) {
    for (t in 1:Ntime_fut) {
      beta_pref_fut[1, (n-1)*Ntime_fut + t] = beta_mean_long[1, Ntime+t] + re_beta_pref_fut[1, (n-1)*Ntime_fut + t];
      beta_pref_fut[2, (n-1)*Ntime_fut + t] = beta_mean_long[2, Ntime+t] + re_beta_pref_fut[2, (n-1)*Ntime_fut + t];
      beta_pref_fut[3, (n-1)*Ntime_fut + t] = beta_mean_long[3, Ntime+t] + re_beta_pref_fut[3, (n-1)*Ntime_fut + t];
    }
  }
  
  // re_intercept by prefecture: replicate same values up to 2022 (assumption: temporally invariant re_intercept) 
  matrix [Nage, Ntime_fut*Npref] re_intercept_mat_fut;
  for(n in 1:Npref){
    for(a in 1:Nage){
        re_intercept_mat_fut[a, ((n-1)*Ntime_fut+1):(n*Ntime_fut)] = rep_row_vector(re_intercept[a, n], Ntime_fut);
    }
  }
  
  // projection of future mortality by age, pref, time
  matrix[Nage, Ntime_fut*Npref] log_mx_mat_fut = svd_vec*beta_pref_fut+re_intercept_mat_fut;
  matrix[Nage, Ntime_fut*Npref] mx_mat_fut = exp(log_mx_mat_fut);
  
  // projection of expected deaths by age, pref, time
  matrix[Nage, Ntime_fut*Npref] mu_dx_mat_fut = mx_mat_fut .* etr_mat_fut;

  ////////////////////
  /// CF mortality ///
  ////////////////////
  matrix[Nage, Ntime_fut] mu_dx_fut_national = rep_matrix(0, Nage, Ntime_fut);
  matrix[Nage, Ntime_fut] dx_fut_pred_national = rep_matrix(0, Nage, Ntime_fut);
  
  matrix[Nage, Ntime_fut*Npref] dx_fut_predicted;
  matrix[Nage, Ntime_fut*Npref] exdeath_fut_expected;
  matrix[Nage, Ntime_fut*Npref] exdeath_fut_predicted;

  for(a in 1:Nage){
    for(t in 1:(Ntime_fut*Npref)){
      exdeath_fut_expected[a, t] = death_mat_fut[a, t] - mu_dx_mat_fut[a, t];
      dx_fut_predicted[a,t] = poisson_rng(mu_dx_mat_fut[a, t]);
      exdeath_fut_predicted[a, t] = death_mat_fut[a, t]-dx_fut_predicted[a,t];
    }
    
  }

}
