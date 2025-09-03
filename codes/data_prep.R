rm(list=ls())
library(dplyr);library(magrittr)

###
readr::read_csv("pref_info.csv") -> pref.info
pref_list<-pref.info$pref_list


lt_national <- read.table("https://www.ipss.go.jp/p-toukei/JMD/00/STATS/bltper_5x1.txt", skip = 2, header = 2)
death_national <- read.table("https://www.ipss.go.jp/p-toukei/JMD/00/STATS/Deaths_5x1.txt", skip = 2, header = 2)
etr_national <- read.table("https://www.ipss.go.jp/p-toukei/JMD/00/STATS/Exposures_5x1.txt", skip = 2, header = 2)

w = c(1, 4, c(rep(5, 20)))

lt_calc = function(lt_orig, death_vec, etr_vec){
  
  lt_tmp  <- matrix(nrow = dim(lt_orig)[1]-2, 
                    ncol = dim(lt_orig)[2]) %>% data.frame()
  
  colnames(lt_tmp) <- colnames(lt_orig)
  lt_tmp$Year <-lt_orig$Year[1:22]
  lt_tmp$Age  <- c(lt_orig$Age[1:21], "100+")
  if(length(death_vec)>22){
    lt_tmp$mx <- c(death_vec[1:21], sum(death_vec[22:24])) / c(etr_vec[1:21], sum(etr_vec[22:24]))
  }else{
    lt_tmp$mx <- c(death_vec) / c(etr_vec)
  }
  
  lt_tmp$ax[1:21] <- lt_orig$ax[1:21]
  lt_tmp$ax[22] <- 1/lt_tmp$mx[22]
  
  lt_tmp %<>%
    mutate(w = c(1, 4, c(rep(5, 20))), 
           ax = case_when(
             ax > w & Age != "100+" ~ w/2, 
             T ~ ax
           ),
           qx = case_when(
             Age =="100+" ~ 1, 
             T ~ (mx * w) / (1 + (w - ax) * mx) 
           ))
  
  lt_tmp$lx[1] <- 1e5
  for(n in 2:nrow(lt_tmp)){
    lt_tmp$lx[n] <- lt_tmp$lx[n-1] * (1-lt_tmp$qx[n-1])
  }
  
  lt_tmp %<>% 
    mutate(dx = c(-diff(lx), NA))%>%
    mutate(dx = case_when(is.na(dx) == T ~ lx, T ~ dx))
  
  for(n in 1:(nrow(lt_tmp))){
    if(n <nrow(lt_tmp)){
      lt_tmp$Lx[n] = lt_tmp$lx[n+1] * lt_tmp$w[n] + (lt_tmp$ax[n] * lt_tmp$dx[n])
    }else{
      lt_tmp$Lx[n]=(lt_tmp$ax[n] * lt_tmp$dx[n])
    }
  }
  
  Tx_vec = sum(lt_tmp$Lx) - c(0, cumsum(lt_tmp$Lx)[-22])
  
  lt_tmp %>%
    mutate(Tx = Tx_vec, 
           ex = Tx / lx) -> lt_y_shortened
  
  
  return(lt_y_shortened)
}

lt_national_for_arriaga = list()

for(y in 2000:2022){
  lt_y <- lt_national%>%
    filter(Year == y)
  
  death_vec <- (death_national%>%filter(Year == y))$Total
  etr_vec <- (etr_national%>%filter(Year == y))$Total
  
  lt_y_shortened <- lt_calc(lt_y, death_vec, etr_vec)
  
  lt_y_shortened$death <- c(death_vec[1:21], sum(death_vec[22:24]))
  lt_y_shortened$etr <- c(etr_vec[1:21], sum(etr_vec[22:24]))
  
  ### store
  lt_national_for_arriaga[[paste0(y, ".Total")]] <- lt_y_shortened
}


###
### Morality matrix for 2000-2022
###
mx_df = matrix(NA, nrow = 22, ncol=23) %>% data.frame()
colnames(mx_df) <- c(paste0("mx.", 2000:2022))

for(y in 2000:2022){
  mx_df[[paste0("mx.", y)]] <- lt_national_for_arriaga[[paste0(y, ".Total")]][["mx"]]
}

mx_df[, c(1:20)] |> log() |> as.matrix() |> t() -> M
svd(M) -> M.svd

d_short <- c(head(M.svd$d, 3), rep(0, length(M.svd$d)-3)) |> diag()

(M.svd$u%*%d_short)[,1:3] |> t() -> beta_vec_prior


#####
##### Prefecture Data
#####

mx_df = matrix(NA, nrow = 22, ncol=20*47)
death_df = matrix(NA, nrow = 22, ncol=20*47)
etr_df = matrix(NA, nrow = 22, ncol=20*47)

mx_df_fut = matrix(NA, nrow = 22, ncol=3*47)
death_df_fut = matrix(NA, nrow = 22, ncol=3*47)
etr_df_fut = matrix(NA, nrow = 22, ncol=3*47)

for(i in 1:47){
  # i=1
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }
  
  lt_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/bltper_5x1.txt"), skip=2, header=T)
  death_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  etr_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Exposures_5x1.txt"), skip=2, header=T)

  for(y in 2000:2019){
    lt_y <- lt_pref %>% filter(Year == y)
    death_vec <- (death_pref%>%filter(Year == y))$Total
    etr_vec <- (etr_pref%>%filter(Year == y))$Total
    
    lt_y_shortened <- lt_calc(lt_y, death_vec, etr_vec)
    
    lt_y_shortened$death <- c(death_vec[1:21], sum(death_vec[22:24]))
    lt_y_shortened$etr <- c(etr_vec[1:21], sum(etr_vec[22:24]))
    
    mx_df[,20*(i-1)+y-1999] <- lt_y_shortened$mx
    death_df[,20*(i-1)+y-1999] <- lt_y_shortened$death
    etr_df[,20*(i-1)+y-1999] <- lt_y_shortened$etr
  }
  for(y in 2020:2022){
    lt_y <- lt_pref %>% filter(Year == y)
    death_vec <- (death_pref%>%filter(Year == y))$Total
    etr_vec <- (etr_pref%>%filter(Year == y))$Total
    
    lt_y_shortened <- lt_calc(lt_y, death_vec, etr_vec)
    lt_y_shortened$death <- c(death_vec[1:21], sum(death_vec[22:24]))
    lt_y_shortened$etr <- c(etr_vec[1:21], sum(etr_vec[22:24]))
    
    mx_df_fut[,3*(i-1)+y-2019] <- lt_y_shortened$mx
    death_df_fut[,3*(i-1)+y-2019] <- lt_y_shortened$death
    etr_df_fut[,3*(i-1)+y-2019] <- lt_y_shortened$etr
  }
}

eq2011_prior=list()
for(i in c(3, 4, 7)){
  if(i <=9){
    pref_idx_full = paste0("0", i)
  }else{
    pref_idx_full = paste0(i)
  }

  dx_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Deaths_5x1.txt"), skip=2, header=T)
  etr_pref = read.table(paste0("https://www.ipss.go.jp/p-toukei/JMD/", pref_idx_full,"/STATS/Exposures_5x1.txt"), skip=2, header=T)
  
  dx_vec_2010 <- (dx_pref%>% filter(Year == 2010))$Total 
  dx_vec_2011 <- (dx_pref%>% filter(Year == 2011))$Total 
  
  etr_vec_2010 <- (etr_pref%>% filter(Year == 2010))$Total 
  etr_vec_2011 <- (etr_pref%>% filter(Year == 2011))$Total 
  
  mx_2010 <- c(dx_vec_2010[1:21], sum(dx_vec_2010[22:24])) / c(etr_vec_2010[1:21], sum(etr_vec_2010[22:24]))
  mx_2011 <- c(dx_vec_2011[1:21], sum(dx_vec_2011[22:24])) / c(etr_vec_2011[1:21], sum(etr_vec_2011[22:24]))
  
  log_mx_prior = pmax(0, log(mx_2011)-log(mx_2010))
  
  eq2011_prior[[paste0("pref", i)]] <- log_mx_prior
}

### Eq data
### source:  https://www.fdma.go.jp/publication/hakusho/h29/data/1705.html
death_eq_3 = c(81, 39, 22, 68, 63, 90, 131, 169, 192, 232, 308, 415, 541, 576, 724, 747, 667, 372, 148, 49, 8)
death_eq_4 = c(205, 181, 142, 146, 150, 199, 269, 304, 367, 388, 516, 724, 985, 1016, 1246, 1326, 1153, 759, 301, 91, 15)
death_eq_7 = c(28, 22, 22, 54, 22, 32, 28, 51, 54, 56, 87, 134, 179, 142, 216, 223, 222, 131, 40, 12, 2)

death_df[3:22, 20*(3-1)+12] <- death_df[3:22, 20*(3-1)+12]-death_eq_3[2:21]
death_df[3:22, 20*(4-1)+12] <- death_df[3:22, 20*(4-1)+12]-death_eq_4[2:21]
death_df[3:22, 20*(7-1)+12] <- death_df[3:22, 20*(7-1)+12]-death_eq_7[2:21]

death_df[3:22, 20*(3-1)+12]
death_df[3:22, 20*(4-1)+12]
death_df[3:22, 20*(7-1)+12]



data_stan=list(
  Ntime=20,
  Nage=22,
  Npref=47,
  NtimeNpref=20*47,
  Ntime_fut=3, 
  Ntime_futNpref=3*47,
  svd_vec=M.svd$v[,1:3],
  beta_vec_prior=beta_vec_prior,
  etr_mat=etr_df,
  mx_obs_mat=mx_df,
  death_mat=round(death_df,0), 
  etr_mat_fut=etr_df_fut,
  death_mat_fut=round(death_df_fut,0), 
  time_index=0:22, 
  re_eq2011_3_prior=c(eq2011_prior[[paste0("pref", 3)]]), 
  re_eq2011_4_prior=c(eq2011_prior[[paste0("pref", 4)]]), 
  re_eq2011_7_prior=c(eq2011_prior[[paste0("pref", 7)]]), 
  death_eq_3=death_eq_3, 
  death_eq_4=death_eq_4, 
  death_eq_7=death_eq_7
  
)

library(cmdstanr)
