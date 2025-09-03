source("./CODES/GH_data_prep.R")
load(file="./CODES/data/stan_result_mean1y_re2019.RData") 
# load(file="./CODES/data/stan_result_mean5y_re2015_2019.RData")

library(posterior)
posterior_result <- as_draws_df(sampling_result)
beta_pref_draws <- subset_draws(posterior_result, variable = "beta_pref")

beta_mean_df=data.frame(
  year=2000:2022, 
  beta_p1_fit=NA, 
  beta_p1_upr=NA, 
  beta_p1_lwr=NA, 
  beta_p2_fit=NA, 
  beta_p2_upr=NA, 
  beta_p2_lwr=NA, 
  beta_p3_fit=NA, 
  beta_p3_upr=NA, 
  beta_p3_lwr=NA
)

for(year in 2000:2022){
  posterior_result%>%subset_draws(variable = "beta_mean_long") -> tmp_draws
  tmp_draws[,(year-1999)*3-2:0]%>%
    apply(MARGIN=2, FUN=quantile, probs=c(0.5, .975, .025))%>%
    as.vector()->beta_mean_df[year-1999,2:10]
}

library(ggplot2)

beta_mean_df%>%
  ggplot(aes(x=year, y=beta_p1_fit))+
  geom_line(linewidth=.25)+
  geom_ribbon(aes(ymin=beta_p1_lwr, ymax=beta_p1_upr), alpha=.2)+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+
  scale_y_continuous(
    breaks = 27+0.5*0:6,
  ) +
  scale_x_continuous(
    breaks = 2000:2022,
  )+
  labs(
    title = paste0("(A)", " Coefficient of the 1st vector, "),
    x = "Year",
    y = "Value"
  )+
  geom_vline(aes(xintercept = 2019.5), linetype = "dashed") -> plot_1

beta_mean_df%>%
  ggplot(aes(x=year, y=beta_p2_fit))+
  geom_line(linewidth=.25)+
  geom_ribbon(aes(ymin=beta_p2_lwr, ymax=beta_p2_upr), alpha=.2)+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    #axis.text.x = element_text(hjust = 0, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+
  scale_y_continuous(
    breaks = -3:3 * .1,
  ) +
  scale_x_continuous(
    breaks = 2000:2022,
  )+
  labs(
    title = paste0("(B)", " Coefficient of the 2nd vector, "),
    x = "Year",
    y = "Value"
  )+
  geom_vline(aes(xintercept = 2019.5), linetype = "dashed") -> plot_2

beta_mean_df%>%
  ggplot(aes(x=year, y=beta_p3_fit))+
  geom_line(linewidth=.25)+
  geom_ribbon(aes(ymin=beta_p3_lwr, ymax=beta_p3_upr), alpha=.2)+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+
  scale_y_continuous(
    breaks = -3:3 * .1,
  ) +
  scale_x_continuous(
    breaks = 2000:2022,
  )+
  labs(
    title = paste0("(C)", " Coefficient of the 3rd vector, "),
    x = "Year",
    y = "Value"
  )+
  geom_vline(aes(xintercept = 2019.5), linetype = "dashed")-> plot_3


library(patchwork)
tiff("./CODES/RESULTS/Fig_S3_beta_1y.tiff", width = 9, height = 9, units = "in", res = 1000, compression = "lzw")
plot_1/plot_2/plot_3
dev.off()
