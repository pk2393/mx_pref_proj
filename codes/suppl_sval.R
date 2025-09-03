source("./CODES/GH_data_prep.R")

sval_df = matrix(nrow = 20, ncol = 2)%>%
  data.frame()

colnames(sval_df) <- c("idx", "sval")

sval_df$idx <- 1:20
sval_df$sval <- M.svd$d
  
sval_df%<>%
  mutate(log_sval = log(sval))%>%
  mutate(log_sval_diff = c(diff(log_sval), NA))

library(ggplot2)

sval_df%>%
  ggplot(aes(x=idx, y = log_sval))+
  geom_point()+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(hjust = .5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  scale_x_continuous(
    breaks = 1:20,
  )+
  labs(
    title = paste0("log-scaled singular values"),
    y = "log(values)",
    x = "index"
  )+geom_vline(aes(xintercept = 3.5), linetype = "dashed") -> plot_1

sval_df%>%
  ggplot(aes(x=idx, y = log_sval_diff))+
  geom_point()+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
     axis.text.x = element_text(hjust = .5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  scale_x_continuous(
    breaks = 1:20,
  )+
  labs(
    title = paste0("Gap between neighboring log-scaled singular values"),
    y = "Gap",
    x = "index"
  )+geom_vline(aes(xintercept = 3.5), linetype = "dashed") -> plot_2

tiff("./CODES/RESULTS/Fig_suppl_singular_values.tiff", width = 6, height = 6, units = "in", res = 1000, compression = "lzw")
plot_1/plot_2
dev.off()

