source("./CODES/GH_data_prep.R")

mx_age_plot = matrix(nrow = 22, ncol = 23+2) %>% data.frame()

colnames(mx_age_plot) = c("age", "age_name", paste0("y", 2000:2022))

mx_age_plot[,1] <- 1:22
mx_age_plot[,2] <-lt_national_for_arriaga[[paste0(y, ".Total")]]["Age"]

for(y in 2000:2022){
  mx_age_plot[1:22, y - 1997] <- lt_national_for_arriaga[[paste0(y, ".Total")]][["mx"]] %>% log()
}

library(RColorBrewer)

age_labels = c(mx_age_plot$age_name[1:21], "100+")

mx_age_plot%>%
  ggplot(aes(x = age))+
  geom_line(aes(y = get(paste0("y", 2000)), color = "2000"), linewidth=.5, linetype = "dashed") +
  geom_line(aes(y = get(paste0("y", 2010)), color = "2010"), linewidth=.5, linetype = "dashed") +
  geom_line(aes(y = get(paste0("y", 2019)), color = "2019"), linewidth=.75) +
  geom_line(aes(y = get(paste0("y", 2020)), color = "2020"), linewidth=.75) +
  geom_line(aes(y = get(paste0("y", 2021)), color = "2021"), linewidth=.75) +
  geom_line(aes(y = get(paste0("y", 2022)), color = "2022"), linewidth=.75) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "(A) Mortality Rate (log-scale) by Age Groups", 
       x = "Age Group (year)", 
       y = "log(Mortality Rate)",
       color = "Year") +
  theme(plot.title = element_text(
    size = 20,
    face = "bold",
    margin = margin(b = 10)), 
    legend.position = "right", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  scale_x_continuous(
    breaks = 1:22,
    labels = age_labels,
    guide = guide_axis(angle = 45)
  )+
  scale_y_continuous(limits=c(-10, 0))->plot_log_mx

plot_log_mx

mx_age_plot%>%
  ggplot(aes(x = age))+
  geom_line(aes(y = get(paste0("y", 2020))-get(paste0("y", 2019)), color = "2020"), linewidth=1.25) +
  geom_line(aes(y = get(paste0("y", 2021))-get(paste0("y", 2019)), color = "2021"), linewidth=1.25) +
  geom_line(aes(y = get(paste0("y", 2022))-get(paste0("y", 2019)), color = "2022"), linewidth=1.25) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "(B) Change in Mortality Rate by Age Groups since 2019", 
       x = "Age Group (year)", 
       y = "log(Mortality Rate)",
       color = "Year") +
  theme(plot.title = element_text(
    size = 20,
    face = "bold",
    margin = margin(b = 10)), 
  legend.position = "right", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
scale_x_continuous(
  breaks = 1:22,
  labels = age_labels,
  guide = guide_axis(angle = 45)
)->plot_log_mx_diff

plot_log_mx_diff

library(patchwork)

tiff("./CODES/RESULTS/Fig1_mortality_change.tiff", width = 12, height = 12, units = "in", res = 1000, compression = "lzw")
plot_log_mx/plot_log_mx_diff
dev.off()
