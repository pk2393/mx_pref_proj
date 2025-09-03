source("./CODES/GH_data_prep.R")

rsv_plot <- matrix(nrow = 22, ncol = 3+2) %>% data.frame()

colnames(rsv_plot) = c("age", "age_name", paste0("rsv", 1:3))

rsv_plot[,1] <- 1:22
rsv_plot[,2] <- lt_national_for_arriaga[[paste0(2000, ".Total")]]["Age"]
rsv_plot[, 3:5] <- M.svd$v[,1:3]

library(RColorBrewer)

age_labels = c(rsv_plot$age_name[1:21], "100+")

rsv_plot %>%
  ggplot(aes(x = age))+
  geom_line(aes(y = get(paste0("rsv", 1))), linewidth=.75)+
  theme_minimal() +
  labs(title = "(A) Baseline Mortality", 
       x = "Age Group (year)", 
       y = "Value",
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
  scale_y_continuous(
    limits = c(-.55, .55),  
    breaks = seq(-.5, .5, by = 0.1)  
  )+
  geom_hline(aes(yintercept = 0), linetype = "dashed")->plot1

rsv_plot%>%
  ggplot(aes(x = age))+
  geom_line(aes(y = get(paste0("rsv", 2))), linewidth=.75)+
  theme_minimal() +
  labs(title = "(B) Higher Infant and Adult mortality", 
       x = "Age Group (year)", 
       y = "Value",
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
  scale_y_continuous(
    limits = c(-.55, .55),  
    breaks = seq(-.5, .5, by = 0.1)  
  )+
  geom_hline(aes(yintercept = 0), linetype = "dashed") -> plot2

plot2

rsv_plot %>%
  ggplot(aes(x = age))+
  geom_line(aes(y = get(paste0("rsv", 3))), linewidth=.75)+
  theme_minimal() +
  labs(title = "(C) Higher Child-Adolescent mortality", 
       x = "Age Group (year)", 
       y = "Value",
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
  scale_y_continuous(
    limits = c(-.55, .55),  
    breaks = seq(-.5, .5, by = 0.1)  
  )+
  geom_hline(aes(yintercept = 0), linetype = "dashed") -> plot3

plot3


library(patchwork)

tiff("./CODES/RESULTS/Fig_S2_right_singular_vectors.tiff", width = 9, height = 15, units = "in", res = 1000, compression = "lzw")
plot1/plot2/plot3
dev.off()
