source("code/data_alpha_stats.R")

plot_alpha <- function(stat, ylab){ 
  alpha_stats %>% 
  inner_join(., metadata, by = "sampleid") %>% 
  ggplot(aes(x = day, y = {{stat}} )) +
  geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
  geom_line(aes(group = horse,
                color = treatment), alpha = 0.3, linewidth = 1) +
  stat_summary(fun.y = mean,  geom = "line", size = 1.5, aes(group = treatment, color = treatment)) + 
  scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
  scale_y_continuous(expand = c(0,0), labels = scales::comma) +
  scale_x_continuous(expand = c(0,0), limits = c(1,12), breaks = c(seq(1,12,1))) +
  theme_classic() +
  labs(x = "Day", y = ylab) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    
    strip.background = element_rect(fill = NA),
    strip.text = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black", face = "bold", size = 12),
    legend.title = element_text(color = "black", face = "bold", size = 12),
    legend.position = "bottom"
  )
}

chao1_plot <- plot_alpha(chao1$chao1, "Chao1 Index") +
  ylim(500, 1750)
shannon_plot <- plot_alpha(shannon, "Shannon Index") +
  ylim(3, 7)

chao1_plot / shannon_plot +
  plot_layout(guides = 'collect') &
  plot_annotation(tag_levels = "A") &
  theme(legend.position = 'bottom',
        plot.tag = element_text(color = "black", face = "bold")) 
  

ggsave("plots/fig1_alpha_plot.png",
       width = 4.5,
       height = 7,
       units = c("in"),
       bg = "white")
