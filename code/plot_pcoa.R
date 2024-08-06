{
  library(glue)
  library(ggforce)
}

source("code/data_beta_diversity.R")

bc_dist <- generate_dist(distance = "bray")

bc_pcoa <- generate_pcoa(bc_dist)

plot_pcoa <- function(pcoa_data, title) {
  
  pco1_var <- round(pcoa_data[[2]][1], 2)
  pco2_var <- round(pcoa_data[[2]][2], 2)
  
  pcoa_data[1] %>% 
    as.data.frame() %>% 
    ggplot(aes(x = PCo4, y = PCo5, 
               alpha = day)) +
    geom_point(size = 3) +
    labs(x = glue("PCo1 - ", pco1_var, "%"),
         y = glue("PCo2 - ", pco2_var, "%"),
         subtitle = title) +
    theme_classic()
}

bc_pcoa[1] %>% 
  as.data.frame() %>% 
  
  mutate(group = case_when(group == "ctl" ~ "Standard",
                           group == "pre" ~ "Standard",
                           group == "post" ~ "Sugar Beet")) %>% 
  ggplot(aes(x = .panel_x, y = .panel_y, color = treatment)) +
  geom_point(aes(alpha = day), size = 2) +
  geom_autodensity(aes(fill = treatment), alpha = 0.3,show.legend = F, color= NA,
                   position = 'identity') +
  geom_density2d(show.legend = F) +
  facet_matrix(vars(PCo1:PCo4),
               layer.lower = 3,
               layer.diag = 2,
               layer.upper = 1)  +
  scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
  scale_fill_manual(values = c("red", "dodgerblue")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_alpha_continuous(name = "Day", range = c(0.2, 1), limits = c(0,12),) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", face = "bold", size = 8),
    strip.background = element_rect(fill = NA),
    strip.text = element_text(color = "black", face = "bold", size = 13),
    legend.text = element_text(color = "black", face = "bold", size = 13),
    legend.title = element_text(color = "black", face = "bold", size = 13),
    legend.spacing.y = unit(0.1,"mm")
  ) +
  guides(alpha = guide_legend(override.aes = list(shape = 15,
                                                  size = 5), 
                              order = 3),
         shape = guide_legend(override.aes = list(size = 5),
                              order = 2),
         color = guide_legend(override.aes = list(size = 5),
                              order = 1))


ggsave("plots/fig2_pcoa_matrix.png",
       width = 9,
       height = 7, 
       units = c("in"),
       bg = "white")


