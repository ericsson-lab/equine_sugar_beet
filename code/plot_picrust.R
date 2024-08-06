{
  library(tidyverse)
  library(cowplot)
}

feature_metadata <- read_tsv("data/picrust_data/picrust_longitudinal/output/feature_metadata.tsv") %>% 
  rename(feature = "id")

feature_data <- read_tsv("data/picrust_data/picrust_longitudinal/output/feeature_data.tsv") %>% 
  rename(feature = "id")
importance_list <- read_tsv('data/picrust_data/picrust_longitudinal/output/importance.tsv')



feature_data %>% 
  select(horse, treatment, day, `PWY-5971`) %>% 
  group_by(horse, treatment) %>% 
  arrange(day) %>% 
  mutate(change = last(`PWY-5971`) - 
           first(`PWY-5971`)) %>% 
  arrange(horse) %>% 
  print(n = 144)

feature_metadata %>% 
  top_n(n = 25, wt = importance) %>%
  summarize(sum = sum(importance))

importance_plot <- feature_metadata %>% 
  top_n(n = 25, wt = importance) %>%
  ggplot(aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "gray70") +
  coord_flip() +
  scale_y_continuous(position = "right", expand = c(0,0), labels = scales::percent,
                     limits = c(0, 0.055)) +
  theme_classic() +
  labs(y = "Importance") +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", face = "bold")
  )

 
picrust_net_change <- feature_data %>% 
  select(-c(feature, group, title)) %>% 
  pivot_longer(-c(horse, treatment, day),
               names_to = "taxon",
               values_to = "rel_abund")  %>% 
  group_by(horse, treatment, taxon) %>% 
  arrange(as.integer(day)) %>% 
  summarize(change = last(rel_abund) - first(rel_abund)) %>% 
  group_by(treatment, taxon) %>% 
  summarize(avg_change = mean(change)) %>% 
  inner_join(., feature_metadata %>% 
               select(feature, importance) %>% 
               rename(taxon = "feature"), 
             by = "taxon") %>% 
  pivot_wider(names_from = "treatment", values_from = "avg_change") %>% 
  arrange(desc(importance)) %>% 
  top_n(25, wt = importance) %>% 
  ggplot(aes(x = reorder(taxon, importance))) + 
  geom_hline(aes(yintercept = 0)) +
  geom_hline(yintercept = c(-0.001, -0.0005, 0.0005, 0.001), 
             linetype = 2, alpha = 0.3) +
  geom_segment(aes(y = Control, yend = `Sugar Beet`,
                   xend = reorder(taxon, importance)),
               size = 3, color = "gray70", alpha = 0.5) +
  geom_point(aes(y = Control), color = "red", size = 3) +
  geom_point(aes(y = `Sugar Beet`), color = "dodgerblue", size = 3) +
  scale_y_continuous(position = "right", expand = c(0,0), 
                     labels = scales::percent,
                     breaks = c(-0.001,0, 0.001),
                     limits = c(-0.0013, 0.0013)) +
  coord_flip() +
  labs(y = "Net Change (%)") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(color = "black", face = "bold", size = 8),
    axis.title.x = element_text(color = "black", face = "bold"),
    axis.line.y = element_blank()
  )

plot_feature <- function(y_value){
  feature_data %>% 
    mutate(group = case_when(group == "ctl" ~ "Standard",
                             group == "pre" ~ "Standard",
                             group == "post" ~ "Sugar Beet")) %>% 
    ggplot(aes(x = as.numeric(day), y = as.numeric({{y_value}}))) +
    geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
    geom_line(aes(group = horse,
                  color = treatment), alpha = 0.3, linewidth = 1) +
    stat_summary(fun.y = mean,  geom = "line", size = 1.5, aes(group = treatment, color = treatment)) + 
    scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    scale_x_continuous(expand = c(0,0), limits = c(1,12), breaks = c(seq(1,12,1))) +
    theme_classic() +
    labs(x = "Day", y = glue::glue("{substitute(y_value)}\nRelative Abundance")) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(color = "black", face = "bold", size = 6),
      axis.title = element_text(color = "black", face = "bold", size = 8),
      
      strip.background = element_rect(fill = NA),
      strip.text = element_text(color = "black", face = "bold"),
      legend.text = element_text(color = "black", face = "bold", size = 12),
      legend.title = element_text(color = "black", face = "bold", size = 12),
      legend.position = "bottom"
    )
}
{
  pwy_5971 <- plot_feature(`PWY-5971`)+ theme(legend.position = "none")
  pwy_7003 <- plot_feature(`PWY-7003`) + theme(legend.position = "none")
  pwy_7221 <- plot_feature(`PWY-7221`) + theme(legend.position = "none")
  pwy_hex <- plot_feature(`HEXITOLDEGSUPER-PWY`) + theme(legend.position = "none")
  pwy_cent <- plot_feature(`CENTFERM-PWY`)
  
  
  r_leg <- get_legend(pwy_cent)
  
  r <- plot_grid(pwy_5971, pwy_7003, pwy_7221, pwy_hex + theme(legend.position = "none"),
                 nrow = 4, rel_heights = c(0.25, 0.25, 0.25, 0.25)) 
  r <- plot_grid(r, r_leg, nrow = 2, rel_heights = c(0.95, 0.05))
  l <- plot_grid(importance_plot, picrust_net_change, rel_widths = c(0.7, 0.3))
}
plot_grid(l, r,
          ncol = 2,
          rel_widths = c(0.6, 0.4))

ggsave("plots/picrust_paths_longitudinal.png",
       width = 9, height = 6,
       units = c("in"),
       bg = "white")

