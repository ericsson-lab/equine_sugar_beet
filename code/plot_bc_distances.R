source("code/data_beta_diversity.R")

{
library(rstatix)
library(patchwork)
}

bc_dist <- generate_dist("bray")

bc_dist_long <- bc_dist %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "sampleid") %>% 
  pivot_longer(-sampleid,
               values_to = "dist",
               names_to = "b") 

d1_ids <- metadata %>% 
  select(sampleid, day, horse) %>% 
  arrange(horse, day) %>% 
  filter(day == 1) %>%
  mutate(b = sampleid) %>% 
  rename(sampleid_d1 = "sampleid",
         horse_d1 = "horse") %>% 
  select(-day)
  
  
from_d1 <- bc_dist_long %>% 
  inner_join(., d1_ids, by = "b") %>% 
  inner_join(., metadata, by = "sampleid") %>%
  filter(horse == horse_d1) %>% 
  # filter(sampleid != b) %>% 
  ggplot(aes(x = day, y = dist)) +
  geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
  geom_line(aes(group = horse, color = treatment), alpha = 0.3, linewidth = 1) +
  stat_summary(fun.y = mean,  geom = "line", size = 1.5, aes(group = treatment, color = treatment)) + 
  scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
  scale_y_continuous(expand = c(0,0), labels = scales::comma) +
  scale_x_continuous(expand = c(0,0), limits = c(1,12), breaks = c(seq(1,12,1))) +
  theme_classic() +
  labs(x = "Day", y = "Bray-Curtis Distance\nFrom Baseline") +
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
  
bc_dist_long %>% 
  inner_join(., d1_ids, by = "b") %>% 
  inner_join(., metadata, by = "sampleid") %>%
  filter(horse == horse_d1) %>% 
  filter(sampleid != b) %>% 
  anova_test(dist ~ treatment * as.factor(day)) %>% 
  write_tsv("stats/bc_distance_from_baseline_anova_treatment_day.tsv")

bc_dist_long %>% 
  inner_join(., d1_ids, by = "b") %>% 
  inner_join(., metadata, by = "sampleid") %>%
  filter(horse == horse_d1) %>% 
  filter(sampleid != b) %>% 
  tukey_hsd(dist ~ treatment * as.factor(day)) %>% 
  arrange(p.adj) %>% 
  write_tsv("stats/bc_distance_from_baseline_tukey_treatment_day.tsv")

b_ids <- metadata %>% 
  select(sampleid, day, horse) %>% 
  arrange(horse, day) %>% 
  mutate(b = sampleid) %>% 
  rename(sampleid_b = "sampleid",
         horse_b = "horse")

day_to_day <- bc_dist_long %>% 
  inner_join(., b_ids, by = "b") %>%
  inner_join(., metadata, by = "sampleid") %>%
  filter(horse == horse_b) %>% 
  filter(sampleid != b) %>% 
  mutate(date_diff = day.y - day.x) %>% 
  filter(date_diff == 1) %>% 
  ggplot(aes(x = day.y, y = dist)) +
  geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
  geom_line(aes(group = horse, color = treatment), alpha = 0.3, linewidth = 1) +
  stat_summary(fun.y = mean,  geom = "line", size = 1.5, aes(group = treatment, color = treatment)) + 
  scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
  scale_y_continuous(expand = c(0,0), labels = scales::comma) +
  scale_x_continuous(expand = c(0,0), limits = c(2,12), breaks = c(seq(1,12,1))) +
  theme_classic() +
  labs(x = "Day", y = "Bray-Curtis Distance\nFrom Previous Day") +
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

from_d1/day_to_day +
  plot_layout(guides = 'collect') &
  plot_annotation(tag_levels = "A") &
  theme(legend.position = 'bottom',
        plot.tag = element_text(color = "black", face = "bold", size = 18))

ggsave("plots/bc_distance.png",
       width = 4.5,
       height = 7,
       units = c("in"),
       bg = "white")
  