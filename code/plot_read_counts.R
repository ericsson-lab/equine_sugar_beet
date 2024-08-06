library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(rstatix)
library(gt)

source("code/load_data.R")

sequences <- read_tsv("data/sequences/per-sample-fastq-counts_trimmed.tsv")
colnames(sequences) <- c("sampleid", "forward", "reverse")

stats <- sequences %>% 
  right_join(., metadata, by = "sampleid") %>% 
  t_test(forward ~ treatment) %>% 
  add_significance() %>% 
  add_xy_position()

stats %>% 
  write_tsv("stats/read.count.tsv")
stats %>% 
  gt() %>% 
  gtsave("stats/plots/read.count.png")
sum_stats <- sequences %>% 
  right_join(., metadata, by = "sampleid") %>% 
  group_by(treatment) %>% 
  summarize(mean = mean(forward),
            sd = sd(forward))
sum_stats %>% 
  gt() %>% 
  gtsave("stats/plots/read.count.sum.stats.png")
sum_stats %>% 
  write_tsv("stats/read.count.sum.stats.tsv")

sequences %>% 
  right_join(., metadata, by = "sampleid") %>% 
  ggplot(aes(x = treatment, y = forward)) +
  geom_beeswarm(aes(color = treatment), size = 1, cex = 2.5) +  
  geom_boxplot(width = 1, fill = NA, color = "black", outlier.color = NA) +
  theme_classic() +
  scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
  scale_y_continuous(expand = c(0,0), labels = scales::comma, limits = c(0, 215000)) +
  stat_pvalue_manual(data = stats, inherit.aes = F, tip.length = 0, size = 8, bracket.size = 1.5) +
  theme_classic() +
  labs(x = "Day", y = "Read Count") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    axis.title.x = element_blank(),
    strip.background = element_rect(fill = NA),
    strip.text = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black", face = "bold", size = 12),
    legend.title = element_text(color = "black", face = "bold", size = 12),
    legend.position = "none"
  )

ggsave("plots/read_counts.png",
       width = 3,
       height = 3.25,
       units = c("in"),
       bg = "white")
