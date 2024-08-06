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

bc_dist_long %>% 
  inner_join(., b_ids, by = "b") %>%
  inner_join(., metadata, by = "sampleid") %>%
  filter(horse == horse_b) %>% 
  filter(sampleid != b) %>% 
  mutate(date_diff = day.y - day.x) %>% 
  filter(date_diff == 1) %>% 
  anova_test(dist ~ treatment * day.y) %>% 
  write_tsv("stats/bc_distance_from_previous_day_anova_treatment_day.tsv")

bc_dist_long %>% 
  inner_join(., b_ids, by = "b") %>%
  inner_join(., metadata, by = "sampleid") %>%
  filter(horse == horse_b) %>% 
  filter(sampleid != b) %>% 
  mutate(date_diff = day.y - day.x) %>% 
  filter(date_diff == 1) %>% 
  tukey_hsd(dist ~ treatment * as.factor(day.y)) %>% 
  arrange(p.adj) %>% 
  write_tsv("stats/bc_distance_from_previous_day_tukey_treatment_day.tsv")
