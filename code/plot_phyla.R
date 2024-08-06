source('code/load_data.R')

library(patchwork)

p_names <- taxonomy %>% 
  filter(level == 'Phylum')

rel_abund_table <- table %>% 
  pivot_longer(-featureid, names_to = 'sampleid') %>% 
  left_join(., p_names) %>% 
  group_by(sampleid, taxon) %>% 
  summarise(count = sum(value)) %>% 
  group_by(sampleid) %>% 
  mutate(rel_abund = count/sum(count)) %>% 
  left_join(., metadata) %>% 
  mutate(taxon = case_when(taxon == 'Firmicutes' ~ 'Bacillota',
                           TRUE ~ taxon))

dominant_taxa <- rel_abund_table %>% 
  group_by(taxon) %>% 
  summarise(mean_rel_abund = mean(rel_abund)) %>% 
  arrange(-mean_rel_abund) %>% 
  filter(mean_rel_abund > 0.01)

control_phyla <- rel_abund_table %>%
  filter(taxon %in% dominant_taxa$taxon) %>% 
  filter(treatment == 'Control') %>% 
  ggplot(aes(x = day, y = rel_abund, group = taxon, color = taxon, fill = taxon)) +
  stat_summary(geom = 'ribbon', fun = mean,
               fun.min = function(x) ifelse(mean(x) - sd(x)< 0, 0, mean(x) - sd(x)),
               fun.max = function(x) mean(x) + sd(x),
               linewidth = 0, alpha = 0.3, show.legend = F) +
  stat_summary(geom = 'line', fun = mean, linewidth = 1) +
  geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
  ggprism::theme_prism() +
  scale_y_continuous(limits = c(0,0.65), 
                     expand = expansion(mult = c(0, 0)),
                     labels = scales::percent)+
  scale_x_continuous(breaks = c(1,6,12),
                     expand = c(0,0)) +
  scale_color_brewer(palette = 'Set1', direction = -1, name = 'Phylum') +
  scale_fill_brewer(palette = 'Set1', direction = -1) +
  labs(y = 'Mean Relative\nAbundance',
       x = 'Day') +
  theme(
    legend.text = element_text(face = 'bold', size = 12),
    legend.title = element_text(face = 'bold'),
  )


sbp_phyla <- rel_abund_table %>%
  filter(taxon %in% dominant_taxa$taxon) %>% 
  filter(treatment != 'Control') %>% 
  ggplot(aes(x = day, y = rel_abund, group = taxon, color = taxon, fill = taxon)) +
  stat_summary(geom = 'ribbon', fun = mean,
               fun.min = function(x) ifelse(mean(x) - sd(x)< 0, 0, mean(x) - sd(x)),
               fun.max = function(x) mean(x) + sd(x),
               linewidth = 0, alpha = 0.3, show.legend = F) +
  stat_summary(geom = 'line', fun = mean, linewidth = 1) +
  geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
  ggprism::theme_prism() +
  scale_y_continuous(limits = c(0,0.65), 
                     expand = expansion(mult = c(0, 0)),
                     labels = scales::percent)+
  scale_x_continuous(breaks = c(1,6,12),
                     expand = c(0,0)) +
  scale_color_brewer(palette = 'Set1', direction = -1, name = 'Phylum') +
  scale_fill_brewer(palette = 'Set1', direction = -1) +
  labs(y = 'Mean Relative\nAbundance',
       x = 'Day') +
  theme(
    legend.text = element_text(face = 'bold', size = 12),
    legend.title = element_text(face = 'bold'),
  )


  

f_b_ratio <- rel_abund_table %>% 
  filter(taxon %in% c('Bacteroidota', 'Bacillota')) %>% 
  select(sampleid, taxon, rel_abund, day, horse, treatment) %>% 
  pivot_wider(names_from = 'taxon', 
              values_from = 'rel_abund') %>% 
  mutate(f_b_ratio = Bacillota / Bacteroidota) %>% 
  ggplot(aes(x = day, y = f_b_ratio)) +
  geom_vline(aes(xintercept = 6), linetype = 2, size = 0.5) +
  geom_line(aes(group = horse,
                color = treatment), alpha = 0.3, linewidth = 1) +
  stat_summary(fun.y = mean,  geom = "line", size = 1.5, aes(group = treatment, color = treatment)) + 
  scale_color_manual(values = c("red", "dodgerblue"), name = "Treatment") +
  scale_y_continuous(expand = expansion(mult = c(0,0.00)), limits = c(0, 2.6), labels = scales::comma) +
  scale_x_continuous(expand = c(0,0), limits = c(1,12), breaks = c(seq(1,12,1))) +
  ggprism::theme_prism() +
  labs(x = "Day", y = 'Bacillota / Bacteriodota Ratio') +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    
    strip.background = element_rect(fill = NA),
    strip.text = element_text(color = "black", face = "bold"),
    legend.text = element_text(color = "black", face = "bold", size = 12),
    legend.title = element_text(color = "black", face = "bold", size = 14),
    legend.position = "right"
  )


phyla <- (control_phyla |
  sbp_phyla) + 
  plot_layout(guides = 'collect')

 
phyla / f_b_ratio +  plot_annotation(tag_levels = 'A') +
  theme(
    legend.justification = 'left'
  )


ggsave('plots/phyla_rel_abund_plot.png', width = 9, height = 6)


rel_abund_table %>% 
  filter(taxon %in% c('Bacteroidota', 'Bacillota')) %>% 
  select(sampleid, taxon, rel_abund, day, horse, treatment) %>% 
  pivot_wider(names_from = 'taxon', 
              values_from = 'rel_abund') %>% 
  mutate(f_b_ratio = Bacillota / Bacteroidota) %>% 
  ungroup() %>% 
  rstatix::anova_test(f_b_ratio ~ treatment * day)

rel_abund_table %>%
  filter(taxon %in% dominant_taxa$taxon) %>% 
  group_by(treatment, taxon) %>% 
  rstatix::anova_test(rel_abund ~ day) 
  
