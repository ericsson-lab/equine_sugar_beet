library(tidyverse)
library(cowplot)

importance <- read_tsv("data/q2_longitudinal_files/output_genus3/importance.tsv")

importance_plot <- importance %>% 
  mutate(feature = str_replace_all(feature, "_", " ")) %>%  
  ggplot(aes(x = reorder(feature, importance), y = importance)) +
  geom_col(fill = "gray70") +
  coord_flip() +
  scale_y_continuous(position = "right", expand = c(0,0), labels = scales::percent,
                     limits = c(0, 0.08)) +
  theme_classic() +
  labs(y = "Importance") +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", face = "bold")
  )


rel_abund <- read_tsv("data/q2_longitudinal_files/output_genus3/rel_abund.tsv")

rel_abund <- rel_abund[-1,]


plot_feature <- function(y_value){
  rel_abund %>% 
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
      axis.text = element_text(color = "black", face = "bold"),
      axis.title = element_text(color = "black", face = "bold"),
      
      strip.background = element_rect(fill = NA),
      strip.text = element_text(color = "black", face = "bold"),
      legend.text = element_text(color = "black", face = "bold", size = 12),
      legend.title = element_text(color = "black", face = "bold", size = 12),
      legend.position = "bottom"
    )
}

plot_feature(`gir-aah93h0`)

sutterella <- plot_feature(Sutterella) + theme(legend.position = "none")
cellulosilyticum <- plot_feature(Cellulosilyticum) + theme(legend.position = "none")
alloprevotella <- plot_feature(Alloprevotella)

colidextribacter <- plot_feature(Colidextribacter) + theme(legend.position = "none")
moryella <- plot_feature(Moryella) + theme(legend.position = "none")
weissella <- plot_feature(Weissella) 

leg <- get_legend(weissella)
plots <- plot_grid(colidextribacter, moryella, 
          weissella + theme(legend.position = 'none'),
          nrow = 3, rel_heights = c(0.3, 0.3, 0.3), labels = 'AUTO',
          label_x = -0.01, label_y = 1) +
  theme(plot.tag = element_text(face = "bold", color = "black", size = 14))

plot_grid(plots, leg, rel_heights = c(0.9, 0.1), nrow = 2)
ggsave('plots/sf_rel_abund_plots.png',
       width = 4, height = 7,
       units = c('in'), 
       bg = 'white')

net_avg_change <- rel_abund %>% 
  select(-c(id, group, title)) %>% 
  pivot_longer(-c(horse, treatment, day),
               names_to = "taxon",
               values_to = "rel_abund") %>% 
  group_by(horse, treatment, taxon) %>% 
  arrange(taxon,as.integer(day)) %>% 
  summarize(change = last(as.double(rel_abund)) - 
           first(as.double(rel_abund))) %>% 
  group_by(treatment, taxon) %>% 
  summarize(avg_change = mean(change)) %>% 
  inner_join(., importance %>% rename(taxon = "feature"), by = "taxon") %>% 
  pivot_wider(names_from = "treatment", values_from = "avg_change") %>% 
  ggplot(aes(x = reorder(taxon, importance))) + 
  geom_hline(aes(yintercept = 0)) +
  geom_hline(yintercept = c(-0.04, -0.02, 0.02, 0.04, 0.06), linetype = 2, alpha = 0.3) +
  geom_segment(aes(y = Control, yend = `Sugar Beet`,
                   xend = reorder(taxon, importance)),
               size = 3, color = "gray70", alpha = 0.5) +
  geom_point(aes(y = Control), color = "red", size = 3) +
  geom_point(aes(y = `Sugar Beet`), color = "dodgerblue", size = 3) +
  scale_y_continuous(position = "right", expand = c(0,0), labels = scales::percent,
                     limits = c(-0.045, 0.065)) +
  coord_flip() +
  labs(y = "Net Change (%)") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(color = "black", face = "bold"),
    axis.title.x = element_text(color = "black", face = "bold"),
    axis.line.y = element_blank()
  )
net_avg_change

  
rel_abund %>% 
  select(-c(id, group, title)) %>% 
  pivot_longer(-c(horse, treatment, day),
               names_to = "taxon",
               values_to = "rel_abund") %>% 
  group_by(horse, treatment, taxon) %>% 
  arrange(taxon,as.integer(day)) %>% 
  summarize(change = last(as.double(rel_abund)) - 
              first(as.double(rel_abund))) %>% 
  group_by(treatment, taxon) %>% 
  # summarize(avg_change = mean(change)) %>% 
  inner_join(., importance %>% rename(taxon = "feature"), by = "taxon") %>% 
  # pivot_wider(names_from = "treatment", values_from = "avg_change") %>% 
  filter(taxon == "Alloprevotella")
rel_abund %>% 
  select(-c(id, group, title)) %>% 
  pivot_longer(-c(horse, treatment, day),
               names_to = "taxon",
               values_to = "rel_abund") %>% 
  group_by(horse, treatment, taxon) %>% 
  arrange(taxon,as.integer(day)) %>% 
  mutate(change = last(as.double(rel_abund)) -
              first(as.double(rel_abund))) %>%
  group_by(treatment, taxon) %>% 
  arrange(horse) %>% 
  filter(taxon == "Alloprevotella") %>% 
  print(n = 144)

  print(n = 22)
  


r_leg <- get_legend(alloprevotella)

r <- plot_grid(sutterella, cellulosilyticum, alloprevotella + theme(legend.position = "none"),
          nrow = 3, rel_heights = c(0.33, 0.33, 0.33))
r <- plot_grid(r, r_leg, nrow = 2, rel_heights = c(0.95, 0.05))
l <- plot_grid(importance_plot, net_avg_change, rel_widths = c(0.7, 0.3))
plot_grid(l, r,
          ncol = 2,
          rel_widths = c(0.6, 0.4))

ggsave("plots/genus_longitudinal.png",
       width = 9,
       height = 6,
       units = c('in'),
       bg = "white")




