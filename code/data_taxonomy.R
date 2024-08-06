library(RColorBrewer)

source("code/load_data.R")


filter_table <- function(taxa_level) {
  taxa_filt <- taxonomy %>% filter(level == taxa_level)
  
  table %>% 
    pivot_longer(-featureid, names_to = "sampleid",
                 values_to = "count") %>% 
    inner_join(taxa_filt, by = "featureid") %>% 
    group_by(sampleid) %>% 
    mutate(rel_abund = count/sum(count)) %>% 
    inner_join(., metadata, by = "sampleid")
}

genus_table <- filter_table(taxa_level = "Genus")



genus_rel_abund <- genus_table %>% 
  group_by(treatment, day, taxon, sampleid) %>% 
  summarise(rel_abund = sum(rel_abund), .groups = "drop") %>% 
  group_by(treatment, day, taxon) %>% 
  summarize(mean_rel_abund = mean(rel_abund), .groups = "drop") 
  # select(Family, mean_rel_abund, group, samplelocation) 

taxon_pool <- genus_rel_abund %>% 
  group_by(taxon) %>% 
  summarise(pool = max(mean_rel_abund) < 0.03, .groups = 'drop') 
table(taxon_pool)


data_to_plot <- inner_join(genus_rel_abund, taxon_pool, by = "taxon") %>% 
  mutate(taxon = if_else(pool, "Other", taxon)) %>% 
  group_by(treatment,day, taxon) %>% 
  summarise(mean_rel_abund = sum(mean_rel_abund)) %>% 
  filter(day <= 12)

taxon_list <- unique(data_to_plot$taxon)
taxon_list <- taxon_list[taxon_list != "Other"]

data_to_plot$taxon <- factor(data_to_plot$taxon, 
                              levels = c(taxon_list, "Other"))

palette_length <- length(taxon_list)
palette_fill <- colorRampPalette(brewer.pal(11, "Spectral"))(palette_length)

data_to_plot %>%
  group_by(day, taxon) %>% 
  ggplot(aes(x = day, y = mean_rel_abund, fill= taxon, group = taxon)) +
  geom_area() +
  geom_vline(aes(xintercept = 6), color = "white", linetype = 2) +
  scale_fill_manual(values = c(palette_fill, "#606060")) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~treatment) +
  theme_classic()

ggsave("plots/taxa_genus_5p.png",
       width = 12,
       height = 8,
       units = c("in"),
       bg = "white")


