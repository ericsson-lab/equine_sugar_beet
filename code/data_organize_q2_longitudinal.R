source("code/load_data.R")

metadata %>% 
  write_tsv("data/q2_longitudinal_files/metadata.tsv")

table %>%
  write_tsv("data/q2_longitudinal_files/table_filt.tsv")

filter_table <- function(taxa_level) {
  taxa_filt <- taxonomy %>% filter(level == taxa_level)
  
  table %>% 
    pivot_longer(-featureid, names_to = "sampleid",
                 values_to = "count") %>% 
    inner_join(taxa_filt, by = "featureid") %>% 
    group_by(sampleid) %>% 
    select(sampleid, taxon, count)
}

genus_table <- filter_table(taxa_level = "Genus")

genus_table %>% 
  pivot_wider(names_from = "sampleid", values_from = "count", values_fn = sum) %>% 
  select(taxon, all_of(beet_samples$sampleid)) %>% 
  write_tsv("data/q2_longitudinal_files/table_filt_genus.tsv")
