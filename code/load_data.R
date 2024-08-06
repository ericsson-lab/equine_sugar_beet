{
  library(tidyverse)
  library(patchwork)
}
# Pull metadata
metadata <- read_tsv("data/metadata.txt") %>% 
  mutate(treatment = case_when(group == "ctl" ~ "Control", 
                               TRUE ~ "Sugar Beet"))

# Load original table rarefied to 24190 features per sample
table_orig <- read_tsv("data/core-metrics-results_24190/rarefied-feature-table_taxa.tsv",
                  skip = 1) %>% 
  rename(featureid = `#OTU ID`,
         taxon = "Taxon") %>% 
  select(featureid, taxon, all_of(metadata$sampleid))

# Pull taxonomy and separate into levels
taxonomy <- table_orig %>% 
  select(featureid, taxon) %>% 
  separate(taxon, 
           into = c("Kingdom", "Phylum", "Class", 
                    "Order", "Family", "Genus"),
           sep = "; ") %>% 
  pivot_longer(-featureid, 
               names_to = "level", 
               values_to = "taxon") %>% 
  mutate(taxon = str_sub(taxon, 4, length(taxon))) %>% 
  group_by(featureid) %>% 
  fill(taxon)

# pull only table 
table <- table_orig %>% 
  select(-taxon)


