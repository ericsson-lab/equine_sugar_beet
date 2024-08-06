{
  library(vegan)
  library(usedist)
  library(EcolUtils)
}

source("code/data_beta_diversity.R")
permutations = 9999

bc_dist <- generate_dist(distance = "bray")

bc_adonis <- adonis(bc_dist ~ treatment * day, data = metadata,
                    permutations = permutations)


bc_adonis$aov.tab %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "group") %>% 
  gt() %>% 
  gtsave("stats/plots/bc_adonis_treatment_day.png")

bc_adonis$aov.tab %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "group") %>% 
  write_tsv("stats/bc_permanova_treatment_day.tsv")

bc_pairwise_adonis <- adonis.pair(bc_dist, 
                                  Factor = interaction(factor(metadata$treatment),
                                                       as.factor(metadata$day)), 
                                  nper = permutations, 
            corr.method = "BH")

bc_pairwise_adonis %>% 
  arrange(P.value) %>% 
  write_tsv("stats/bc_pairwise_permanova_treatment_day.tsv")



