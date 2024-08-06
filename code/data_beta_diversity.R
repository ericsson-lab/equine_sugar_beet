{
  library(vegan)
  library(ape)
}

source("code/load_data.R")

permutations = 9999

generate_dist <- function(distance = "bray"){
  
  table_t <- table %>% 
    # select(featureid, all_of(ids)) %>% 
    column_to_rownames(var = "featureid") %>% 
    t()
  
  table.transformed <- table_t^1/4
  
  dist <- vegan::vegdist(table.transformed, method= distance)
}

generate_pcoa <- function(dist) {
  pcoa_res <- ape::pcoa(dist, correction = "cailliez") 

  p_var <- (pcoa_res$values$Eigenvalues/pcoa_res$trace)*100
  
  pcoa_vectors <- pcoa_res$vectors %>% 
    as_tibble(rownames = "sampleid") %>% 
    select(sampleid, Axis.1, Axis.2, Axis.3, Axis.4, Axis.5)
  
  colnames(pcoa_vectors) <- c("sampleid", "PCo1", "PCo2", "PCo3", "PCo4", "PCo5")
  
  pcoa.metadata <- inner_join(pcoa_vectors, metadata, by = "sampleid") 
  # adonis.output <- adonis(dist ~ treatment,
  #                         permutations = permutations,
  #                         data = metadata)
  # 
  # f_value <- round(adonis.output$aov.tab$F.Model[1],2)
  # 
  # p_value <- ifelse(adonis.output$aov.tab$`Pr(>F)`[1] < 0.001,
  #                   "< 0.001",
  #                   adonis.output$aov.tab$`Pr(>F)`[1])
  
  output <- list(pcoa.metadata, p_var
                 # f_value, p_value
                 )
  return(output)
}


