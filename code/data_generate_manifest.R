library(tidyverse)

metadata <- read_tsv("data/metadata.tsv")
sampleids <- metadata$sampleid %>% as.array()

sink("data/manifest.txt")

cat("sample-id\tdirection\n")

for (i in 1:length(sampleids)) {
  cat(paste0(noquote(sampleids[i])),'\tforward\n')
  cat(paste0(noquote(sampleids[i])),'\treverse\n')
}

sink()
