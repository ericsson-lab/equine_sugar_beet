library(rstatix)
library(gt)
library(lme4)

source("code/data_alpha_stats.R")


alpha_values <- alpha_stats %>% 
  inner_join(., metadata, by = "sampleid") %>% 
  mutate(group = case_when(group == "ctl" ~ "Standard",
                           group == "pre" ~ "Standard",
                           group == "post" ~ "SBT")) 

alpha_values %>% 
  pull(shannon) %>%
  shapiro_test(shannon)
##    variable statistic         p
##  1 chao1        0.941 0.0000105


alpha_values %>% 
  anova_test(chao1 ~ treatment * as.factor(day)) %>% 
  gt() %>% 
  gtsave("stats/plots/chao1_anova_res.png")

alpha_values %>% 
  anova_test(chao1 ~ treatment * as.factor(day)) %>% 
  write_tsv("stats/chao1_anova_treatment_day.tsv")

alpha_values %>% 
  tukey_hsd(chao1 ~ treatment * as.factor(day)) %>% 
  gt() %>% 
  gtsave("stats/plots/chao1_tukey_res.png")

alpha_values %>% 
  tukey_hsd(chao1 ~ treatment * as.factor(day),) %>% 
  arrange(p.adj) %>% 
  write_tsv("stats/chao1_tukey_treatment_day.tsv")

alpha_values %>% 
  anova_test(shannon ~ treatment * as.factor(day)) %>% 
  gt() %>% 
  gtsave("stats/plots/shannon_anova_res.png")


alpha_values %>% 
  anova_test(shannon ~ treatment * as.factor(day)) %>% 
  write_tsv("stats/shannon_anova_res.tsv")


library(lmerTest)

alpha_values
aov1 <- aov(chao1~(treatment/group)+Error(horse), data = alpha_values)
summary(aov1)
day <- lmer(chao1~(day) + (1|horse),
              data = alpha_values,REML = F )
treatment <- lmer(chao1~(day * treatment) + (1|horse),
                        data = alpha_values, REML = F)

anova(treatment)
treatment_group <- lmer(chao1~(day * treatment/group) + (1|horse),
              data = alpha_values, REML = F)
