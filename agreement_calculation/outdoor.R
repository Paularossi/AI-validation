
library(readxl)
library(writexl)

source("C:/Users/P70090005/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")

root_folder <- "C:/Users/P70090005/Desktop/phd/AI-validation/data/outdoor ads/"
responses_diet_all <- read_excel(paste(root_folder, "dieticians_outdoor_all_final.xlsx", sep="")) %>%
  mutate(img_id = gsub("\\.jpg$", "", img_id)) %>% arrange(img_id)
gpt <- read_excel(paste(root_folder, "gpt_all_outdoor.xlsx", sep="")) %>% arrange(img_id)
#pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))

gpt <- gpt %>%
  mutate(brand = map_chr(brand, clean_brand))

models <- list(gpt = gpt)


# ===== SINGLE CHOICE =====
results_kappa <- compare_human_ai_kappa(responses_diet_all, models)
#results_kappa <- compare_human_ai_kappa(responses_human_all, models, ai_mapping, human_mapping)
head(results_kappa)

# unnest the conf int for gwet
results_kappa <- results_kappa %>%
  mutate(gwet_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", gwet_ci)),
         gwet_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", gwet_ci))) 

# pivot to long format for plotting
plot_df <- results_kappa %>%
  mutate(rater = sub("^.+_(diet[1-3]|dietcons)$", "\\1", coder),
         rater = recode(rater,
                        diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                        dietcons = "Consensus (D)")) %>%
  pivot_longer(
    cols = c(prop_agreement, kappa, gwet_coeff),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = recode(metric, prop_agreement = "% Agreement", kappa = "Cohen's Kappa", 
                         gwet_coeff = "Gwet's AC1"))


ggplot(plot_df, aes(x = model, y = rater, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  facet_grid(metric ~ question, scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  labs(
    title = "AI vs. Human Single-Choice Agreement",
    x = "AI Model", y = "Human Rater"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/diet_ai_single.png", sep=""), width = 9, height = 5)


# all together
comparison_df <- compare_all_raters(responses_diet_all, models)

plot_df <- comparison_df %>%
  pivot_longer(cols = c(kappa, prop_agreement, gwet_coeff), names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(diet[1-3]|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(diet[1-3]|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    dietcons = "Consensus (D)", .default = rater2),
    metric = recode(metric, kappa = "Cohen's Kappa", prop_agreement = "% Agreement",
                    gwet_coeff = "Gwet's AC1")
  ) 

plot_df_symmetric <- plot_df %>%
  bind_rows(
    plot_df %>%
      rename(rater1 = rater2, rater2 = rater1)
  ) %>%
  distinct(question, metric, rater1, rater2, .keep_all = TRUE)


plot_df_symmetric %>% 
  filter(metric != "Cohen's Kappa") %>%
  #filter(question == "alcohol") %>%
  ggplot(aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.7) +
  facet_grid(metric ~ question, scales = "free", space = "free") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  labs(
    title = "Agreement Across All Raters (Single-Choice)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_single.png", sep=""), width = 10, height = 5)



# ====== MULTI-LABEL COLUMNS ======
res_ml <- compare_multilabel_human_ai(responses_diet_all, models, consensus = FALSE,
                                      multi_label_vars = multi_label_vars)

plot_df_ml <- res_ml %>%
  select(-kripp_alpha_unweighted) %>%
  pivot_longer(cols = c(jaccard, kripp_alpha_binary, kripp_alpha_masi), 
               names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(diet[1-3]|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(diet[1-3]|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    dietcons = "Consensus (D)", .default = rater2),
    metric = recode(metric, jaccard = "Jaccard Similarity",
                    kripp_alpha_binary = "Kripp. α (Binary)",
                    kripp_alpha_masi = "Kripp. α (MASI)")
  )


plot_df_sym <- plot_df_ml %>%
  bind_rows(
    plot_df_ml %>%
      rename(rater1 = rater2, rater2 = rater1)
  ) %>% 
  filter(metric != "Kripp. α (Binary)")

ggplot(plot_df_sym, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.7) +
  facet_grid(metric ~ question, scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  labs(
    title = "Agreement Across All Raters (Multi-Label)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_multiple.png", sep=""), width = 10, height = 5)



# ===== ANALYSIS BY LABELS =====
# by label for single columns 
disagreed_by_label_single <- lapply(single_choice_vars, function(col) {
  df <- single_disagreement_by_label(models = models, column = col, dieticians = responses_diet_all)
  df$question <- col
  df
}) %>% bind_rows()

# 1. which labels are most missed overall? (pct_all_failed) - ambiguity in visual/textual cues.
# 2. which model misses more?

plot_df <- disagreed_by_label_single %>%
  left_join(label_df, by = join_by(label == code, question == category)) %>%
  filter(question == "new_type_ad") %>%
  select(total, text_label, starts_with("failed_")) %>%
  pivot_longer(
    cols = starts_with("failed_"),
    names_to = c("model", ".value"),
    names_pattern = "failed_(.*)_(n|pct)"
  ) %>%
  mutate(
    model = recode(model,
                   gpt = "GPT",
                   qwen = "Qwen",
                   pixtral = "Pixtral",
                   gemma = "Gemma",
                   all = "All Models")
  ) %>%
  mutate(text_label_n = paste0(text_label, " (", total, ")")) %>% 
  filter(model %!in% c("all_n", "all_pct"))

plot_df %>%
  filter(model != "All Models") %>%
  ggplot(aes(x = model, y = reorder(text_label_n, -pct), fill = pct)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste(round(pct, 2), " (", n, ")", sep="")), size = 4) +
    scale_fill_gradient2(
      low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
    labs(title = "Missed Consensus Labels by Model and Label (Ad Type)",
         x = "Model", y = "Label Code", fill = "% Missed") +
    theme_minimal()

ggsave(paste(root_folder, "plots/ai_missed_ad_type_out.png", sep=""), width = 10, height = 6)


# by label for multi-label
disagreed_by_label_multiple <- lapply(multi_label_vars, function(col) {
  df <- multi_disagreement_by_label(models = models, column = col, dieticians = responses_diet_all)
  df$question <- col
  df
}) %>% bind_rows()


plot_df <- disagreed_by_label_multiple %>%
  left_join(label_df, by = join_by(label == code, question == category)) %>%
  filter(question == "who_cat_clean") %>%
  select(total, text_label, starts_with("failed_")) %>%
  pivot_longer(
    cols = starts_with("failed_"),
    names_to = c("model", ".value"),
    names_pattern = "failed_(.*)_(n|pct)"
  ) %>%
  mutate(
    model = recode(model,
                   gpt = "GPT",
                   qwen = "Qwen",
                   pixtral = "Pixtral",
                   gemma = "Gemma",
                   all = "All Models"),
    text_label_n = paste0(text_label, " (", total, ")")
  )


plot_df %>%
  filter(model != "All Models") %>%
  ggplot(aes(x = model, y = reorder(text_label_n, -pct), fill = pct)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste(round(pct, 2), " (", n, ")", sep="")), size = 4) +
    scale_fill_gradient2(
      low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
    labs(title = "Missed Consensus Labels by Model and Label (WHO Categories)",
         x = "Model", y = "Label Code", fill = "% Missed") +
    theme_minimal()


ggsave(paste(root_folder, "plots/ai_missed_who_cat_out.png", sep=""), width = 8, height = 6)




# ===== ANALYZE THE BRAND QUESTION =====
df_brand <- responses_diet_all %>%
  select(c(img_id, brand_diet1, brand_diet2, brand_diet3)) %>%
  #left_join(gpt[, c("img_id", "brand")], by = "img_id") %>%
  mutate(
    brand_diet1 = map_chr(brand_diet1, clean_brand),
    brand_diet2 = map_chr(brand_diet2, clean_brand),
    brand_diet3 = map_chr(brand_diet3, clean_brand),
  )

# multi_label_consensus and multi_label_consensus_majority are from merging_outputs.R
diet_cols <- paste0("brand_diet", 1:3)
#consensus_col <- paste0("brand", "_dietcons")
#df_brand[[consensus_col]] <- pmap_chr(df_brand[diet_cols], multi_label_consensus)

df_brand <-  df_brand %>%
  mutate(brand_dietcons = pmap_chr(
    list(brand_diet1, brand_diet2, brand_diet3),
    ~ multi_label_consensus_majority(..1, ..2, ..3, threshold = 2)
    )
  )


# all raters together
agreement_brand <- compare_multilabel_human_ai(df_brand, models, consensus = FALSE, multi_label_vars = c("brand"))

cbind(df_brand, gpt$brand)

plot_df_ml <- agreement_brand %>%
  select(-kripp_alpha_unweighted) %>%
  pivot_longer(cols = c(jaccard, kripp_alpha_binary, kripp_alpha_masi), 
               names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(diet[1-3]|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(diet[1-3]|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    dietcons = "Consensus (D)", .default = rater2),
    metric = recode(metric, jaccard = "Jaccard Similarity",
                    kripp_alpha_binary = "Kripp. α (Binary)",
                    kripp_alpha_masi = "Kripp. α (MASI)")
  )


plot_df_sym <- plot_df_ml %>%
  bind_rows(
    plot_df_ml %>%
      rename(rater1 = rater2, rater2 = rater1)
  )

ggplot(plot_df_sym, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.7) +
  facet_grid(metric ~ ., scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  labs(
    title = "Agreement Across All Raters (Brand)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_brand.png", sep=""), width = 8, height = 6)



disagreed_by_label_multiple <- multi_disagreement_by_label(models = list(gpt = gpt), 
                                    column = "brand", dieticians = df_brand)







