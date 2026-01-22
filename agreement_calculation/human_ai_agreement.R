library(readxl)
library(writexl)
library(ggplot2)

source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")

root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/gpu outputs/"
plot_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/plots/"
responses_human_all <- read_excel(paste(root_folder, "responses_human_final.xlsx", sep=""))
responses_dieticians <- read_excel(paste(root_folder, "dieticians_all_final.xlsx", sep=""))

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))

# change to only use the images from dieticians
# gemma <- gemma %>%
#   filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)
# gpt <- gpt %>%
#   filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)
# pixtral <- pixtral %>%
#   filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)
# qwen <- qwen %>%
#   filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)

models <- list(Gemma = gemma, Pixtral = pixtral, GPT = gpt, Qwen = qwen)
models <- list(GPT = gpt)


########## COMPARE HUMANS WITH THE AIS ##########

# ====== SINGLE-CHOICE COLUMNS ======
# COMPARE EVERYTHING TOGETHER

# without the dieticians
comparison_df <- compare_all_raters(responses_human_all, models)

# with the dieticians
comparison_df_diet <- compare_all_raters(responses_human_all, models, responses_dieticians)

# all together
comparison_single_all <- comparison_df_diet %>%
  filter(str_ends(rater2, ".*_(diet[1-3]|dietcons)$") | str_ends(rater1, ".*_(diet[1-3]|dietcons)$")) %>%
  rbind(comparison_df) %>% arrange(question)

# re-load the data without rerunning
#comparison_single_all <- read_excel(paste(plot_folder, "agreement_single_all.xlsx", sep=""))

plot_df <- comparison_single_all %>%
  pivot_longer(cols = c(kappa, prop_agreement, gwet_coeff), names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater2),
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
  ggplot(aes(x = rater1, y = rater2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 1.9) +
    facet_grid(metric ~ question, scales = "free", space = "free") +
    scale_fill_gradient2(
      low = "red", mid = "yellow", high = "darkgreen",
      midpoint = 0.5, limits = c(0, 1), name = "Agreement"
    ) +
    labs(
      title = "Agreement Across All Raters (Single-Option)",
      x = "Rater 1", y = "Rater 2"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = -90),
      panel.spacing = unit(0.5, "lines")
    )

ggsave(paste(plot_folder, "single_mixed_paper.jpg", sep = ""), width = 10, height = 5)


comparison_single_all %>%
  mutate(
    across(where(is.numeric), ~round(.x, 2)),
    rater1 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater2),
    kappa_conf = paste0("(", kappa_conf_low, ",", kappa_conf_upp, ")", sep="")) %>%
  select(-c("kappa_conf_low", "kappa_conf_upp")) %>%
  # save to excel
  #write_xlsx(paste(root_folder, "plots/agreement_single_all.xlsx", sep=""))
  DT::datatable(options = list(scrollX = TRUE))


# ====== MULTI-LABEL COLUMNS ======

# without the dieticians
res_ml <- compare_multilabel_human_ai(responses_human_all, models, consensus = FALSE,
                                      multi_label_vars = multi_label_vars, filter_other = TRUE)

# with the dieticians
res_ml_diet <- compare_multilabel_human_ai(responses_human_all, models, consensus = FALSE, 
                                           responses_dieticians, multi_label_vars = multi_label_vars, filter_other = TRUE)

# all together
res_ml_all <- res_ml_diet %>% 
  filter(str_ends(rater2, ".*_(diet[1-3]|dietcons)$") | str_ends(rater1, ".*_(diet[1-3]|dietcons)$")) %>%
  rbind(res_ml) %>% arrange(question)

# re-load the data without rerunning
#res_ml_all <- read_excel(paste(plot_folder, "agreement_multiple_all.xlsx", sep=""))

plot_df_ml <- res_ml_all %>%
  select(-kripp_alpha_unweighted) %>%
  pivot_longer(cols = c(jaccard, kripp_alpha_binary, kripp_alpha_masi), 
               names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater2),
    metric = recode(metric, jaccard = "Jaccard Similarity",
                    kripp_alpha_binary = "Krippendorff’s α (Binary)",
                    kripp_alpha_masi = "Krippendorff’s α (MASI)")
  )


plot_df_sym <- plot_df_ml %>%
  bind_rows(
    plot_df_ml %>%
      rename(rater1 = rater2, rater2 = rater1)
  ) %>%
  filter(metric != "Krippendorff’s α (Binary)")

ggplot(plot_df_sym, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 1.9) +
  facet_grid(metric ~ question, scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  labs(
    title = "Agreement Across All Raters (Multi-Option)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(plot_folder, "multiple_mixed_paper.jpg", sep=""), width = 10, height = 5, dpi = 300)


res_ml_all %>%
  mutate(
    across(where(is.numeric), ~round(.x, 2)),
    rater1 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater2)) %>%
  # save to excel
  #write_xlsx(paste(root_folder, "plots/agreement_multiple_all.xlsx", sep=""))
  DT::datatable(options = list(scrollX = TRUE))

# Monte-carlo simulations for the agreement metrics can be found in monte_regr.R




# ======= ANALYZE MISSED LABELS =======
# 1. which labels are most missed overall? (pct_all_failed) - ambiguity in visual/textual cues.
# 2. which model misses more?
# by label for single columns 
disagreed_by_label_single <- lapply(single_choice_vars, function(col) {
  df <- single_disagreement_by_label(models = models, column = col, dieticians = responses_dieticians)
  df$question <- col
  df
}) %>% bind_rows()

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
    #geom_text(aes(label = round(pct, 3)), size = 4) +
    geom_text(aes(label = paste(round(pct, 2), " (", n, ")", sep="")), size = 4) +
    scale_fill_gradient2(
      low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
    labs(title = "Missed Consensus Labels by Model and Label (Alcohol)",
         x = "Model", y = "Label Code", fill = "% Missed") +
    theme_minimal()

ggsave(paste(plot_folder, "ai_missed_alcohol.png", sep = ""), width = 10, height = 6)


# by label for multi-label
disagreed_by_label_multiple <- lapply(multi_label_vars, function(col) {
  df <- multi_disagreement_by_label(models = models, column = col, humans = responses_human_all)
  df$question <- col
  df
}) %>% bind_rows()

plot_df <- disagreed_by_label_multiple %>%
  left_join(label_df, by = join_by(label == code, question == category)) %>%
  filter(question == "prem_offer") %>%
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
      low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1)) +
    labs(title = "Missed Consensus Labels by Model and Label (WHO Categories)",
         x = "Model", y = "Label Code", fill = "% Missed") +
    theme_minimal()

ggsave(paste(plot_folder, "ai_missed_who_cat.png", sep = ""), width = 10, height = 6)




# ======= AGREEMENT BY LANGUAGE =======
responses_human_nl <- gpt %>% select(img_id, language) %>%
  inner_join(responses_human_all, by = "img_id") %>%
  filter(language == "NL")

responses_human_fr <- gpt %>% select(img_id, language) %>%
  inner_join(responses_human_all, by = "img_id") %>%
  filter(language == "FR")

responses_dieticians_nl <- gpt %>% select(img_id, language) %>%
  inner_join(responses_dieticians, by = "img_id") %>%
  filter(language == "NL")

responses_dieticians_fr <- gpt %>% select(img_id, language) %>%
  inner_join(responses_dieticians, by = "img_id") %>%
  filter(language == "FR")

# ===== single-choice columns
# without the dieticians
comparison_df_nl <- compare_all_raters(responses_human_nl, models) %>% mutate(language = "NL")
comparison_df_fr <- compare_all_raters(responses_human_fr, models) %>% mutate(language = "FR")

# with the dieticians
comparison_df_diet_nl <- compare_all_raters(responses_human_nl, models, responses_dieticians_nl) %>% 
  mutate(language = "NL")
comparison_df_diet_fr <- compare_all_raters(responses_human_fr, models, responses_dieticians_fr) %>% 
  mutate(language = "FR")

# calculate the difference between agreement for NL and FR (delta)
comparison_delta <- comparison_df_diet_nl %>%
  rename(gwet_coeff_nl = gwet_coeff, prop_agreement_nl = prop_agreement, gwet_ci_nl = gwet_ci) %>%
  inner_join(comparison_df_diet_fr %>% rename(gwet_coeff_fr = gwet_coeff, prop_agreement_fr = prop_agreement, gwet_ci_fr = gwet_ci),
             by = c("rater1", "rater2", "question")) %>%
  mutate(delta_gwet = gwet_coeff_nl - gwet_coeff_fr,
         delta_agreement = prop_agreement_nl - prop_agreement_fr,
         gwet_ci_low_nl = as.numeric(sub("\\(([^,]+),.*", "\\1", gwet_ci_nl)),
         gwet_ci_upp_nl = as.numeric(sub(".*,(.+)\\)", "\\1", gwet_ci_nl)),
         gwet_ci_low_fr = as.numeric(sub("\\(([^,]+),.*", "\\1", gwet_ci_fr)),
         gwet_ci_upp_fr = as.numeric(sub(".*,(.+)\\)", "\\1", gwet_ci_fr))) %>%
  filter(
    # keep pairs that involve dieticians, consensus, or AI
    grepl("diet", rater1) | #grepl("diet", rater2) |
      grepl("cons", rater1) | #grepl("cons", rater2) |
      grepl("gpt|qwen|pixtral|gemma", rater1)#| grepl("gpt|qwen|pixtral|gemma", rater2)
  ) %>%
  rowwise() %>%
  mutate(
    se_nl = ci_to_se(gwet_ci_upp_nl, gwet_ci_low_nl),
    se_fr = ci_to_se(gwet_ci_upp_fr, gwet_ci_low_fr),
    # Two-tailed p-value
    z = z_test_from_ests(gwet_coeff_nl, se_nl, gwet_coeff_fr, se_fr)["z"],
    p_value = z_test_from_ests(gwet_coeff_nl, se_nl, gwet_coeff_fr, se_fr)["p"],
    p_bonf = z_test_from_ests(gwet_coeff_nl, se_nl, gwet_coeff_fr, se_fr)["p_bonf"],
    significant = p_value < 0.05,
    across(where(is.numeric), ~round(.x, 2)),
  ) %>%
  ungroup() %>%
  mutate(
    rater1 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater2))

comparison_delta %>%
  filter(rater2 == "GPT" | rater2 == "Qwen") %>%
  ggplot(aes(x = interaction(rater1, rater2), y = delta_gwet, fill = delta_gwet > 0)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(title = "Difference in agreement NL vs FR (Single-Choice)", y = "Δ Agreement (NL - FR)", x = "Rater pair") +
    scale_fill_manual(values = c("red", "green"), guide = "none") +
    facet_wrap(~question, scales = "free_y")
# green bar - agreement was higher in NL ads
#ggsave(paste(plot_folder, "delta_single.jpg", sep = ""), width = 11, height = 4)

view(comparison_delta)


# ===== multi-label columns
res_ml_nl <- compare_multilabel_human_ai(responses_human_nl, models, consensus = FALSE,
                                      multi_label_vars = multi_label_vars, filter_other = TRUE) %>% 
  mutate(language = "NL")

res_ml_fr <- compare_multilabel_human_ai(responses_human_fr, models, consensus = FALSE,
                                         multi_label_vars = multi_label_vars, filter_other = TRUE) %>% 
  mutate(language = "FR")

# with the dieticians
res_ml_diet_nl <- compare_multilabel_human_ai(responses_human_nl, models, consensus = FALSE, 
                                           responses_dieticians_nl, multi_label_vars = multi_label_vars, filter_other = TRUE) %>% 
  mutate(language = "NL")

res_ml_diet_fr <- compare_multilabel_human_ai(responses_human_fr, models, consensus = FALSE, 
                                              responses_dieticians_fr, multi_label_vars = multi_label_vars, filter_other = TRUE) %>% 
  mutate(language = "FR")


res_ml_delta <- res_ml_diet_nl %>%
  rename(kripp_alpha_masi_nl = kripp_alpha_masi, jaccard_nl = jaccard, alpha_ci_nl = kripp_alpha_masi_ci) %>%
  inner_join(res_ml_diet_fr %>% rename(kripp_alpha_masi_fr = kripp_alpha_masi, jaccard_fr = jaccard, alpha_ci_fr = kripp_alpha_masi_ci),
             by = c("rater1", "rater2", "question")) %>%
  mutate(delta_masi = kripp_alpha_masi_nl - kripp_alpha_masi_fr,
         delta_jaccard = jaccard_nl - jaccard_fr,
         alpha_ci_low_nl = as.numeric(sub("\\(([^,]+),.*", "\\1", alpha_ci_nl)),
         alpha_ci_upp_nl = as.numeric(sub(".*,(.+)\\)", "\\1", alpha_ci_nl)),
         alpha_ci_low_fr = as.numeric(sub("\\(([^,]+),.*", "\\1", alpha_ci_fr)),
         alpha_ci_upp_fr = as.numeric(sub(".*,(.+)\\)", "\\1", alpha_ci_fr))) %>%
  filter(
    # keep pairs that involve dieticians, consensus, or AI
    grepl("diet", rater1) | #grepl("diet", rater2) |
      grepl("cons", rater1) | #grepl("cons", rater2) |
      grepl("gpt|qwen|pixtral|gemma", rater1)#| grepl("gpt|qwen|pixtral|gemma", rater2)
  ) %>%
  rowwise() %>%
  mutate(
    se_nl = ci_to_se(alpha_ci_low_nl, alpha_ci_upp_nl),
    se_fr = ci_to_se(alpha_ci_low_fr, alpha_ci_upp_fr),
    # Two-tailed p-value
    z = z_test_from_ests(kripp_alpha_masi_nl, se_nl, kripp_alpha_masi_fr, se_fr)["z"],
    p_value = z_test_from_ests(kripp_alpha_masi_nl, se_nl, kripp_alpha_masi_fr, se_fr)["p"],
    p_bonf = z_test_from_ests(kripp_alpha_masi_nl, se_nl, kripp_alpha_masi_fr, se_fr)["p_bonf"],
    significant = p_value < 0.05,
    across(where(is.numeric), ~round(.x, 2)),
  ) %>%
  ungroup() %>%
  mutate(
    across(where(is.numeric), ~round(.x, 2)),
    rater1 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                    diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                    cons   = "Consensus", dietcons = "Consensus (D)", .default = rater2))

res_ml_delta %>%
  filter(rater2 == "GPT" | rater2 == "Qwen") %>%
  ggplot(aes(x = interaction(rater1, rater2), y = delta_masi, fill = delta_masi > 0)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(title = "Difference in agreement NL vs FR (Multi-Label)", y = "Δ Agreement (NL - FR)", x = "Rater pair") +
    scale_fill_manual(values = c("red", "green"), guide = "none") +
    facet_wrap(~question, scales = "free_y")

#ggsave(paste(root_folder, "plots/delta_multiple.png", sep=""), width = 11, height = 4)

# need to remove the "Other" labels in prem offers and marketing str
view(comparison_delta)
view(res_ml_delta)

# create just one single figure with the most interesting results
delta_all <- comparison_delta %>% 
  filter(question %!in% c("alcohol", "new_type_ad")) %>%
  select(question, rater1, rater2, delta = delta_gwet, p_value) %>%
  rbind(
    res_ml_delta %>%
      select(question, rater1, rater2, delta = delta_masi, p_value)
  )

library(ggh4x)

delta_all %>%
  filter(rater2 == "GPT" | rater2 == "Qwen") %>%
  ggplot(aes(x = interaction(rater1, rater2), y = delta, fill = delta > 0)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(title = "Difference in agreement NL vs FR", y = "Δ Agreement (NL - FR)", x = "Rater pair") +
    scale_fill_manual(values = c("red", "green"), guide = "none") +
    facet_wrap2(~question, ncol = 4, axes = "x") +
    theme(
      strip.text = element_text(size = 10, face = "bold", 
                                margin = margin(b = 5, t = 5)),
      strip.background = element_rect(fill = "gray90", color = "gray60", linewidth = 0.5),
      axis.text.y = element_text(size = 8),
      panel.spacing.x = unit(0.8, "lines"),  # Increased spacing between panels
      panel.border = element_rect(color = "gray70", fill = NA, size = 0.5),  # Add panel borders
      panel.background = element_rect(fill = "white", color = "gray80")
    )

# green bar - agreement was higher in NL ads
ggsave(paste(plot_folder, "delta_languages.jpg", sep = ""), width = 10, height = 5)


# ======= MONTE-CARLO SIMULATION FOR BOOTSTRAP AGREEMENT METRICS =======

library(purrr)
library(tibble)
library(irr)
library(readxl)
library(patchwork)

source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/monte_regr.R")

# # SINGLE-CHOICE QUESTIONS
# results_single <- lapply(single_choice_vars, function(question) {
#   print(paste("Processing question:", question))
#   bootstrap_agreement(models = list(GPT = gpt), responses_human_all, question, responses_dieticians, 1000, "single")
# })

# names(results_single) <- single_choice_vars

# wb <- createWorkbook()
# for (question in names(results_single)) {
#   addWorksheet(wb, question)
#   writeData(wb, question, results_single[[question]]$sims)
# }
# saveWorkbook(wb, "gpu outputs/plots/bootstrap_agreement_single_choice.xlsx", overwrite = TRUE)

# plot_gwet_distribution(results_single$alcohol$sims, "Alcohol")
# plot_prop_distribution(results_single$alcohol$sims, "Alcohol")


# # run one by one because it takes a while
# temp3 <- bootstrap_agreement(models = list(GPT = gpt), responses_human_all, "marketing_str", responses_dieticians, 1000, "multi")
# # results_multi <- list(temp, temp2, temp3)

# # MULTI-CHOICE QUESTIONS
# results_multi <- lapply(multi_label_vars, function(question) {
#   print(paste("Processing question:", question))
#   bootstrap_agreement(models = list(GPT = gpt), responses_human_all, question, responses_dieticians, 1000, "multi")
# })

# names(results_multi) <- multi_label_vars

# wb <- createWorkbook()
# for (question in names(results_multi)) {
#   addWorksheet(wb, question)
#   writeData(wb, question, results_multi[[question]])
# }
# saveWorkbook(wb, "gpu outputs/plots/bootstrap_agreement_multi_choice.xlsx", overwrite = TRUE)

# plot_masi_distribution(results_multi$prem_offer$sims, "Premium Offers")
# plot_jacc_distribution(results_multi$prem_offer$sims, "Premium Offers")


# read all results single and multi from the excel files (for later use)
results_single <- setNames(lapply(single_choice_vars, function(question) {
  read_excel(path = file.path(plot_folder, "bootstrap_agreement_single_choice.xlsx"), sheet = question)
}), single_choice_vars)

results_multi <- setNames(lapply(multi_label_vars, function(question) {
  read_excel(path = file.path(plot_folder, "bootstrap_agreement_multi_choice.xlsx"), sheet = question)
}), multi_label_vars)


plot_combined_distributions(results_single, single_choice_vars, results_multi, multi_label_vars, save = TRUE)


# ======== OPTION BIAS WITHIN EACH QUESTION (z-tests) + LABEL-LEVEL FIXED EFFECTS REGRESSION ========
# quantify for each question and label how much each model over/under-selects an option relative to human consensus
source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/monte_regr.R")

bias_prem <- compute_option_bias("prem_offer", models, responses_human_all) %>%
    left_join(label_df %>% filter(category == "prem_offer") %>% select(code, text_label), by = c("label" = "code")) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))

bias_market <- compute_option_bias("marketing_str", models, responses_human_all) %>%
    left_join(label_df %>% filter(category == "marketing_str") %>% select(code, text_label), by = c("label" = "code")) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))

bias_who <- compute_option_bias("who_cat_clean", models, responses_dieticians) %>%
    left_join(label_df %>% filter(category == "who_cat_clean") %>% select(code, text_label), by = c("label" = "code")) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))

# collect the full range of bias
all_values <- c(bias_market$bias, bias_prem$bias, bias_who$bias)
value_range <- range(all_values, na.rm = TRUE)

# create individual plots
p1_prem <- ggplot(bias_prem, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 2.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias", limits = value_range) +
    labs(subtitle = "Premium Offers",
         x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))

p2_market <- ggplot(bias_market, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 2.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias", limits = value_range) +
    labs(subtitle = "Marketing Strategies",
         x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))

p3_who <- ggplot(bias_who, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 2.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias", limits = value_range) +
    labs(subtitle = "WHO Food Categories", x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))

# combine plots with proper layout and single legend
combined_bias_plot <- (p1_prem + p2_market + p3_who) +
    plot_layout(widths = c(1, 1, 1.5), guides = "collect") +
    plot_annotation(
      title = "Label-level bias (z-test) across multi-option question categories",
      tag_levels = 'A'
    ) &
    theme(plot.tag = element_text(face = 'bold', size = 10),
          legend.position = "right")

ggsave(paste(plot_folder, "bias_combined_multi.jpg", sep = ""), plot = combined_bias_plot, width = 10, height = 5, dpi = 350)


bias_alc <- compute_option_bias("alcohol", models, responses_dieticians) %>%
    left_join(label_df %>% filter(category == "alcohol") %>% select(code, text_label), by = c("label" = "code")) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))

bias_targ <- compute_option_bias("target_group", models, responses_dieticians) %>%
    left_join(label_df %>% filter(category == "target_group") %>% select(code, text_label), by = c("label" = "code")) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))

bias_type <- compute_option_bias("new_type_ad", models, responses_dieticians) %>%
    left_join(label_df %>% filter(category == "new_type_ad") %>% select(code, text_label), by = c("label" = "code")) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))

# collect the full range of bias
all_values <- c(bias_alc$bias, bias_targ$bias, bias_type$bias)
value_range <- range(all_values, na.rm = TRUE)

# create individual plots
p1_alc <- ggplot(bias_alc, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 2.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias", limits = value_range) +
    labs(subtitle = "Alcohol",
         x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))

p2_target <- ggplot(bias_targ, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 2.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias", limits = value_range) +
    labs(subtitle = "Target Group",
         x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))

p3_type <- ggplot(bias_type, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 2.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias", limits = value_range) +
    labs(subtitle = "Ad Type", x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7))

# combine plots with proper layout and single legend (vertical arrangement)
combined_bias_plot <- (p1_alc / p2_target / p3_type) +
    plot_layout(heights = c(0.5, 0.5, 1.5), guides = "collect") +
    plot_annotation(
      title = "Label-level bias (z-test) across single-option question categories",
      tag_levels = 'A'
    ) &
    theme(plot.tag = element_text(face = 'bold', size = 10),
          legend.position = "right")

ggsave(paste(plot_folder, "bias_combined_single.jpg", sep = ""), plot = combined_bias_plot, width = 6, height = 7, dpi = 350)
