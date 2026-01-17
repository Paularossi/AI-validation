
library(readxl)
library(writexl)

source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")

root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/data/"
plot_folder <- paste(root_folder, "plots/", sep="")
responses_diet_all <- read_excel(paste(root_folder, "dieticians_outdoor_all_final.xlsx", sep="")) %>%
  mutate(img_id = gsub("\\.jpg$", "", img_id)) %>% arrange(img_id)
gpt <- read_excel(paste(root_folder, "gpt_all_outdoor.xlsx", sep="")) %>% arrange(img_id)

gpt <- gpt %>%
  mutate(brand = map_chr(brand, clean_brand))

models <- list(GPT = gpt)


# ===== SINGLE CHOICE =====

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
    title = "Agreement Across All Raters (Single-Option)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_single_out.jpg", sep=""), width = 8, height = 5)



# ====== MULTI-LABEL COLUMNS ======
res_ml <- compare_multilabel_human_ai(responses_diet_all, models, consensus = FALSE,
                                      multi_label_vars = multi_label_vars, filter_other = TRUE)

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
    title = "Agreement Across All Raters (Multi-Option)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_multiple.jpg", sep=""), width = 8, height = 5)



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
                    kripp_alpha_binary = "Krippendorff's α (Binary)",
                    kripp_alpha_masi = "Krippendorff's α (MASI)")
  )


plot_df_sym <- plot_df_ml %>%
  bind_rows(
    plot_df_ml %>%
      rename(rater1 = rater2, rater2 = rater1)
  ) %>% 
  filter(metric != "Krippendorff's α (Binary)")

ggplot(plot_df_sym, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.8) +
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

ggsave(paste(root_folder, "plots/brand.jpg", sep=""), width = 6, height = 5)

# ======== OPTION BIAS WITHIN EACH QUESTION (z-tests) + LABEL-LEVEL FIXED EFFECTS REGRESSION ========
source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")
source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/monte_regr.R")
library(patchwork)
# compute and plot option bias for all questions
for (q in c(single_choice_vars, multi_label_vars)) {
  q_label <- get_question_label(q)
  print(paste("Calculating option bias for question:", q_label))
  bt <- compute_option_bias(q, models, responses_diet_all)
  fe_result <- label_fe_regression(q, models, responses_diet_all, outdoor = TRUE)
  
  # get FE result data first to align label ordering
  fe_heatmap_result <- plot_label_fe_heatmap(fe_result, save = FALSE, outdoor = TRUE)
  fe_data <- fe_heatmap_result$data
  
  # get the exact label order from FE heatmap (which includes reference at top)
  fe_label_levels <- levels(fe_data$label_name)
  
  # get reference label info
  ref_label_code <- get_reference_level(q)
  ref_label_name <- label_df %>%
    filter(category == q, code == ref_label_code) %>%
    pull(text_label)
  
  bt <- bt %>%
    left_join(
      label_df %>% # join with label names from label_df
        filter(category == q) %>%
        select(code, text_label),
      by = c("label" = "code")
    ) %>%
    mutate(sig = case_when(
      is.na(p.value) ~ "",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
      ),
      # use text_label if available, otherwise use code
      label_name = ifelse(is.na(text_label), label, text_label))
  
  expected_labels <- label_df %>%
    filter(category == fe_result$question) %>%
    pull(text_label)
  missing_labels <- if (fe_result$question == "alcohol") 0 else setdiff(expected_labels, unique(bt$label_name))
  
  # add missing labels with NA estimates
  if (length(missing_labels) > 0) {
    for (lbl in missing_labels) {
      bt <- bt %>%
        add_row(model = rep(unique(bt$model), each = 1),
                label = lbl,
                bias = NA,
                p.value = NA,
                sig = "",
                label_name = lbl)
    }
  }

  # use the same y-axis ordering as FE heatmap for alignment
  bt <- bt %>%
    mutate(label_name = factor(label_name, levels = fe_label_levels))

  # calculate combined range for both plots to ensure same legend
  all_values <- c(bt$bias, fe_data$estimate)
  value_range <- range(all_values, na.rm = TRUE)

  p1 <- ggplot(bt, aes(x = model, y = label_name, fill = bias)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", bias), sig)), color = "black", size = 3) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias",
                         limits = value_range) +
    labs(subtitle = "Option bias (z-test)",
         x = "Model", y = "Label") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8))

  p2 <- fe_heatmap_result$plot +
    labs(subtitle = paste("FE regression (relative to reference: ", ref_label_name, ")", sep = ""),
         title = NULL, x = "Model", y = NULL) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "red",
                         midpoint = 0, name = "Bias",
                         limits = value_range) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank())
  
  combined <- (p1 | p2) +
    plot_layout(widths = c(1.2, 1), guides = "collect") +
    plot_annotation(
      title = paste("Label-level bias:", get_question_label(q)),
      tag_levels = 'A'
    ) &
    theme(plot.tag = element_text(face = 'bold', size = 10),
          legend.position = "right")

  fn_combined <- paste0("bias_", q, "_out.jpg")
  # custom size for who_cat_clean
  sizes <- if (q %in% c("who_cat_clean")) {
    list(width = 7, height = 5)
  } else {
    list(width = 6, height = 3)
  }
  ggsave(paste(plot_folder, fn_combined, sep = ""), plot = combined, width = sizes$width, height = sizes$height, dpi = 350)
  #readr::write_csv(bt, paste0("option_bias_", q, ".csv"))
}