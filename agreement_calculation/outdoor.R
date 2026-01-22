
library(readxl)
library(writexl)

source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")

root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/data/"
plot_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/plots/"
responses_diet_all <- read_excel(paste(root_folder, "dieticians_outdoor_all_final.xlsx", sep="")) %>%
  mutate(img_id = gsub("\\.jpg$", "", img_id)) %>% arrange(img_id)
gpt <- read_excel(paste(root_folder, "gpt_all_outdoor.xlsx", sep="")) %>% arrange(img_id)
qwen <- read_excel(paste(root_folder, "qwen_all_outdoor.xlsx", sep="")) %>% arrange(img_id)

gpt <- gpt %>%
  mutate(brand = map_chr(brand, clean_brand))

qwen <- qwen %>%
  mutate(brand = map_chr(brand, clean_brand))

models <- list(GPT = gpt, Qwen = qwen)


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

ggsave(paste(plot_folder, "everything_single_out.jpg", sep=""), width = 8, height = 5)



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

ggsave(paste(plot_folder, "everything_multiple.jpg", sep=""), width = 8, height = 5)



# ===== ANALYZE THE BRAND QUESTION =====
df_brand <- responses_diet_all %>%
  select(c(img_id, brand_diet1, brand_diet2, brand_diet3)) %>%
  mutate(
    brand_diet1 = map_chr(brand_diet1, clean_brand),
    brand_diet2 = map_chr(brand_diet2, clean_brand),
    brand_diet3 = map_chr(brand_diet3, clean_brand),
  )

# cons_threshold from merging_outputs.R
diet_cols <- paste0("brand_diet", 1:3)
df_brand <-  df_brand %>%
  mutate(brand_dietcons = pmap_chr(
    list(brand_diet1, brand_diet2, brand_diet3),
    ~ cons_threshold(..1, ..2, ..3, threshold = 2, fallback = "union")
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

ggsave(paste(plot_folder, "brand.jpg", sep=""), width = 6, height = 5)

# ======== OPTION BIAS WITHIN EACH QUESTION (z-tests) + LABEL-LEVEL FIXED EFFECTS REGRESSION ========
#source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")
source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/monte_regr.R")
library(patchwork)
# compute and plot option bias for all questions
bias_prem <- compute_option_bias("prem_offer", models, responses_diet_all) %>%
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

bias_market <- compute_option_bias("marketing_str", models, responses_diet_all) %>%
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

bias_who <- compute_option_bias("who_cat_clean", models, responses_diet_all) %>%
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

ggsave(paste(plot_folder, "bias_combined_multi_out.jpg", sep = ""), plot = combined_bias_plot, width = 10, height = 5, dpi = 350)


bias_alc <- compute_option_bias("alcohol", models, responses_diet_all) %>%
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

bias_targ <- compute_option_bias("target_group", models, responses_diet_all) %>%
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

bias_type <- compute_option_bias("new_type_ad", models, responses_diet_all) %>%
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

ggsave(paste(plot_folder, "bias_combined_single_out.jpg", sep = ""), plot = combined_bias_plot, width = 6, height = 7, dpi = 350)



















