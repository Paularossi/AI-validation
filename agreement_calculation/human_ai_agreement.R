library(readxl)
library(writexl)
library(ggplot2)

source("C:/Users/P70090005/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")

root_folder <- "C:/Users/P70090005/Desktop/phd/AI-validation/gpu outputs/"
responses_human_all <- read_excel(paste(root_folder, "responses_human_final.xlsx", sep=""))
responses_dieticians <- read_excel(paste(root_folder, "dieticians_all_final.xlsx", sep=""))

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))

# to get the categories of the ads (snacks, drinks, etc)
# the code below (for all models) is run only once
# cats <- read_excel("C:/Users/P70090005/Desktop/phd/AI-validation/data/3000_ads_by_cat_language.xlsx")

# qwen <- cats %>% select(c(img_id, language, category)) %>%
#   right_join(qwen, by = "img_id")
# write_xlsx(qwen, paste(root_folder, "qwen_all_1000.xlsx", sep=""))

# responses_human_all$img_id <- gsub("\\.png$", "", responses_human_all$Image_ID)
# responses_human_all <- cats %>% select(c(img_id, category, language)) %>%
#   right_join(responses_human_all, by = "img_id")
# write_xlsx(responses_human_all, paste(root_folder, "responses_human_final.xlsx", sep=""))

# change to only use the images from dieticians
gemma <- gemma %>% 
  filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)
gpt <- gpt %>% 
  filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)
pixtral <- pixtral %>%
  filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)
qwen <- qwen %>% 
  filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)

# responses_human_all <- responses_human_all %>% 
#   filter(img_id %in% responses_dieticians$img_id) %>% arrange(img_id)

models <- list(Gemma = gemma, Pixtral = pixtral, GPT = gpt, Qwen = qwen)



########## COMPARE HUMANS WITH THE AIS

# ====== SINGLE-CHOICE COLUMNS ======

# without the dieticians
results_kappa <- compare_human_ai_kappa(responses_human_all, models)

# with the dieticians
results_kappa_diet <- compare_human_ai_kappa(responses_human_all, models, responses_dieticians)

# unnest the conf int for gwet
results_kappa_diet <- results_kappa_diet %>%
  mutate(gwet_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", gwet_ci)),
         gwet_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", gwet_ci))) 

# pivot to long format for plotting
plot_df <- results_kappa_diet %>%
  mutate(rater = sub("^.+_(coder[1-3]|diet[1-3]|cons|dietcons)$", "\\1", coder),
         rater = recode(rater,
                        coder1 = "Coder 1", coder2 = "Coder 2", coder3 = "Coder 3",
                        diet1 = "Dietician 1", diet2 = "Dietician 2", diet3 = "Dietician 3",
                        cons   = "Consensus", dietcons = "Consensus (D)")) %>%
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

ggsave(paste(root_folder, "plots/human_diet_ai_single.png", sep=""), width = 9, height = 5)

# plot with the confidence intervals
plot_df_confint <- results_kappa_diet %>%
  pivot_longer(cols = c(kappa, gwet_coeff),
               names_to = "metric", values_to = "value") %>%
  mutate(
    ci_low = ifelse(metric == "kappa", kappa_conf_low, gwet_ci_low),
    ci_upp = ifelse(metric == "kappa", kappa_conf_upp, gwet_ci_upp),
    metric = recode(metric, kappa = "Cohen's Kappa", gwet_coeff = "Gwet’s AC1"),
    coder = sub("^.+_(coder[1-3]|cons|diet[1-3]|dietcons)$", "\\1", coder)
  )

ggplot(plot_df_confint, aes(x = coder, y = value, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_upp),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_grid(question ~ metric) +
  labs(y = "Agreement", x = "AI Model", color = "Rater",
       title = "Confidence Intervals of AI vs. Human Agreement") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing.y = unit(1, "lines"),  # adds space between rows
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold", size = 12)
  ) 

ggsave(paste(root_folder, "plots/human_diet_ai_single_confint.png", sep=""), width = 9, height = 5)

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
#comparison_single_all <- read_excel(paste(root_folder, "plots/agreement_single_all.xlsx", sep=""))


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
      title = "Agreement Across All Raters (Single-Choice)",
      x = "Rater 1", y = "Rater 2"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = -90),
      panel.spacing = unit(0.5, "lines")
    )

ggsave(paste(root_folder, "plots/single_mixed_paper.png", sep=""), width = 10, height = 5)


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


# multi_confint <- read_excel(paste(root_folder, "plots/agreement_multiple_all.xlsx", sep=""))
# 
# # plot with the confidence intervals
# plot_df_confint <- multi_confint %>%
#   filter(!(rater1 %in% c("gpt", "qwen", "gemma", "pixtral")),
#          rater2 %in% c("gpt", "qwen", "gemma", "pixtral")) %>%
#   mutate(kripp_masi_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", kripp_alpha_masi_ci)),
#          kripp_masi_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", kripp_alpha_masi_ci))) %>%
#   pivot_longer(cols = c(jaccard, kripp_alpha_masi),
#                names_to = "metric", values_to = "value") %>%
#   mutate(
#     ci_low = ifelse(metric == "jaccard", value, kripp_masi_ci_low),
#     ci_upp = ifelse(metric == "jaccard", value, kripp_masi_ci_upp),
#     metric = recode(metric, jaccard = "Jaccard Similarity", kripp_alpha_masi = "Krippendorff's Alpha MASI")
#   )
# 
# ggplot(plot_df_confint, aes(x = rater1, y = value, color = rater2)) +
#   geom_point(position = position_dodge(width = 0.5)) +
#   geom_errorbar(aes(ymin = ci_low, ymax = ci_upp),
#                 width = 0.2, position = position_dodge(width = 0.5)) +
#   facet_grid(question ~ metric) +
#   labs(y = "Agreement", x = "AI Model", color = "Rater",
#        title = "Confidence Intervals of AI vs. Human Agreement") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     panel.spacing.y = unit(1, "lines"),  # adds space between rows
#     strip.background = element_rect(fill = "gray90", color = NA),
#     strip.text = element_text(face = "bold", size = 12)
#   ) 
# 
# ggsave(paste(root_folder, "plots/human_ai_multiple_confint.png", sep=""), width = 9, height = 5)



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
#res_ml_all <- read_excel(paste(root_folder, "plots/agreement_multiple_all.xlsx", sep=""))

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
    title = "Agreement Across All Raters (Multi-Label)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )


ggsave(paste(root_folder, "plots/multiple_mixed_paper.png", sep=""), width = 10, height = 5)


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



# ==============================================================================
# MORE ANALYSIS - by category of the brand
cats <- read_excel("C:/Users/P70090005/Desktop/phd/AI-validation/data/3000_ads_by_cat_language.xlsx")

responses_human_all <- cats %>%
  select(c(img_id, category, language)) %>%
  inner_join(responses_human_all, by = "img_id")

responses_dieticians <- cats %>%
  select(c(img_id, category, language)) %>%
  inner_join(responses_dieticians, by = "img_id")

table(responses_human_all$category)

# how consistent are AI models in different product categories?
category_agreement_single <- lapply(single_choice_vars, function(col) {
  df <- agreement_by_category_all(list(gpt = gpt, qwen = qwen), column=col, 
                                  responses_human_all, responses_dieticians)
  df$question <- col
  df
}) %>% bind_rows()

category_agreement_multiple <- lapply(multi_label_vars, function(col) {
  df <- agreement_by_category_all(list(gpt = gpt, qwen = qwen), column = col, 
                                  responses_human_all, responses_dieticians, multiple = TRUE)
  df$question <- col
  df
}) %>% bind_rows()

custom_colors <- c(
  "gpt_consensus" = "#00BFFF",
  "gpt_diet_consensus" = "#1C86EE",
  #"qwen_consensus" = "#1C86EE",
  "gpt_qwen" = "#EEC900"
)
table(category_agreement_single$model_pair)

# single choice
category_agreement_single %>%
  mutate(category_n = paste0(category, " (", n, ")")) %>%
  filter(model_pair %in% c("gpt_qwen", "gpt_consensus", "gpt_diet_consensus"),
         question == "new_type_ad") %>%
  ggplot(aes(x = category, y = gwet_coeff, fill = model_pair)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = round(gwet_coeff, 2)), 
              position = position_dodge(width = 0.9), vjust = 3.5, size = 2.5) +
    geom_errorbar(aes(ymin = gwet_ci_low, ymax = gwet_ci_upp),
                  width = 0.2, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = custom_colors) +
    #facet_wrap(~ question, scales = "free_x") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Model Agreement by Brand Category - Ad Type",
      y = "Gwet's AC Agreement",
      x = "Brand Category"
    )

ggsave(paste(root_folder, "plots/brand_gwet_type_ad_single.png", sep=""), width = 10, height = 6)


# multi-label
multi_confint <- read_excel(paste(root_folder, "plots/agreement_multiple_all.xlsx", sep=""))

overall_alphas <- multi_confint %>%
  filter(question == "marketing_str", rater1 == "Consensus") %>%
  select(rater2, kripp_alpha_masi)
alpha_gpt <- overall_alphas %>% filter(rater2 == "gpt") %>% pull(kripp_alpha_masi)
alpha_qwen <- overall_alphas %>% filter(rater2 == "qwen") %>% pull(kripp_alpha_masi)


category_agreement_multiple %>%
  filter(model_pair %in% c("gpt_qwen", "gpt_consensus", "gpt_diet_consensus"),
         question == "marketing_str") %>%
  mutate(category_n = paste0(category, " (", n, ")")) %>%
  ggplot(aes(x = category, y = alpha_masi, fill = model_pair)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(alpha_masi, 2)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
    geom_errorbar(aes(ymin = alpha_ci_low, ymax = alpha_ci_upp),
                  width = 0.2, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = custom_colors) +
    annotate("text", x = Inf, y = Inf,
             label = paste0("GPT vs Consensus α = ", round(alpha_gpt, 2)),
             hjust = 1, vjust = 26, size = 4, fontface = "italic") +
    annotate("text", x = Inf, y = Inf,
             label = paste0("Qwen vs Consensus α = ", round(alpha_qwen, 2)),
             hjust = 1, vjust = 28, size = 4, fontface = "italic") +
    #facet_wrap(~ question, scales = "free_x") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    #ylim(-0.6, 1) +
    labs(
      title = "Model Agreement by Brand Category - Marketing Strategies",
      y = "Krippendorff's Alpha (MASI)",
      x = "Brand Category"
    ) 
  

ggsave(paste(root_folder, "plots/brand_alpha_marketing_str.png", sep=""), width = 10, height = 6)

# find some categories to analyze more in detail, for ex. online ordering and sweet 
# biscuits have really low agreement for who_cat




top_bottom <- category_agreement_multiple %>%
  group_by(category, question, model_pair) %>%
  summarise(avg_alpha_masi = mean(alpha_masi, na.rm = TRUE)) %>%
  arrange(avg_alpha_masi)

head(top_bottom, 5)    # worst categories
tail(top_bottom, 5) 


# next steps:
# - compare model-model agreement with human-human agreement



table(gpt$new_type_ad, responses_human_all$new_type_ad_cons)


# - find ads where all models give a different label or no model matches human consensus
# disagreed_ads_single <- lapply(single_choice_vars, function(col) {
#   df <- find_disagreeing_ads(models = list(gpt = gpt, qwen = qwen), 
#                              column = col, humans = responses_dieticians)
#   df$question <- col
#   df
# }) %>% bind_rows()
# 
# #disagreed_ads_adtype %>% group_by(category) %>% count() %>% arrange(desc(n))
# 
# # !!! doesn't really make sense for multiple columns
# disagreed_ads_multiple <- lapply(multi_label_vars, function(col) {
#   df <- find_disagreeing_ads(models = list(gpt = gpt, qwen = qwen), 
#                              column = col, humans = responses_human_all)
#   df$question <- col
#   df
# }) %>% bind_rows()



# by label for single columns 
disagreed_by_label_single <- lapply(single_choice_vars, function(col) {
  df <- single_disagreement_by_label(models = models, column = col, dieticians = responses_dieticians)
  df$question <- col
  df
}) %>% bind_rows()


# 1. which labels are most missed overall? (pct_all_failed) - ambiguity in visual/textual cues.
# 2. which model misses more?

plot_df <- disagreed_by_label_single %>%
  left_join(label_df, by = join_by(label == code, question == category)) %>%
  filter(question == "target_group") %>%
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


ggplot(plot_df, aes(x = model, y = reorder(text_label_n, -pct), fill = pct)) +
  geom_tile(color = "white") +
  #geom_text(aes(label = round(pct, 3)), size = 4) +
  geom_text(aes(label = paste(round(pct, 2), " (", n, ")", sep="")), size = 4) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
  labs(title = "Missed Consensus Labels by Model and Label (Target Group)",
       x = "Model", y = "Label Code", fill = "% Missed") +
  theme_minimal()

ggsave(paste(root_folder, "plots/ai_missed_target_group.png", sep=""), width = 10, height = 6)


# by label for multi-label
disagreed_by_label_multiple <- lapply(multi_label_vars, function(col) {
  df <- multi_disagreement_by_label(models = models, column = col, dieticians = responses_dieticians)
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

ggplot(plot_df, aes(x = model, y = reorder(text_label_n, -pct), fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(round(pct, 2), " (", n, ")", sep="")), size = 4) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
  labs(title = "Missed Consensus Labels by Model and Label (WHO Categories)",
       x = "Model", y = "Label Code", fill = "% Missed") +
  theme_minimal()

ggsave(paste(root_folder, "plots/ai_missed_who_cat.png", sep=""), width = 10, height = 6)

# need to remove the "Other" labels in prem offers and marketing str


table(gpt$new_type_ad, responses_dieticians$new_type_ad_dietcons)




# ======= ANALYSIS BY LANGUAGE =======
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
  rename(gwet_coeff_nl = gwet_coeff, prop_agreement_nl = prop_agreement) %>%
  inner_join(comparison_df_diet_fr %>% rename(gwet_coeff_fr = gwet_coeff, prop_agreement_fr = prop_agreement),
             by = c("rater1", "rater2", "question")) %>%
  mutate(delta_gwet = gwet_coeff_nl - gwet_coeff_fr,
         delta_agreement = prop_agreement_nl - prop_agreement_fr) %>%
  filter(
    # keep pairs that involve dieticians, consensus, or AI
    grepl("diet", rater1) | #grepl("diet", rater2) |
      grepl("cons", rater1) | #grepl("cons", rater2) |
      grepl("gpt|qwen|pixtral|gemma", rater1)#| grepl("gpt|qwen|pixtral|gemma", rater2)
  ) %>%
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
ggsave(paste(root_folder, "plots/delta_single.png", sep=""), width = 11, height = 4)


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
  rename(kripp_alpha_masi_nl = kripp_alpha_masi, jaccard_nl = jaccard) %>%
  inner_join(res_ml_diet_fr %>% rename(kripp_alpha_masi_fr = kripp_alpha_masi, jaccard_fr = jaccard),
             by = c("rater1", "rater2", "question")) %>%
  mutate(delta_masi = kripp_alpha_masi_nl - kripp_alpha_masi_fr,
         delta_jaccard = jaccard_nl - jaccard_fr) %>%
  filter(
    # keep pairs that involve dieticians, consensus, or AI
    grepl("diet", rater1) | #grepl("diet", rater2) |
      grepl("cons", rater1) | #grepl("cons", rater2) |
      grepl("gpt|qwen|pixtral|gemma", rater1)#| grepl("gpt|qwen|pixtral|gemma", rater2)
  ) %>%
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

ggsave(paste(root_folder, "plots/delta_multiple.png", sep=""), width = 11, height = 4)







