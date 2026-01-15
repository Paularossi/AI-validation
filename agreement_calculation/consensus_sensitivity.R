

# for each sensitivity condition, overwrite the _cons columns with one of the alternative consensus variants
# for each rule r: {thr2_union, thr2_empty, union, intersection}:
# 1. Set:
# - prem_offer_cons = prem_offer_cons_<r>
# - marketing_str_cons = marketing_str_cons_<r>
# - who_cat_clean_cons = who_cat_clean_cons_<r>
# 2. Run the existing agreement pipeline.
# 3. Store the resulting agreement metrics.

source("C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/agreement_calculation/agreement_functions.R")
root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/data/"

df <- read_excel(paste(root_folder, "responses_human_final_sensitivity.xlsx", sep=""))
responses_dieticians <- read_excel(paste(root_folder, "dieticians_all_final.xlsx", sep=""))

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))

models <- list(Gemma = gemma, Pixtral = pixtral, GPT = gpt, Qwen = qwen)
rules <- c("thr2_union", "thr2_empty", "union", "intersection")

# helper: activate a rule by copying *_cons_<rule> into *_cons (so downstream code stays unchanged)
activate_consensus_rule <- function(dat, vars, rule_suffix) {
  for (v in vars) {
    src <- paste0(v, "_cons_", rule_suffix)
    dst <- paste0(v, "_cons")
    if (!src %in% names(dat)) {
      stop("Missing consensus column: ", src, "\nCheck that you created it earlier.")
    }
    dat[[dst]] <- dat[[src]]
  }
  dat
}

results <- list()

for (r in rules) {
  paste0("===== EVALUATING CONSENSUS RULE: ", r, " =====")
  dat_active <- activate_consensus_rule(df, multi_label_vars, r)
  
  #out <- compare_multilabel_human_ai(dat_active, models, consensus = FALSE, 
  #                                   responses_dieticians, multi_label_vars = multi_label_vars, filter_other = TRUE)
  # without the dieticians
  out_ml <- compare_multilabel_human_ai(dat_active, models, consensus = TRUE,
                                        multi_label_vars = multi_label_vars, filter_other = TRUE)
  
  #TODO: need to first do the correct consensus for dieticians and then run this as well
  
  
  results[[r]] <- out_ml
}

# combine the results from all consensus rules
res_all <- bind_rows(lapply(names(results), function(r) {
  out <- results[[r]]
  out %>% mutate(consensus_rule = r)
}))
write_xlsx(res_all, file.path(root_folder, "multi_agreement_all_cons_rules.xlsx"))

# agreement levels by rule
ai_models <- c("GPT", "Qwen", "Pixtral", "Gemma")
res_ai_cons <- res_all %>% # remove this later as we run sensitivity only on consensus=TRUE
  filter(
    (rater1 %in% ai_models & str_detect(rater2, "_cons$")) |
      (rater2 %in% ai_models & str_detect(rater1, "_cons$"))
  ) %>%
  mutate(
    model = if_else(rater1 %in% ai_models, rater1, rater2)
  )

sens_table <- res_ai_cons %>%
  group_by(consensus_rule, question, model) %>%
  summarise(
    jaccard = mean(jaccard, na.rm = TRUE),
    alpha_masi = mean(kripp_alpha_masi, na.rm = TRUE),
    .groups = "drop"
  )

# if we pick the original rule as baseline:
baseline <- sens_table %>% filter(consensus_rule == "thr2_union") %>%
  select(question, model, jaccard_base = jaccard, alpha_base = alpha_masi)

delta_table <- sens_table %>%
  left_join(baseline, by = c("question", "model")) %>%
  mutate(
    delta_jaccard = jaccard - jaccard_base,
    delta_alpha   = alpha_masi - alpha_base
  ) %>%
  filter(consensus_rule != "thr2_union")

plot_df <- sens_table %>%
  filter(consensus_rule %in% c("thr2_union", "thr2_empty", "union", "intersection")) %>%
  select(consensus_rule, question, model, alpha_masi)

# for each (question, model), plot baseline vs each alternative rule
base_df <- plot_df %>%
  filter(consensus_rule == "thr2_union") %>%
  rename(alpha_base = alpha_masi) %>%
  select(question, model, alpha_base)

alt_df <- plot_df %>%
  filter(consensus_rule != "thr2_union") %>%
  left_join(base_df, by = c("question", "model"))

ggplot(alt_df, aes(y = model)) +
  geom_segment(aes(x = alpha_base, xend = alpha_masi, yend = model)) +
  geom_point(aes(x = alpha_base), size = 2) +
  geom_point(aes(x = alpha_masi), size = 2) +
  facet_grid(question ~ consensus_rule, scales = "free_x") +
  labs(x = "α(MASI)", y = NULL) +
  theme_minimal()



# summarize deltas
delta_summary <- delta_table %>%
  group_by(consensus_rule, question) %>%
  summarise(
    mean_delta_jaccard = mean(delta_jaccard, na.rm = TRUE),
    max_abs_delta_jaccard = max(abs(delta_jaccard), na.rm = TRUE),
    mean_delta_alpha = mean(delta_alpha, na.rm = TRUE),
    max_abs_delta_alpha = max(abs(delta_alpha), na.rm = TRUE),
    .groups = "drop"
  )




