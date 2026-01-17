library(readxl)
library(writexl)

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
plot_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/plots/"

humans <- read_excel(paste(root_folder, "responses_human_final_sensitivity.xlsx", sep=""))
diets <- read_excel(paste(root_folder, "dieticians_all_sensitivity.xlsx", sep=""))

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))

models <- list(Gemma = gemma, Pixtral = pixtral, GPT = gpt, Qwen = qwen)
rules <- c("thr2_union", "thr2_empty", "union", "intersection")

# helper: activate a rule by copying *_cons_<rule> into *_cons (so downstream code stays unchanged)
activate_consensus_rule <- function(dat, vars, rule_suffix, diet=FALSE) {
  for (v in vars) {
    src <- paste0(v, ifelse(diet, "_dietcons_", "_cons_"), rule_suffix)
    dst <- paste0(v, ifelse(diet, "_dietcons", "_cons"))
    dat[[dst]] <- dat[[src]]
  }
  dat
}

results_h <- list()
results_d <- list()

for (r in rules) {
  print(paste0("===== EVALUATING CONSENSUS RULE: ", r, " ====="))
  humans_active <- activate_consensus_rule(humans, multi_label_vars, r)
  diet_active <- activate_consensus_rule(diets, multi_label_vars, r, diet = TRUE)

  # without the dieticians
  out <- compare_multilabel_human_ai(humans_active, models, consensus = TRUE,
                                     multi_label_vars = multi_label_vars, filter_other = TRUE)
  out_diet <- compare_multilabel_human_ai(diet_active, models, consensus = TRUE,
                                     multi_label_vars = multi_label_vars, filter_other = TRUE)
  # add the human responses here??? i'm confused

  results_h[[r]] <- out
  results_d[[r]] <- out_diet
}

# combine the results from all consensus rules
res_all <- bind_rows(lapply(names(results_h), function(r) {
  out <- results_h[[r]]
  out %>% mutate(consensus_rule = r)
}))

res_all_d <- bind_rows(lapply(names(results_d), function(r) {
  out <- results_d[[r]]
  out %>% mutate(consensus_rule = r)
}))

res_all <- rbind(res_all, res_all_d)
res_all <- res_all[!duplicated(res_all), ] %>% arrange(consensus_rule, question)

write_xlsx(res_all, file.path(root_folder, "multi_agreement_all_cons_rules.xlsx"))


# reload the data
res_all <- read_excel(paste(root_folder, "multi_agreement_all_cons_rules.xlsx", sep=""))

# agreement levels by rule
ai_models <- c("GPT", "Qwen", "Pixtral", "Gemma")
res_ai_cons <- res_all %>%
  filter(
    (rater1 %in% ai_models & str_detect(rater2, "_(cons|dietcons)$")) |
      (rater2 %in% ai_models & str_detect(rater1, "_(cons|dietcons)$"))
  ) %>%
  mutate(model = if_else(rater1 %in% ai_models, rater1, rater2))

calculate_delta <- function(res_ai_cons, fallback_rates, diet = TRUE) {
  sens_table <- res_ai_cons %>%
    # filter only for humans/dieticians
    filter(str_ends(rater1, ifelse(diet, ".*_dietcons$", ".*_cons$"))) %>%
    group_by(consensus_rule, question, model) %>%
    summarise(
      jaccard = mean(jaccard, na.rm = TRUE),
      alpha_masi = mean(kripp_alpha_masi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # change labels for consensus rules
    mutate(consensus_rule = recode(consensus_rule,
                                   thr2_empty = "Threshold 2 - Empty",
                                   union = "Union",
                                   intersection = "Intersection",
                                   thr2_union = "Baseline")) %>%
    mutate(
      consensus_rule = factor(
        consensus_rule,
        levels = c("Baseline", "Intersection", "Threshold 2 - Empty", "Union")
      )
    )

  # if we pick the original rule as baseline:
  baseline <- sens_table %>% filter(consensus_rule == "Baseline") %>%
    select(question, model, jaccard_base = jaccard, alpha_base = alpha_masi)

  delta_table <- sens_table %>%
    left_join(baseline, by = c("question", "model")) %>%
    mutate(delta_jaccard =  jaccard - jaccard_base,
          delta_alpha = alpha_masi - alpha_base) %>%
    filter(consensus_rule != "Baseline")

  # join dietician fallback rates to delta table
  delta_table <- delta_table %>%
    left_join(fallback_rates %>% select(question, fallback_rate = pct_fallback), by = "question")

  delta_table %>%
    filter(consensus_rule != "Baseline") %>%
    droplevels()
}


plot_delta <- function(delta_table, diet = TRUE, save = FALSE) {
  alpha_limits <- range(c(delta_table$alpha_base, delta_table$alpha_masi), na.rm = TRUE)
  x_center <- mean(alpha_limits)

  rule_levels <- if (is.factor(delta_table$consensus_rule)) {
    levels(delta_table$consensus_rule)
  } else {
    sort(unique(delta_table$consensus_rule))
  }
  rule_top <- rule_levels[1]

  fallback_labels <- delta_table %>%
    distinct(question, fallback_rate) %>%
    mutate(
      consensus_rule = rule_top, # facet row to draw on
      label = paste0("Fallback: ", round(fallback_rate * 100, 1), "%")
    )

  # line plot with points for baseline and alternative
  g <- ggplot(delta_table, aes(y = model)) +
        geom_segment(aes(x = alpha_base, xend = alpha_masi, yend = model)) +
        geom_point(aes(x = alpha_base), shape = 1, col = "#178817", size = 2) +   # baseline
        geom_point(aes(x = alpha_masi), shape = 16, size = 2) +  # alternative
        facet_grid(consensus_rule ~ question) +
        # add the fallback rates just once next to the question labels
        geom_text(data = fallback_labels,
                  aes(label = label),
                  x = x_center, y = Inf,
                  hjust = 0.5, vjust = 1, inherit.aes = FALSE, size = 3, fontface = "bold.italic", color = "#178817") +
        labs(x = "α(MASI)", y = NULL, title = paste("Agreement Sensitivity by Consensus Rule and Question Against", ifelse(diet, "Dieticians", "Humans"))) +
        theme_minimal()

  if (save) {
    ggsave(paste(plot_folder, paste("consensus_delta_", ifelse(diet, "diet", "human"), ".jpg", sep = ""), sep = ""), plot = g, width = 8, height = 5.5)
  }
  g
}


# load the fallback rates
human_fallback <- read_excel(paste(root_folder, "responses_human_final_sensitivity.xlsx", sep=""), sheet = 2)
diet_fallback <- read_excel(paste(root_folder, "dieticians_all_sensitivity.xlsx", sep=""), sheet = 2)

delta_table_diet <- calculate_delta(res_ai_cons, diet_fallback)
delta_table_human <- calculate_delta(res_ai_cons, human_fallback, diet = FALSE)

write_xlsx(delta_table_diet, file.path(plot_folder, "consensus_delta_dieticians.xlsx"))
write_xlsx(delta_table_human, file.path(plot_folder, "consensus_delta_humans.xlsx"))

view(delta_table_diet)

plot_delta(delta_table_diet, diet = TRUE, save = TRUE)
plot_delta(delta_table_human, diet = FALSE, save = TRUE)



# barplot of deltas
ggplot(delta_table_diet, aes(x = model, y = delta_alpha, fill = consensus_rule)) +
  geom_col(position = "dodge") +
  facet_wrap(~ question, nrow = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    y = "Δ α(MASI) relative to baseline",
    x = NULL
  ) +
  theme_minimal()

# summarize deltas
delta_table_diet %>%
  group_by(consensus_rule, question) %>%
  summarise(
    mean_delta_jaccard = mean(delta_jaccard, na.rm = TRUE),
    max_abs_delta_jaccard = max(abs(delta_jaccard), na.rm = TRUE),
    mean_delta_alpha = mean(delta_alpha, na.rm = TRUE),
    max_abs_delta_alpha = max(abs(delta_alpha), na.rm = TRUE),
    fallback_rate = min(fallback_rate, na.rm = TRUE),
    .groups = "drop"
  )