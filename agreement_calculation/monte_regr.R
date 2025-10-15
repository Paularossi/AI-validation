library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(irr)
library(irrCAC)
library(openxlsx)
library(readxl)

# only need the merge_ai_human_column function
source("agreement_calculation/agreement_functions.R")

root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/gpu outputs/"
responses_human_all <- read_excel(paste(root_folder, "responses_human_final.xlsx", sep = "")) %>% arrange(img_id)
responses_dieticians <- read_excel(paste(root_folder, "dieticians_all_final.xlsx", sep = "")) %>% arrange(img_id)

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep = "")) %>% arrange(img_id)
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep = "")) %>% arrange(img_id)
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep = "")) %>% arrange(img_id)
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep = "")) %>% arrange(img_id)

# define the scorer function (agreement for single vs. multi choice)
scorer <- function(predictions, consensus, task_type) {

  if (task_type == "single") {
    gwet_coeff <- gwet.ac1.raw(cbind(predictions, consensus))$est$coeff.val
    prop_agreement <- gwet.ac1.raw(cbind(predictions, consensus))$est$pa
    gwet_ci <- gwet.ac1.raw(cbind(predictions, consensus))$est$conf.int

    list(gwet_coeff, prop_agreement, gwet_ci)

  } else {
    sims <- mapply(jaccard_similarity, predictions, consensus)
    jacc <- mean(sims, na.rm = TRUE)

    wt <- MASI_simmilarity_matrix(data.frame(predictions, consensus), sep = ", ")
    alpha_masi <- krippen.alpha.raw(ratings = data.frame(predictions, consensus), weights = wt)$est

    list(jacc, alpha_masi = alpha_masi$coeff.val, alpha_masi_ci = alpha_masi$conf.int)
  }
}


# Monte-carlo simulation for bootstrapping agreement metrics
bootstrap_agreement <- function(models, humans, column, dieticians, B = 1000, task_type) {

  merged <- merge_ai_human_column(models, humans, column, dieticians)
  # remove rows with all NA in dietician columns
  merged <- merged %>%
    filter(!(is.na(diet1) & is.na(diet2) & is.na(diet3)))

  n <- nrow(merged)

  if (task_type == "single") {
    sims <- map_dfr(seq_len(B), function(b) {
      idx <- sample.int(n, size = n, replace = TRUE)
      tibble(
        iter  = b,
        diet1_gwet = scorer(merged$diet1[idx], merged$dietcons[idx], task_type)[[1]],
        diet1_prop = scorer(merged$diet1[idx], merged$dietcons[idx], task_type)[[2]],
        diet1_ci = list(scorer(merged$diet1[idx], merged$dietcons[idx], task_type)[[3]]),

        diet2_gwet = scorer(merged$diet2[idx], merged$dietcons[idx], task_type)[[1]],
        diet2_prop = scorer(merged$diet2[idx], merged$dietcons[idx], task_type)[[2]],
        diet2_ci = list(scorer(merged$diet2[idx], merged$dietcons[idx], task_type)[[3]]),

        diet3_gwet = scorer(merged$diet3[idx], merged$dietcons[idx], task_type)[[1]],
        diet3_prop = scorer(merged$diet3[idx], merged$dietcons[idx], task_type)[[2]],
        diet3_ci = list(scorer(merged$diet3[idx], merged$dietcons[idx], task_type)[[3]]),

        model_gwet = scorer(merged$GPT[idx], merged$dietcons[idx], task_type)[[1]],
        model_prop = scorer(merged$GPT[idx], merged$dietcons[idx], task_type)[[2]],
        model_ci = list(scorer(merged$GPT[idx], merged$dietcons[idx], task_type)[[3]])
      )
    })

    # exchangeability: model within [min(diets), max(diets)]
    sims <- sims %>%
      rowwise() %>%
      mutate(
        min_h = min(c(diet1_gwet, diet2_gwet, diet3_gwet), na.rm = TRUE),
        max_h = max(c(diet1_gwet, diet2_gwet, diet3_gwet), na.rm = TRUE),
        in_range = (model_gwet >= min_h) & (model_gwet <= max_h)
      ) %>%
      ungroup()

    # calculate confidence intervals for agreement metrics
    agreement_metrics <- sims %>%
      summarise(
        diet1_mean_gwet = mean(diet1_gwet, na.rm = TRUE),
        diet2_mean_gwet = mean(diet2_gwet, na.rm = TRUE),
        diet3_mean_gwet = mean(diet3_gwet, na.rm = TRUE),
        model_mean_gwet = mean(model_gwet, na.rm = TRUE),
        exch_pct = mean(in_range, na.rm = TRUE)
      )
  } else {

    sims <- map_dfr(seq_len(B), function(b) {
      idx <- sample.int(n, size = n, replace = TRUE)
      score_diet1 <- scorer(merged$diet1[idx], merged$dietcons[idx], task_type)
      score_diet2 <- scorer(merged$diet2[idx], merged$dietcons[idx], task_type)
      score_diet3 <- scorer(merged$diet3[idx], merged$dietcons[idx], task_type)
      score_model <- scorer(merged$GPT[idx], merged$dietcons[idx], task_type)
      tibble(
        iter  = b,
        diet1_masi = score_diet1[[1]],
        diet1_jacc = score_diet1[[2]],
        diet1_ci = list(score_diet1[[3]]),

        diet2_masi = score_diet2[[1]],
        diet2_jacc = score_diet2[[2]],
        diet2_ci = list(score_diet2[[3]]),

        diet3_masi = score_diet3[[1]],
        diet3_jacc = score_diet3[[2]],
        diet3_ci = list(score_diet3[[3]]),

        model_masi = score_model[[1]],
        model_jacc = score_model[[2]],
        model_ci = list(score_model[[3]])
      )
    })

    # exchangeability: model within [min(diets), max(diets)]
    sims <- sims %>%
      rowwise() %>%
      mutate(
        min_h = min(c(diet1_masi, diet2_masi, diet3_masi), na.rm = TRUE),
        max_h = max(c(diet1_masi, diet2_masi, diet3_masi), na.rm = TRUE),
        in_range = (model_masi >= min_h) & (model_masi <= max_h)
      ) %>%
      ungroup()

    # calculate confidence intervals for agreement metrics
    agreement_metrics <- sims %>%
      summarise(
        diet1_mean_masi = mean(diet1_masi, na.rm = TRUE),
        diet2_mean_masi = mean(diet2_masi, na.rm = TRUE),
        diet3_mean_masi = mean(diet3_masi, na.rm = TRUE),
        model_mean_masi = mean(model_masi, na.rm = TRUE),
        exch_pct = mean(in_range, na.rm = TRUE)
      )

  }
  list(
    sims = sims,
    agreement_metrics = agreement_metrics
  )
}

# plot the distribution of gwet coefficients for dieticians and model 
plot_gwet_distribution <- function(sims, question) {

  sims_long <- sims %>%
    select(iter, diet1_gwet, diet2_gwet, diet3_gwet, model_gwet) %>%
    pivot_longer(cols = -iter, names_to = "rater", values_to = "gwet")

  ggplot(sims_long, aes(x = gwet, fill = rater)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of Gwet's AC1 Coefficients for", question),
         x = "Gwet's AC1",
         y = "Density") +
    theme_minimal()
  # save as png
  #ggsave("gwet_distribution.png", width = 8, height = 6)
}

# plot the distribution of percentage agreement for dieticians and model
plot_prop_distribution <- function(sims, question) {

  sims_long <- sims %>%
    select(iter, diet1_prop, diet2_prop, diet3_prop, model_prop) %>%
    pivot_longer(cols = -iter, names_to = "rater", values_to = "prop")

  ggplot(sims_long, aes(x = prop, fill = rater)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of Proportion Agreement for", question),
         x = "Proportion Agreement",
         y = "Density") +
    theme_minimal()
  # save as png
  #ggsave("prop_distribution.png", width = 8, height = 6)
}

# plot the distribution of masi coefficients for dieticians and model
plot_masi_distribution <- function(sims, question) {
  sims_long <- sims %>%
    select(iter, diet1_masi, diet2_masi, diet3_masi, model_masi) %>%
    pivot_longer(cols = -iter, names_to = "rater", values_to = "masi")

  ggplot(sims_long, aes(x = masi, fill = rater)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of MASI Coefficients for", question),
         x = "MASI Coefficient",
         y = "Density") +
    theme_minimal()
  # save as png
  #ggsave("masi_distribution.png", width = 8, height = 6)
}

# plot distribution of jaccard similarity for dieticians and model
plot_jacc_distribution <- function(sims, question) {
  sims_long <- sims %>%
    select(iter, diet1_jacc, diet2_jacc, diet3_jacc, model_jacc) %>%
    pivot_longer(cols = -iter, names_to = "rater", values_to = "jacc")

  ggplot(sims_long, aes(x = jacc, fill = rater)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of Jaccard Similarity for", question),
         x = "Jaccard Similarity",
         y = "Density") +
    theme_minimal()
  # save as png
  #ggsave("jacc_distribution.png", width = 8, height = 6)
}


# SINGLE-CHOICE QUESTIONS
results_single <- lapply(single_choice_vars, function(question) {
  print(paste("Processing question:", question))
  bootstrap_agreement(models = list(GPT = gpt), responses_human_all, question, responses_dieticians, 1000, "single")
})

names(results_single) <- single_choice_vars

wb <- createWorkbook()
for (question in names(results_single)) {
  addWorksheet(wb, question)
  writeData(wb, question, results_single[[question]]$sims)
}
saveWorkbook(wb, "gpu outputs/plots/bootstrap_agreement_single_choice.xlsx", overwrite = TRUE)

plot_gwet_distribution(results_single$alcohol$sims, "Alcohol")
plot_prop_distribution(results_single$alcohol$sims, "Alcohol")


# run one by one because it takes a while
# temp3 <- bootstrap_agreement(models, responses_human_all, "who_cat_clean", responses_dieticians, 1000, "multi")
# results_multi <- list(temp, temp2, temp3)

# MULTI-CHOICE QUESTIONS
results_multi <- lapply(multi_label_vars, function(question) {
  print(paste("Processing question:", question))
  bootstrap_agreement(models = list(GPT = gpt), responses_human_all, question, responses_dieticians, 1000, "multi")
})

names(results_multi) <- multi_label_vars

wb <- createWorkbook()
for (question in names(results_multi)) {
  addWorksheet(wb, question)
  writeData(wb, question, results_multi[[question]]$sims)
}
saveWorkbook(wb, "gpu outputs/plots/bootstrap_agreement_multi_choice.xlsx", overwrite = TRUE)

plot_masi_distribution(results_multi$prem_offer$sims, "Premium Offers")
plot_jacc_distribution(results_multi$prem_offer$sims, "Premium Offers")


# read all results single and multi from the excel files (for later use)
results_single <- setNames(lapply(single_choice_vars, function(question) {
  read_excel(path = file.path(root_folder, "plots", "bootstrap_agreement_single_choice.xlsx"), sheet = question)
}), single_choice_vars)

results_multi <- setNames(lapply(multi_label_vars, function(question) {
  read_excel(path = file.path(root_folder, "plots", "bootstrap_agreement_multi_choice.xlsx"), sheet = question)
}), multi_label_vars)






# ======== FIXED EFFECTS LINEAR REGRESSION ========
# fixed effects: model, question (alcohol, prem_offer), question type (single, multi)
# interactions: model*question, model*question_type
# response: agreement metric (gwet for single, masi for multi)


# build ad x question x model panel from original annotations
build_panel_from_annotations <- function(models_list, dieticians_df) {
  out <- list()
  idx <- 1

  # helper to get human consensus column names used in agreement_functions.R
  for (question in c(single_choice_vars, multi_label_vars)) {
    question_type <- ifelse(question %in% single_choice_vars, "single", "multi")

    # dieticians consensus (agreement_functions defines mapping)
    diet_col <- paste0(question, "_dietcons")
    ai_column <- ai_mapping[[question]]

    for (model_name in names(models_list)) {
      model_df <- models_list[[model_name]]

      # align by img_id and join model predictions with dieticians consensus
      df <- model_df %>%
        select(img_id, !!sym(ai_column)) %>%
        rename(model_pred = !!sym(ai_column)) %>%
        inner_join(dieticians_df %>% select(img_id, !!sym(diet_col)), by = "img_id") %>%
        rename(diet_cons = !!sym(diet_col))

      # compute per-ad agreement score
      if (question_type == "single") {
        df <- df %>% mutate(score = as.numeric(model_pred == diet_cons))
      } else {
        df <- df %>% mutate(score = mapply(function(a, b) jaccard_similarity(a, b), model_pred, diet_cons))
      }

      out[[idx]] <- df %>% mutate(question = question, question_type = question_type, model = model_name)
      idx <- idx + 1
    }
  }

  panel <- bind_rows(out)
  panel
}

# ======== Build panel from original annotations and run FE regression ========
models_list <- list(Gemma = gemma, Pixtral = pixtral, GPT = gpt, Qwen = qwen)

# build the ad x question x model panel
panel <- build_panel_from_annotations(models_list, responses_dieticians)
cat("Panel rows:", nrow(panel), "\n")

# convert categorical variables to factors
panel <- panel %>% mutate(
  question = as.factor(question),
  question_type = as.factor(question_type),
  model = as.factor(model)
)


# fixed effects regression with clustering using fixest
library(fixest)
library(broom)
# 1. FE with ad fixed effects (img_id)
fe_ad <- feols(score ~ model * question + model * question_type | img_id, data = panel)

# clustered SEs by question
summary(fe_ad, cluster = ~ question)

coefs_with <- tidy(fe_ad, conf.int = TRUE)

# 2. FE without ad fixed effects
fe_no_ad <- feols(score ~ model * question + model * question_type, data = panel)
summary(fe_no_ad, cluster = ~ question)

coefs_no <- tidy(fe_no_ad, conf.int = TRUE)

# predicted mean scores per model x question_type
# group-level summaries (observed means)
summary_table <- panel %>%
  group_by(model, question_type) %>%
  summarise(
    n = n(),
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    se = sd_score / sqrt(n),
    ci_low = mean_score - 1.96 * se,
    ci_high = mean_score + 1.96 * se
  ) %>%
  ungroup()

ggplot(summary_table, aes(x = model, y = mean_score, color = question_type)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2,
                position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(y = "Mean agreement score", title = "Average agreement by model and question_type")

# interaction plots (model x question_type) to visually check heterogeneity
ggplot(summary_table, aes(x = question_type, y = mean_score, fill = model)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = 0.9), width = 0.2) +
  theme_minimal() +
  labs(title = "Model x Question type interaction (observed means)")






# using plm
library(plm)
panel_plm <- pdata.frame(panel, index = c("img_id"))
plm_fit <- plm(score ~ model * question + model * question_type,
               data = panel_plm, model = "within", effect = "individual")
summary(plm_fit)
fixef(plm_fit)
