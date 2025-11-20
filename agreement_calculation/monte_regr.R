library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(irr)
library(openxlsx)
library(patchwork)

plot_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/plots/"

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
  #ggsave(paste(plot_folder, "gwet_distribution.jpg", sep = ""), width = 8, height = 6)
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
  #ggsave(paste(plot_folder, "prop_distribution.jpg", sep = ""), width = 8, height = 6)
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
  #ggsave(paste(plot_folder, "masi_distribution.jpg", sep = ""), width = 8, height = 6)
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
  #ggsave(paste(plot_folder, "jacc_distribution.jpg", sep = ""), width = 8, height = 6)
}

# plot the gwet distributions for all three single-choice questions
plot_all_gwet_distributions <- function(results_list, question_names, save = TRUE) {
  # combine all results into one dataframe with question labels
  all_sims <- lapply(seq_along(question_names), function(i) {
    question <- question_names[i]
    results_list[[question]] %>%
      select(iter, diet1_gwet, diet2_gwet, diet3_gwet, model_gwet, in_range) %>%
      mutate(question = question)
  }) %>%
    bind_rows()

  # calculate % in_range for each question
  pct_in_range <- all_sims %>%
    group_by(question) %>%
    summarise(pct = mean(in_range, na.rm = TRUE) * 100) %>%
    mutate(
      question_label = get_question_label(question),
      facet_title = paste0(question_label, "\nGPT in range: ", sprintf("%.1f", pct), "%")
    )

  # pivot longer for plotting
  sims_long <- all_sims %>%
    pivot_longer(cols = c(diet1_gwet, diet2_gwet, diet3_gwet, model_gwet),
                 names_to = "rater", values_to = "gwet") %>%
    mutate(
      rater = factor(rater,
                     levels = c("diet1_gwet", "diet2_gwet", "diet3_gwet", "model_gwet"),
                     labels = c("Dietician 1", "Dietician 2", "Dietician 3", "GPT"))
    )
  # Join facet titles
  sims_long <- sims_long %>%
    left_join(pct_in_range %>% select(question, facet_title), by = "question")

  p <- ggplot(sims_long, aes(x = gwet, fill = rater)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ facet_title, scales = "free_y", ncol = 1) +
    labs(title = "Bootstrap Distribution of Gwet's AC1 Coefficients (B=1000)",
         x = "Gwet's AC1",
         y = "Density",
         fill = "Rater") +
    theme_minimal() +
    theme(legend.position = "bottom")

  if (save)
    ggsave(paste(plot_folder, "monte_carlo_single_vars.jpg", sep = ""), width = 8, height = 6)

  invisible(p)
}

# plot the masi distributions for all three multi-label questions
plot_all_masi_distributions <- function(results_list, question_names, save = TRUE) {
  # combine all results into one dataframe with question labels
  all_sims <- lapply(seq_along(question_names), function(i) {
    question <- question_names[i]
    results_list[[question]] %>%
      select(iter, diet1_masi, diet2_masi, diet3_masi, model_masi, in_range) %>%
      mutate(question = question)
  }) %>%
    bind_rows()

  filter(all_sims, question == "marketing_str")

  # calculate % in_range for each question
  pct_in_range <- all_sims %>%
    group_by(question) %>%
    summarise(pct = mean(in_range, na.rm = TRUE) * 100) %>%
    mutate(
      question_label = get_question_label(question),
      facet_title = paste0(question_label, "\nGPT in range: ", sprintf("%.1f", pct), "%")
    )

  # pivot longer for plotting
  sims_long <- all_sims %>%
    pivot_longer(cols = c(diet1_masi, diet2_masi, diet3_masi, model_masi),
                 names_to = "rater", values_to = "masi") %>%
    mutate(
      rater = factor(rater,
                     levels = c("diet1_masi", "diet2_masi", "diet3_masi", "model_masi"),
                     labels = c("Dietician 1", "Dietician 2", "Dietician 3", "GPT"))
    )

  sims_long <- sims_long %>%
    left_join(pct_in_range %>% select(question, facet_title), by = "question")

  p <- ggplot(sims_long, aes(x = masi, fill = rater)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ facet_title, scales = "free_y", ncol = 1) +
    labs(title = "Bootstrap Distribution of Krippendorff's Alpha Coefficients (B=1000)",
         x = "Krippendorff's Alpha",
         y = "Density",
         fill = "Rater") +
    theme_minimal() +
    theme(legend.position = "bottom")

  if (save)
    ggsave(paste(plot_folder, "monte_carlo_multi_vars.jpg", sep = ""), width = 8, height = 6)

  invisible(p)
}

# combined plot for both single-choice and multi-label distributions in one figure
plot_combined_distributions <- function(results_single, single_vars, results_multi, multi_vars, save = FALSE) {

  p1 <- plot_all_gwet_distributions(results_single, single_vars, save = FALSE)
  p2 <- plot_all_masi_distributions(results_multi, multi_vars, save = FALSE)

  p1 <- p1 + labs(title = "Single-Option: Gwet's AC1")
  p2 <- p2 + labs(title = "Multi-Option: Krippendorff's Alpha")

  # combine plots side-by-side with shared legend
  combined <- p1 + p2 +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  if (save)
    ggsave(paste(plot_folder, "monte_carlo_combined.jpg", sep = ""), plot = combined, width = 14, height = 6)

  combined
}


# ======== OPTION BIAS WITHIN EACH QUESTION (label-wise z-tests) ========
# quantify for each question and label how much each model over/under-selects an option relative to human consensus
# positive bias -> model selects the label more often than humans (over-flag), negative -> under-flag.

# split a multi-label string into a set
split_labels <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  # split by comma (with or without space after it)
  labs <- unlist(strsplit(as.character(x), ",\\s*"))
  # trim whitespace and remove empty strings
  labs <- trimws(labs)
  labs[nzchar(labs)]
}

# get the set of labels for a question from human consensus and model outputs
collect_label_set <- function(question, models_list, dieticians_df) {

  diet_col <- paste0(question, "_dietcons")
  ai_col <- ai_mapping[[question]]
  # from humans
  human_vals <- dieticians_df[[diet_col]]
  # from models (concatenate across models)
  model_vals <- unlist(lapply(models_list, function(df) df[[ai_col]]))
  vals <- unique(c(human_vals, model_vals))
  vals <- vals[!is.na(vals)]

  if (question %in% multi_label_vars) {
    # split all multi-label strings and get unique individual labels
    all_labels <- unlist(lapply(vals, split_labels))
    sort(unique(all_labels))
  } else {
    sort(unique(as.character(vals)))
  }
}

# compute bias table for one question
compute_option_bias <- function(question, models_list, dieticians_df) {
  # diet_col is either _dietcons or _cons
  diet_col <- grep(paste0("^", question, "_(cons|dietcons)$"), names(dieticians_df), value = TRUE)
  ai_col <- ai_mapping[[question]]
  labels <- collect_label_set(question, models_list, dieticians_df)
  labels <- labels[labels != "-1"] # remove the -1 label

  out <- list()
  for (m_name in names(models_list)) {
    mdl <- models_list[[m_name]]
    # join on img_id to align - use character column names with sym()
    df <- mdl %>%
      select(img_id, model_pred = !!sym(ai_col)) %>%
      inner_join(
        dieticians_df %>% select(img_id, diet_cons = !!sym(diet_col)),
        by = "img_id"
      )

    # for each label, compute bias = mean(1[model selects] - 1[consensus selects])
    for (lab in labels) {
      # remove the "other" labels in marketing_str (11) and prem_offer (10) for diet_cons
      if (question == "marketing_str") {
        if (lab == "11") next
      } else if (question == "prem_offer") {
        if (lab == "10") next
      }

      if (question %in% multi_label_vars) {
        y_hat <- vapply(df$model_pred, function(s) lab %in% split_labels(s), logical(1))
        y_true <- vapply(df$diet_cons, function(s) lab %in% split_labels(s), logical(1))
      } else {
        y_hat <- as.character(df$model_pred) == lab
        y_true <- as.character(df$diet_cons) == lab
      }
      d <- as.numeric(y_hat) - as.numeric(y_true)
      n <- sum(!is.na(d))
      est <- mean(d, na.rm = TRUE)
      se <- sd(d, na.rm = TRUE) / sqrt(n)
      z <- ifelse(se > 0, est / se, NA_real_)
      p <- ifelse(is.na(z), NA_real_, 2 * pnorm(abs(z), lower.tail = FALSE))
      ci_low <- est - 1.96 * se
      ci_high <- est + 1.96 * se
      out[[length(out) + 1]] <- tibble(
        question = question,
        label = lab,
        model = m_name,
        bias = est,
        se = se,
        conf.low = ci_low,
        conf.high = ci_high,
        p.value = p,
        n = n
      )
    }
  }
  bind_rows(out)
}
# run this later with the FE regressions, so we plot the 2 heatmaps together


# ======== LABEL-LEVEL FIXED EFFECTS REGRESSION ========
# panel: ad xd model x label, with label fixed effects that control for label difficulty
library(fixest)
library(broom)

# build label-level panel for one question
build_label_panel <- function(question, models_list, dieticians_df) {
  diet_col_name <- grep(paste0("^", question, "_(cons|dietcons)$"), names(dieticians_df), value = TRUE)
  ai_col_name <- as.character(ai_mapping[[question]])
  labels <- collect_label_set(question, models_list, dieticians_df)

  out <- list()

  # get human consensus data for joining
  human_data <- dieticians_df %>%
    dplyr::select(img_id, human_pred = !!sym(diet_col_name))

  for (m_name in names(models_list)) {
    df <- models_list[[m_name]] %>%
      dplyr::select(img_id, model_pred = !!sym(ai_col_name)) %>%
      inner_join(human_data, by = "img_id")

    # for each label, create binary indicators for model AND human
    for (lab in labels) {
      if (question %in% multi_label_vars) {
        df_lab <- df %>%
          mutate(
            model_selected = vapply(model_pred,
                                    function(s) lab %in% split_labels(s),
                                    logical(1)),
            human_selected = vapply(human_pred,
                                    function(s) lab %in% split_labels(s),
                                    logical(1)),
            # compute difference model - human (the bias measure)
            selected_diff = as.numeric(model_selected) - as.numeric(human_selected),
            label = lab,
            model = m_name,
            question = question
          )
      } else {
        df_lab <- df %>%
          mutate(
            model_selected = as.character(model_pred) == lab,
            human_selected = as.character(human_pred) == lab,
            selected_diff = as.numeric(model_selected) - as.numeric(human_selected),
            label = lab,
            model = m_name,
            question = question
          )
      }
      out[[length(out) + 1]] <- df_lab %>%
        dplyr::select(img_id, model, question, label, model_selected, human_selected, selected_diff)
    }
  }

  bind_rows(out)
}

# run label-level FE regression for one question
label_fe_regression <- function(question, models_list, dieticians_df, outdoor = FALSE) {
  print(paste("Running label FE regression for question:", question))
  panel <- build_label_panel(question, models_list, dieticians_df)
  panel <- panel %>% filter(label != "-1") %>% arrange(img_id)

  # filter out the "other" labels in marketing_str (11) and prem_offer (10)
  if (question == "marketing_str") {
    panel <- panel %>% filter(label != "11")
  } else if (question == "prem_offer") {
    panel <- panel %>% filter(label != "10")
  }

  # set correct reference levels for factors
  ref_label <- get_reference_level(question)
  # convert to factors with GPT as reference
  panel <- panel %>%
    mutate(
      model = relevel(as.factor(model), ref = "GPT"),
      label = relevel(as.factor(label), ref = ref_label)
    )

  # FE regression with label fixed effects
  # DV: selected_diff = model selection - human selection (ranges from -1 to +1)
  library(plm)
  fit_gpt2 <- plm(selected_diff ~ label, index = "img_id", data = panel %>% filter(model == "GPT"), model = "random")
  fit_gpt <- feols(selected_diff ~ label | img_id, data = panel %>% filter(model == "GPT"))

  if (outdoor) {
    return(list(
      fit_gpt = fit_gpt,
      fit_gpt2 = summary(fit_gpt2),
      panel = panel,
      question = question
    ))
  }

  # coefficient = average bias difference relative to GPT, controlling for label base rate
  fe_fit_ad <- feols(selected_diff ~ model * label | img_id, data = panel, cluster = ~label)
  fit_qwen <- feols(selected_diff ~ label | img_id, data = panel %>% filter(model == "Qwen"))
  fit_pixtral <- feols(selected_diff ~ label | img_id, data = panel %>% filter(model == "Pixtral"))
  fit_gemma <- feols(selected_diff ~ label | img_id, data = panel %>% filter(model == "Gemma"))

  list(
    fit_gpt = fit_gpt,
    fit_gpt2 = summary(fit_gpt2),
    fit_qwen = fit_qwen,
    fit_pixtral = fit_pixtral,
    fit_gemma = fit_gemma,
    fit_ad = fe_fit_ad,
    panel = panel,
    question = question
  )
}

# visualize label FE results
plot_label_fe_heatmap <- function(fe_result, save = TRUE, outdoor = FALSE) {
  # combine label-level coefficients from all individual model regressions
  if (outdoor) {
    model_fits <- list(
      GPT = fe_result$fit_gpt
    )
  } else {
    model_fits <- list(
      GPT = fe_result$fit_gpt,
      Qwen = fe_result$fit_qwen,
      Pixtral = fe_result$fit_pixtral,
      Gemma = fe_result$fit_gemma
    )
  }

  # determine the reference label from the factor levels
  ref_label <- levels(fe_result$panel$label)[1]

  # non-intercept (differences vs. reference) coefficients
  coef_list <- lapply(names(model_fits), function(m) {
    tidy(model_fits[[m]], conf.int = TRUE) %>%
      mutate(
        model = m,
        label = gsub("label", "", term)
      )
  })

  all_coefs <- bind_rows(coef_list)

  # join with label names
  all_coefs <- all_coefs %>%
    left_join(
      label_df %>%
        filter(category == fe_result$question) %>%
        select(code, text_label),
      by = c("label" = "code")
    ) %>%
    mutate(
      label_name = ifelse(is.na(text_label), label, text_label),
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )

  q_label <- get_question_label(fe_result$question)
  # get the name of the label from label_df
  ref_label_name <- label_df %>%
    filter(category == fe_result$question, code == ref_label) %>%
    pull(text_label)

  # add reference rows manually (estimate = 0 by definition, as it's the baseline)
  ref_rows <- expand.grid(
    model = if (outdoor) c("GPT") else c("GPT", "Qwen", "Pixtral", "Gemma"),
    label = ref_label,
    label_name = ref_label_name,
    estimate = 0,
    sig = "",
    stringsAsFactors = FALSE
  )
  all_coefs <- bind_rows(all_coefs, ref_rows) %>% arrange(model, label_name)

  # check if there is missing combinations by looking at all labels in label_df for this question
  expected_labels <- label_df %>%
    filter(category == fe_result$question) %>%
    pull(text_label)
  missing_labels <- setdiff(expected_labels, unique(all_coefs$label_name))
  
  # add missing labels with NA estimates
  if (length(missing_labels) > 0) {
    for (m in if (outdoor) c("GPT") else c("GPT", "Qwen", "Pixtral", "Gemma")) {
      for (lab in missing_labels) {
        all_coefs <- bind_rows(all_coefs, tibble(
          model = m,
          label = NA,
          label_name = lab,
          estimate = NA,
          sig = ""
        ))
      }
    }
  }

  # lock the reference label at the top of the y-axis
  # ggplot draws the last factor level at the top, so put the reference last
  levels_y <- sort(unique(all_coefs$label_name), decreasing = FALSE)
  levels_y <- c(setdiff(levels_y, ref_label_name), ref_label_name)
  all_coefs <- all_coefs %>%
    mutate(label_name = factor(label_name, levels = levels_y))

  p <- ggplot(all_coefs, aes(x = model, y = label_name, fill = estimate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(sprintf("%.2f", estimate), sig)),
              color = "black", size = 3) +
    scale_fill_gradient2(
      low = "#d73027", mid = "white", high = "red",
      midpoint = 0,
      name = "Bias"
    ) +
    labs(
      title = paste("Label-level bias heatmap by model -", q_label),
      subtitle = paste("FE regression coefficients (per model, controlling for ad FE). Reference:", ref_label_name),
      x = "Model",
      y = "Label"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 11),
      plot.subtitle = element_text(size = 9)
    )

  fn <- paste0("label_fe_", fe_result$question, ".jpg")
  if (save)
    ggsave(fn, plot = p, width = 6, height = 5)
  list(plot = p, data = all_coefs)
}
