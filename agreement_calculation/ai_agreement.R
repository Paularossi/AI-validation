
library(readxl)
library(dplyr)
library(irr)
library(tidyr)
library(ggplot2)
library(boot)

root_folder <- "C:/Users/P70090005/Documents/AI-validation/gpu outputs/"

###### START ANALYSIS HERE

# LOAD THE AI-LABELLED DATA
gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))
models <- list(gemma = gemma, pixtral = pixtral, gpt = gpt, qwen = qwen)

# dig_coding <- read_excel(paste(root_folder, "digital_coding_clean.xlsx", sep=""))
# dig_coding <- dig_coding[dig_coding$img_id %in% gemma$img_id, ]
# dig_coding <- dig_coding[order(dig_coding$img_id), ]
# dig_coding[] <- lapply(dig_coding, as.character)


# ==============================================================================
# VALIDATION OF SINGLE-CHOICE COLUMNS

compare_agreement <- function(colname, models) {
  # filter the models that contain the colname
  valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
  
  # extract the relevant column from each model into a named list
  model_cols <- lapply(valid_models, function(df) df[[colname]])
  names(model_cols) <- names(valid_models)
  
  df <- as.data.frame(model_cols)
  n <- nrow(df)
  
  # compute percentage agreement (full agreement rows only)
  percent_agreement <- mean(apply(df, 1, function(x) length(unique(x)) == 1)) * 100
  
  # pairwise Cohen's Kappa
  pairwise <- combn(names(df), 2, simplify = FALSE)
  kappa_results <- setNames(
    lapply(pairwise, function(p) {
      kappa2(df[, p])$value
    }),
    sapply(pairwise, function(p) paste("kappa", p[1], p[2], sep = "_"))
  )
  
  # Fleiss' Kappa (over all models)
  fleiss_val <- kappam.fleiss(as.matrix(df))$value
  
  return(list(
    column = colname,
    percent_agreement = percent_agreement,
    pairwise_kappa = kappa_results,
    fleiss_kappa = fleiss_val
  ))
}

summarize_agreement <- function(result_list) {
  # find all pairwise keys across all results
  all_keys <- unique(unlist(lapply(result_list, function(res) names(res$pairwise_kappa))))
  
  # build a uniform dataframe for each result
  rows <- lapply(result_list, function(res) {
    kappa_vals <- res$pairwise_kappa
    # fill missing keys with NA
    for (k in setdiff(all_keys, names(kappa_vals))) {
      kappa_vals[[k]] <- NA
    }
    kappa_df <- as.data.frame(as.list(kappa_vals))[all_keys]  # enforce column order
    data.frame(
      question = res$column,
      percent_agreement = res$percent_agreement,
      fleiss_kappa = res$fleiss_kappa,
      kappa_df
    )
  })
  
  do.call(rbind, rows)
}

questions <- c("type_ad", "new_type_ad", "target_group", "is_alcohol")
all_results <- lapply(questions, function(q) {compare_agreement(q, models)})
names(all_results) <- questions

agreement_summary <- summarize_agreement(all_results)

print(agreement_summary)
# proportion of ads where all raters gave exactly the same answer
# fleiss kappa - agreement beyond chance, across all raters

agreement_long <- agreement_summary %>%
  pivot_longer(
    cols = starts_with("kappa_"),
    names_to = "pair",
    values_to = "kappa"
  )


ggplot(agreement_long, aes(x = pair, y = question, fill = kappa)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(kappa, 2)), size = 3, color = "black") +
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0.5, 
                       limits = c(0,1), name = "Cohen's Kappa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pairwise Agreement Across AI Models", x = "Model Pair", y = "Question")

ggsave(paste(root_folder, "plots/pairwise_kappa_heatmap.png", sep=""), width = 9, height = 5)


ggplot(agreement_summary, aes(x = reorder(question, fleiss_kappa), y = fleiss_kappa)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Fleiss' Kappa per Question", x = "Question", y = "Fleiss' Kappa") +
  theme_minimal()

ggsave(paste(root_folder, "tt/fleiss_kappa_barplot.png", sep=""), width = 8, height = 5)



# ==============================================================================
# compute the agreement using bootstrapping
bootstrap_pairwise_kappa <- function(models, column, R = 1000) {
  model_names <- names(models)
  pairs <- combn(model_names, 2, simplify = FALSE)
  
  all_results <- list()
  
  for (pair in pairs) {
    model1 <- models[[pair[1]]][, c("img_id", column)]
    model2 <- models[[pair[2]]][, c("img_id", column)]
    
    merged <- inner_join(model1, model2, by = "img_id", suffix = c("_1", "_2")) %>%
      filter(!is.na(.data[[paste0(column, "_1")]]) & !is.na(.data[[paste0(column, "_2")]]))
    
    ratings <- merged[, c(paste0(column, "_1"), paste0(column, "_2"))]
    
    # convert to character to avoid level mismatch
    ratings[] <- lapply(ratings, as.character)
    
    # bootstrap function: resample rows and recompute kappa
    boot_fn <- function(data, indices) {
      kappa2(data[indices, ])$value
    }
    
    boot_result <- boot(data = ratings, statistic = boot_fn, R = R)
    
    result_df <- data.frame(
      question = column,
      model_pair = paste(pair, collapse = "_"),
      mean_kappa = mean(boot_result$t),
      ci_lower = quantile(boot_result$t, 0.025),
      ci_upper = quantile(boot_result$t, 0.975)
    )
    
    all_results[[paste(pair, collapse = "_")]] <- result_df
  }
  
  bind_rows(all_results)
}

kappa_boot_all <- bind_rows(
  lapply(questions, function(var) {
    bootstrap_pairwise_kappa(models, column = var, R = 1000)
  })
)


# plot both kappas together
kappa_point_df <- agreement_long %>%
  mutate(model_pair = gsub("kappa_", "", pair) %>% 
           gsub("_", "_", .)  # ensures same format as in bootstrap
  ) %>%
  select(model_pair, point_kappa = kappa, question)

kappa_combined <- left_join(kappa_boot_all, kappa_point_df, by = c("question", "model_pair"))

ggplot(kappa_combined, aes(x = model_pair, y = mean_kappa, fill = question)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_point(aes(y = point_kappa), color = "red",
             position = position_dodge(width = 0.8), size = 2.5) +
  geom_text(aes(y = point_kappa, label = round(point_kappa, 2)),
            position = position_dodge(width = 0.8), vjust = -1.5,
            color = "red", size = 3) +
  labs(
    title = "Model Agreement Across Single-Choice Questions",
    subtitle = "Red dots = regular Kappa | Bars = bootstrapped mean ± 95% CI",
    x = "Model Pair", y = "Cohen's Kappa"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  facet_wrap(~ question) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste(root_folder, "tt/single_choice_bootsr_kappa.png", sep=""), width = 8, height = 6)



# ==============================================================================
# VALIDATION OF MULTIPLE CHOICE COLUMNS
jaccard_similarity <- function(str1, str2) {
  set1 <- trimws(unlist(strsplit(str1, ",")))
  set2 <- trimws(unlist(strsplit(str2, ",")))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  if (union == 0) return(NA)  # to handle empty annotations
  return(intersection / union)
}


compare_multilabel_agreement <- function(colname, models) {
  
  valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
  
  # extract the relevant column from each model into a named list
  model_cols <- lapply(valid_models, function(df) df[[colname]])
  names(model_cols) <- names(valid_models)
  
  df <- as.data.frame(model_cols)
  model_pairs <- combn(names(df), 2, simplify = FALSE)
  
  # compute average Jaccard similarity for each pair
  avg_pairwise_jaccard <- sapply(model_pairs, function(pair) {
    sims <- mapply(jaccard_similarity, df[[pair[1]]], df[[pair[2]]])
    mean(sims, na.rm = TRUE)
  })
  
  names(avg_pairwise_jaccard) <- sapply(model_pairs, function(p) paste(p, collapse = "_"))
  
  return(list(
    column = colname,
    pairwise_jaccard = avg_pairwise_jaccard
  ))
}
  
multi_label_cols <- c("marketing_str", "prem_offer", "who_cat", "new_who_cat")

multi_results <- lapply(multi_label_cols, function(col) {
  compare_multilabel_agreement(col, models)
})
names(multi_results) <- multi_label_cols

multi_summary <- bind_rows(lapply(multi_results, function(res) {
  data.frame(
    question = res$column,
    t(as.data.frame(res$pairwise_jaccard))
  )
}), .id = NULL)ge

multi_long <- multi_summary %>%
  pivot_longer(
    cols = -question,
    names_to = "pair",
    values_to = "jaccard"
  )

ggplot(multi_long, aes(x = pair, y = question, fill = jaccard)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(jaccard, 2)), size = 3) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5,
                       limits = c(0, 1), name = "Jaccard Similarity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pairwise Agreement on Multi-Label Columns", x = "Model Pair", y = "Question")

ggsave(paste(root_folder, "tt/jaccard_multilabel_heatmap.png", sep=""), width = 9, height = 5)


# using intersection-over-union distance (jaccard)
iou_distance <- function(str1, str2) {
  sim <- jaccard_similarity(str1, str2)
  if (is.na(sim)) return(NA)
  return(1 - sim)
}


compare_krippendorff_alpha_multilabel <- function(colname, models) {
  valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
  
  df_all <- bind_rows(lapply(valid_models, function(df) {
    df %>% select(img_id, model, all_of(colname))
  }))
  
  # split marketing_str into separate rows per label
  df_long <- df_all %>%
    mutate(!!colname := gsub("\\s+", "", .data[[colname]])) %>%  # remove any spaces
    separate_rows(!!sym(colname), sep = ",")
  
  # create binary data
  df_binary <- df_long %>%
    mutate(value = 1,
           label = paste0("label_", .data[[colname]])) %>%
    select(img_id, model, label, value) %>%
    pivot_wider(names_from = label, values_from = value, values_fill = 0) %>%
    arrange(img_id, model)
  
  models_used <- unique(df_binary$model)
  model_pairs <- combn(models_used, 2, simplify = FALSE)
  
  label_cols <- unique(unlist(strsplit(df_all[[colname]], ", ")))
  label_cols <- paste0("label_", label_cols[!is.na(label_cols)])
  
  # for each label, reshape to matrix: rows = items, cols = raters/models
  # alphas <- sapply(label_cols, function(label) {
  #   df_label <- df_binary %>%
  #     select(img_id, model, !!label) %>%
  #     pivot_wider(names_from = model, values_from = !!label) %>%
  #     select(-img_id)
  #   
  #   kripp.alpha(t(as.matrix(df_label)), method = "nominal")$value
  # })
  
  # loop over pairs and labels
  result_list <- list()
  for (pair in model_pairs) {
    m1 <- pair[1]
    m2 <- pair[2]
    
    pair_df <- df_binary %>%
      filter(model %in% c(m1, m2)) %>%
      arrange(img_id, model)
    
    for (label in label_cols) {
      mat <- pair_df %>%
        select(img_id, model, !!label) %>%
        pivot_wider(names_from = model, values_from = !!label) %>%
        select(-img_id)
      
      if (ncol(mat) == 2 && nrow(mat) > 0) {
        alpha_val <- kripp.alpha(t(as.matrix(mat)), method = "nominal")$value
        result_list[[length(result_list) + 1]] <- data.frame(
          variable = colname,
          model_pair = paste(m1, m2, sep = "_vs_"),
          label = gsub("^label_", "", label),
          kripp_alpha = alpha_val
        )
      }
    }
  }
  
  result_df <- do.call(rbind, result_list)
  
  # result_df <- data.frame(
  #   variable = rep(colname, length(alphas)),
  #   label = gsub("^label_", "", names(alphas)),
  #   avg_kripp_alpha = mean(alphas),
  #   kripp_alpha = as.numeric(alphas)
  # )
  
  return(result_df)
}

multi_kripp_results <- lapply(multi_label_cols, function(col) {
  compare_krippendorff_alpha_multilabel(col, models)
})
multi_kripp_results <- bind_rows(multi_kripp_results)

pairwise_sumary <- multi_kripp_results %>%
  group_by(variable, model_pair) %>%
  summarise(avg_kripp_alpha = mean(kripp_alpha, na.rm = TRUE), .groups = "drop")

ggplot(pairwise_summary, aes(x = model_pair, y = variable, fill = avg_kripp_alpha)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(avg_kripp_alpha, 2)), color = "black") +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5,
                       limits = c(0, 1), name = "Krippendorf's Alpha") +
  labs(title = "Avg Krippendorff's Alpha Across All Multi-Label Questions",
       x = "Model Pair", y = "Question") +
  theme_minimal() +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




compare_iou_krippendorff_alpha <- function(colname, models) {
  valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
  
  df_all <- bind_rows(lapply(valid_models, function(df) {
    df %>% select(img_id, model, all_of(colname))
  }))
  
  # compute distances per image
  all_ids <- unique(df_all$img_id)
  models_used <- unique(df_all$model)
  model_pairs <- combn(models_used, 2, simplify = FALSE)
  
  distance_matrix <- data.frame()
  
  for (img in all_ids) {
    df_img <- df_all %>% filter(img_id == img)
    for (pair in model_pairs) {
      m1 <- pair[1]; m2 <- pair[2]
      l1 <- df_img %>% filter(model == m1) %>% pull(!!colname)
      l2 <- df_img %>% filter(model == m2) %>% pull(!!colname)
      
      if (length(l1) == 0 || length(l2) == 0) next
      
      dist <- iou_distance(l1, l2)
      distance_matrix <- rbind(distance_matrix, data.frame(
        img_id = img,
        model_pair = paste(m1, m2, sep = "_vs_"),
        distance = dist
      ))
    }
  }
  
  # pivot to rater-pair × item matrix
  df_wide <- distance_matrix %>%
    pivot_wider(names_from = img_id, values_from = distance)
  
  # Custom Krippendorff’s Alpha (interval version)
  kripp_alpha_interval <- function(data_matrix) {
    data_matrix <- as.matrix(data_matrix)
    Do <- 0; De <- 0; N <- 0
    for (i in seq(ncol(data_matrix))) {
      col <- data_matrix[, i]
      col <- col[!is.na(col)]
      if (length(col) < 2) next
      combs <- combn(col, 2)
      Do <- Do + sum((combs[1, ] - combs[2, ])^2)
      N <- N + ncol(combs)
    }
    if (N == 0) return(NA)
    Do <- Do / N
    
    all_vals <- unlist(data_matrix)
    all_vals <- all_vals[!is.na(all_vals)]
    if (length(all_vals) < 2) return(NA)
    all_combs <- combn(all_vals, 2)
    De <- sum((all_combs[1, ] - all_combs[2, ])^2) / ncol(all_combs)
    
    return(1 - Do / De)
  }
  
  alpha_val <- kripp_alpha_interval(df_wide %>% select(-model_pair))
  
  return(data.frame(
    variable = colname,
    kripp_alpha_iou = alpha_val
  ))
}




# ==============================================================================
# apply again bootstrapping
bootstrap_pairwise_jaccard <- function(models, column, R = 1000) {
  model_names <- names(models)
  pairs <- combn(model_names, 2, simplify = FALSE)
  
  all_results <- list()
  
  for (pair in pairs) {
    model1 <- models[[pair[1]]][, c("img_id", column)]
    model2 <- models[[pair[2]]][, c("img_id", column)]
    
    merged <- inner_join(model1, model2, by = "img_id", suffix = c("_1", "_2")) %>%
      filter(!is.na(.data[[paste0(column, "_1")]]) & !is.na(.data[[paste0(column, "_2")]]))
    
    if (nrow(merged) == 0) next  # skip empty pairs
    
    # compute jaccard for all rows
    merged$jaccard <- mapply(jaccard_similarity,
                             merged[[paste0(column, "_1")]],
                             merged[[paste0(column, "_2")]])
    
    # bootstrap: sample row indices, compute mean jaccard
    boot_fn <- function(data, indices) {
      mean(data$jaccard[indices], na.rm = TRUE)
    }
    
    boot_result <- boot(data = merged, statistic = boot_fn, R = R)
    
    result_df <- data.frame(
      model_pair = paste(pair, collapse = "_"),
      mean_jaccard = mean(boot_result$t),
      ci_lower = quantile(boot_result$t, 0.025, na.rm = TRUE),
      ci_upper = quantile(boot_result$t, 0.975, na.rm = TRUE)
    )
    
    all_results[[paste(pair, collapse = "_")]] <- result_df
  }
  
  bind_rows(all_results)
}


jaccard_boot_summary <- bootstrap_pairwise_jaccard(models, column = "new_who_cat", R = 1000)

print(jaccard_boot_summary)
ggplot(jaccard_boot_summary, aes(x = model_pair, y = mean_jaccard)) +
  geom_col(fill = "darkorange") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Bootstrapped Pairwise Jaccard Similarity for WHO Categories",
       x = "Model Pair", y = "Jaccard Similarity") +
  theme_minimal()

ggsave(paste(root_folder, "tt/bootstr_kappa_who_cat.png", sep=""), width = 8, height = 5)



# ==============================================================================
# analyze the multi-label columns by answer choice individually

# function to create dummy vars for a given column
create_dummy_vars <- function(df, all_cats, column_to_sep) {
  df %>%
    separate_rows(!!sym(column_to_sep), sep = ", ") %>%
    mutate(value = 1) %>%
    pivot_wider(names_from = !!sym(column_to_sep), values_from = value, 
                values_fill = 0, names_prefix = paste0(column_to_sep, "_")) %>%
    { 
      existing_cats <- gsub(paste0(column_to_sep, "_"), "", names(.))
      missing_cats <- setdiff(all_cats, existing_cats)
      for (cat in missing_cats) {
        .[[paste0(column_to_sep, "_", cat)]] <- 0
      }
      .
    } %>%
    select(img_id, starts_with(paste0(column_to_sep, "_")))
}

# calculate Kappa for each category
compare_multilabel_kappa <- function(df1, df2, column_name, label1 = "model1", label2 = "model2") {
  # get all unique categories across both datasets
  all_cats <- unique(c(
    unlist(strsplit(df1[[column_name]], ", ")),
    unlist(strsplit(df2[[column_name]], ", "))
  ))
  
  # create aligned dummy matrices
  dummy1 <- create_dummy_vars(df1, all_cats, column_name)
  dummy2 <- create_dummy_vars(df2, all_cats, column_name)
  
  # merge by img_id
  merged <- full_join(dummy1, dummy2, by = "img_id", suffix = c(paste0("_", label1), paste0("_", label2)))
  
  # compute kappa per category
  results <- lapply(all_cats, function(cat) {
    col1 <- paste0(column_name, "_", cat, "_", label1)
    col2 <- paste0(column_name, "_", cat, "_", label2)
    
    if (!(col1 %in% names(merged)) || !(col2 %in% names(merged))) {
      return(data.frame(category = cat, kappa = NA, p_value = NA))
    }
    
    kapp <- kappa2(cbind(merged[[col1]], merged[[col2]]))
    data.frame(category = cat, kappa = kapp$value, p_value = kapp$p.value)
  })
  
  do.call(rbind, results)
}

# network model for agreement (check the AI polling paper)
all_offers_text <- c("0" = "None", "1" = "App downloads", "2" = "Contests", "3" = "Pay 2 take 3", "4" = "20% extra",
                     "5" = "Limited edition", "6" = "Social charity", "7" = "Gifts", "8" = "Price discount", "9" = "Loyalty programs")

############## PREMIUM OFFERS
kappa_pixtral_vs_gpt <- compare_multilabel_kappa(pixtral, gpt, "prem_offer", "pixtral", "gpt")
kappa_gemma_vs_gpt <- compare_multilabel_kappa(gemma, gpt, "prem_offer", "gemma", "gpt")

# look at them together
kappa_pixtral_vs_gpt$compared_to <- "pixtral"
kappa_gemma_vs_gpt$compared_to <- "gemma"
kappa_combined <- rbind(kappa_pixtral_vs_gpt, kappa_gemma_vs_gpt)

ggplot(kappa_combined, aes(x = all_offers_text[category], y = kappa, fill = compared_to)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_text(aes(label = round(p_value, 3)), position = position_dodge(0.9), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Premium Offers Agreement of GPT",
       x = "Category", y = "Kappa", fill = "Compared to") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))
ggsave(paste(root_folder, "tt/prem_offers_gpt_gemma_pixt.png", sep=""), width = 8, height = 5)


############## MARKETING STRATEGIES
all_marketing_str_text <- c("0" = "None", "1" = "Cartoons", "2" = "Licensed char", "3" = "Amateur sports", "4" = "Celebrity",
                            "5" = "Movie char", "6" = "Famous sports", "7" = "Events", "8" = "Age-targeted", 
                            "9" = "Awards", "10" = "Sport events")

kappa_gpt_pixtral <- compare_multilabel_kappa(gpt, pixtral, "marketing_str", "gpt", "pixtral")
kappa_gpt_gemma <- compare_multilabel_kappa(gpt, gemma, "marketing_str", "gpt", "gemma")

kappa_gpt_pixtral$compared_to <- "pixtral"
kappa_gpt_gemma$compared_to <- "gemma"

#kappa_qwen_pixtral$compared_to <- "pixtral"
kappa_combined <- rbind(kappa_gpt_pixtral, kappa_gpt_gemma)

ggplot(kappa_combined, aes(x = all_marketing_str_text[category], y = kappa, fill = compared_to)) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_text(aes(label = round(p_value, 3)), position = position_dodge(0.9), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Marketing Strategies Agreement of Qwen",
       x = "Category", y = "Kappa", fill = "Compared to") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))
ggsave(paste(root_folder, "tt/marketing_str_gpt_gemma_pixt.png", sep=""), width = 9, height = 5)


table(dig_coding$marketing_str)


# ==============================================================================
# MORE ANALYSIS - by category of the brand
# !!! run the below lines only once - not needed anymore since we save un updated excel

# cats <- read_excel("C:/Users/P70090005/Documents/AI-validation/data/3000_ads_by_cat_language.xlsx")
# 
# gpt <- cats %>% select(c(img_id, category)) %>% 
#   right_join(gpt, by = "img_id")
# 
# gemma <- cats %>% select(c(img_id, category)) %>% 
#   right_join(gemma, by = "img_id")
# 
# pixtral <- cats %>% select(c(img_id, category)) %>% 
#   right_join(pixtral, by = "img_id")
# 
# write_xlsx(gpt, paste(root_folder, "gpt_all_1000.xlsx", sep=""))
# write_xlsx(gemma, paste(root_folder, "gemma_all_1000.xlsx", sep=""))
# write_xlsx(pixtral, paste(root_folder, "pixtral_all_1000.xlsx", sep=""))


# how consistent are AI models in different product categories?


agreement_by_category <- function(models, column) {
  model_names <- names(models)
  pairs <- combn(model_names, 2, simplify = FALSE)
  all_results <- list()
  
  for (pair in pairs) {
    model1 <- models[[pair[1]]][, c("img_id", column, "category")]
    model2 <- models[[pair[2]]][, c("img_id", column, "category")]
    
    merged <- inner_join(model1, model2, by = c("img_id", "category"), suffix = c("_1", "_2")) %>%
      filter(!is.na(.data[[paste0(column, "_1")]]) & !is.na(.data[[paste0(column, "_2")]])) %>%
      mutate(across(starts_with(column), as.character))
    
    if (nrow(merged) == 0) next
    
    kappa_by_cat <- merged %>%
      group_by(category) %>%
      summarise(
        kappa = if (n() >= 5) kappa2(select(pick(everything()), ends_with("_1"), ends_with("_2")))$value else NA_real_,
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(model_pair = paste(pair, collapse = "_"))
    
    all_results[[paste(pair, collapse = "_")]] <- kappa_by_cat
  }
  
  bind_rows(all_results)
}

column <- "new_type_ad"
category_kappas <- agreement_by_category(models, column = column)


ggplot(category_kappas, aes(x = category, y = kappa, fill = model_pair)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(kappa, 2)), position = position_dodge(0.8), vjust = -0.3, size = 3) +
  labs(
    title = "Model Agreement by Brand Category",
    subtitle = paste0("Cohen's Kappa for ", column, " across model pairs"),
    x = "Brand Category", y = "Kappa Agreement"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# next steps:
# - compare model-model agreement with human-human agreement
# - find ads where all models give a different label or no model matches human consensus
# - do bootstrapping also by category?





