
library(readxl)
library(dplyr)
library(irr)
library(tidyr)
library(ggplot2)
library(boot)

root_folder <- "C:/Users/P70090005/Desktop/phd/AI-validation/gpu outputs/"

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))

gemma <- gemma[order(gemma$img_id), ]
gpt <- gpt[order(gpt$img_id), ]
pixtral <- pixtral[order(pixtral$img_id), ]
qwen <- qwen[order(qwen$img_id), ]

models <- list(gemma = gemma, pixtral = pixtral, gpt = gpt, qwen = qwen)

single_choice_vars <- c("new_type_ad", "target_group", "alcohol")
multi_label_vars <- c("new_who_cat", "prem_offer", "marketing_str")

# ==============================================================================
# SINGLE-CHOICE COLUMNS

compare_ai_pairwise_kappa_agreement <- function(models, single_choice_vars) {
  results <- list()
  
  for (colname in single_choice_vars) {
    valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
    
    if (length(valid_models) < 2) next  # need at least 2 models to compare
    
    model_pairs <- combn(names(valid_models), 2, simplify = FALSE)
    
    for (pair in model_pairs) {
      model1 <- pair[1]
      model2 <- pair[2]
      
      df1 <- valid_models[[model1]]
      df2 <- valid_models[[model2]]
      
      if (!(colname %in% names(df1)) || !(colname %in% names(df2))) next
      
      merged <- merge(
        df1[, c("img_id", colname)],
        df2[, c("img_id", colname)],
        by = "img_id"
      )
      
      # remove rows with NAs
      merged <- merged[!is.na(merged[[paste0(colname, ".x")]]) & !is.na(merged[[paste0(colname, ".y")]]), ]
      
      if (nrow(merged) > 0) {
        kappa_val <- tryCatch({
          kappa2(merged[, c(paste0(colname, ".x"), paste0(colname, ".y"))])$value
        }, error = function(e) NA)
        
        agreement_val <- mean(merged[[paste0(colname, ".x")]] == merged[[paste0(colname, ".y")]], na.rm = TRUE)
      } else {
        kappa_val <- NA
        agreement_val <- NA
      }
      
      results[[length(results) + 1]] <- data.frame(
        question = colname,
        model_1 = model1,
        model_2 = model2,
        kappa = kappa_val,
        agreement = agreement_val
      )
    }
  }
  
  return(bind_rows(results))
}

single_choice_agreement <- compare_ai_pairwise_kappa_agreement(models, single_choice_vars)

kappa_matrix <- single_choice_agreement %>%
  select(question, model_1, model_2, kappa) %>%
  mutate(metric = "Cohen's Kappa", value = kappa)

agreement_matrix <- single_choice_agreement %>%
  select(question, model_1, model_2, agreement) %>%
  mutate(metric = "% Agreement", value = agreement)

plot_df <- bind_rows(kappa_matrix, agreement_matrix)

plot_df_symmetric <- plot_df %>%
  bind_rows(
    plot_df %>%
      rename(model_1 = model_2, model_2 = model_1)
  )

ggplot(plot_df_symmetric, aes(x = model_1, y = model_2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  facet_grid(metric ~ question, scales = "free", space = "free") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(0.5, "lines"),
  ) +
  labs(
    title = "Pairwise Agreement Between AI Models",
    x = "Model 1",
    y = "Model 2"
  )

ggsave(paste(root_folder, "plots/ai_ai_single.png", sep=""), width = 9, height = 5)



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
# MULTI-LABEL COLUMNS

compare_multilabel_ai_ai <- function(models, multi_label_vars) {
  out <- list(); idx <- 1
  
  for (question in multi_label_vars) {
    # check which models have this column
    valid_models <- models[sapply(models, function(df) question %in% colnames(df))]
    
    model_pairs <- combn(names(valid_models), 2, simplify = FALSE)
    
    for (pair in model_pairs) {
      m1 <- pair[1]
      m2 <- pair[2]
      
      df1 <- valid_models[[m1]]
      df2 <- valid_models[[m2]]
      
      # Merge by img_id
      merged <- merge(
        df1[, c("img_id", question)],
        df2[, c("img_id", question)],
        by = "img_id",
        suffixes = c("_m1", "_m2")
      )
      
      A <- merged[[paste0(question, "_m1")]]
      B <- merged[[paste0(question, "_m2")]]
      
      keep <- !is.na(A) & !is.na(B)
      
      if (sum(keep) < 2) {
        jacc <- NA_real_; alpha_iou <- NA_real_; alpha_bin <- NA_real_
      } else {
        # 1) Jaccard similarity
        sims <- mapply(jaccard_similarity, A[keep], B[keep])
        jacc <- mean(sims, na.rm = TRUE)
        
        # 2) Krippendorff alpha (IoU distance)
        d <- 1 - sims
        alpha_iou <- kripp_alpha_interval(matrix(d, nrow = 1))
        
        # 3) Binary Krippendorff alpha per label
        all_labels <- unique(unlist(strsplit(
          na.omit(c(A[keep], B[keep])), split = ",\\s*"
        )))
        alpha_bin <- compute_kripp_alpha_binary(A[keep], B[keep], all_labels)
      }
      
      out[[idx]] <- data.frame(
        variable = question,
        model_1 = m1,
        model_2 = m2,
        jaccard = jacc,
        kripp_alpha_iou = alpha_iou,
        kripp_alpha_binary = alpha_bin,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  
  bind_rows(out)
}

multi_choice_agreement <- compare_multilabel_ai_ai(models, multi_label_vars)

# Jaccard
jaccard_matrix <- multi_choice_agreement %>%
  select(variable, model_1, model_2, jaccard) %>%
  mutate(metric = "Jaccard Similarity", value = jaccard)

# Krippendorff's alpha (IoU)
iou_matrix <- multi_choice_agreement %>%
  select(variable, model_1, model_2, kripp_alpha_iou) %>%
  mutate(metric = "Krippendorff’s α (IoU)", value = kripp_alpha_iou)

# Krippendorff's alpha (Binary)
binary_matrix <- multi_choice_agreement %>%
  select(variable, model_1, model_2, kripp_alpha_binary) %>%
  mutate(metric = "Krippendorff’s α (Binary)", value = kripp_alpha_binary)

plot_df <- bind_rows(jaccard_matrix, iou_matrix, binary_matrix)

plot_df_symmetric <- plot_df %>%
  bind_rows(
    plot_df %>%
      rename(model_1 = model_2, model_2 = model_1)
  )

ggplot(plot_df_symmetric, aes(x = model_1, y = model_2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  facet_grid(metric ~ variable, scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0, limits = c(-1, 1), name = "Agreement"
  ) +
  labs(
    title = "AI vs. Human Multi-Label Agreement",
    x = "AI Model", y = "Human Rater"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/ai_ai_multiple.png", sep=""), width = 9, height = 5)



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




