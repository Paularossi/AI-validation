library(dplyr)
library(irr)
library(ggplot2)
library(readxl)

# ==============================================================================
root_folder <- "C:/Users/P70090005/Documents/AI-validation/gpu outputs/"
responses_human_all <- read_excel(paste(root_folder, "responses_human_final.xlsx", sep=""))
single_choice_vars <- c("new_type_ad", "target_group", "alcohol")
multi_label_vars <- c("who_cat_clean", "prem_offer", "marketing_str")

# check the single choice questions
agreement_single_cols <- function(responses_wide, variable_cols) {
  
  results <- lapply(variable_cols, function(var_name) {
    # auto-select the coder columns for this variable
    coder_cols <- grep(paste0("^", var_name, "_coder[123]$"), names(responses_wide), value = TRUE)
    
    if (length(coder_cols) != 3) {
      warning(paste("Skipping", var_name, ": found", length(coder_cols), "coder columns (expected 3)"))
      return(NULL)
    }
    
    human_matrix <- responses_wide %>%
      select(all_of(coder_cols))
    
    names(human_matrix) <- paste0("coder", 1:3)
    
    # Fleiss' Kappa
    fleiss_result <- kappam.fleiss(human_matrix)
    
    # full agreement prop
    prop_full_agreement <- mean(apply(human_matrix, 1, function(x) length(unique(x)) == 1))
    
    pairwise <- combn(names(human_matrix), 2, simplify = FALSE)
    
    pairwise_df <- do.call(rbind, lapply(pairwise, function(pair) {
      k <- kappa2(human_matrix[, pair])
      data.frame(
        variable = var_name,
        coder_pair = paste(pair, collapse = "_"),
        fleiss_kappa = fleiss_result$value,
        fleiss_p = fleiss_result$p.value,
        prop_full_agreement = prop_full_agreement,
        cohen_kappa = k$value,
        p_value = k$p.value
      )
    }))
  })
  
  bind_rows(results[!sapply(results, is.null)])
}

agreement_summary <- agreement_single_cols(responses_wide = responses_human_all, variable_cols = single_choice_vars)

fleiss_rows <- agreement_summary %>%
  distinct(variable, fleiss_kappa, fleiss_p) %>%
  mutate(coder_pair = "Fleiss", cohen_kappa = fleiss_kappa, p_value = fleiss_p)

plot_df <- bind_rows(
  agreement_summary %>% select(variable, coder_pair, cohen_kappa, p_value),
  fleiss_rows %>% select(variable, coder_pair, cohen_kappa, p_value)
)

ggplot(plot_df, aes(x = variable, y = cohen_kappa, fill = coder_pair)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "Human Agreement per Question",
    x = "Question",
    y = "Kappa Value",
    fill = "Coder Pair"
  ) +
  geom_text(aes(label = round(cohen_kappa, 2)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  ylim(0, 1) +
  theme_minimal()


ggsave("C:/Users/P70090005/Documents/AI-validation/gpu outputs/plots/humans_single_cols.png", width = 9, height = 5)


# now for multi-label columns
library(krippendorffsalpha)

jaccard_similarity <- function(str1, str2) {
  set1 <- trimws(unlist(strsplit(str1, ",")))
  set2 <- trimws(unlist(strsplit(str2, ",")))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  if (union == 0) return(1)  # to handle empty annotations
  return(intersection / union)
}

agreement_multiple_cols <- function(responses_wide_all, variable_list) {
  
  all_summary <- list()
  all_alpha_long <- list()
  
  for (var_name in variable_list) {
    # 1) detect all coder columns for this variable
    coder_cols <- grep(paste0("^", var_name, "_coder"), names(responses_wide_all), value = TRUE)
    
    n_coders <- length(coder_cols)
    if (n_coders < 2) {
      warning(paste("Skipping", var_name, ": found", length(coder_cols), "coder columns (expected > 1)"))
      next
    }
    
    # 2) pull out only those columns + img_id
    df <- responses_wide_all %>%
      select(all_of(coder_cols))
    
    # rename them uniformly to coder1... coderN
    new_names <- paste0("coder", seq_len(n_coders))
    df <- `colnames<-`(df, new_names)
    
    # --- 3) pairwise Jaccard similarity across all coder pairs ---
    model_pairs <- combn(names(df), 2, simplify = FALSE)
    avg_pairwise_jaccard <- sapply(model_pairs, function(pair) {
      sims <- mapply(jaccard_similarity, df[[pair[1]]], df[[pair[2]]])
      mean(sims, na.rm = TRUE)
    })
    names(avg_pairwise_jaccard) <- sapply(model_pairs, function(p) paste(p, collapse = "_"))
    
    # --- 4) build long‐form binary table for per‐label alpha ---
    df_long <- df %>%
      mutate(img_id = row_number()) %>%
      pivot_longer(cols = starts_with("coder"), names_to = "coder", values_to = "labels") %>%
      mutate(labels = gsub("\\s+", "", labels)) %>%
      separate_rows(labels, sep = ",") %>%
      mutate(value = 1, label = paste0("label_", labels)) %>%
      select(img_id, coder, label, value) %>%
      pivot_wider(names_from = label, values_from = value, values_fill = 0) %>%
      arrange(img_id, coder)
    
    label_cols <- setdiff(names(df_long), c("img_id", "coder"))
    
    # --- 5) compute Krippendorff’s alpha per label ---
    alpha_vals <- sapply(label_cols, function(label) {
      mat <- df_long %>%
        select(img_id, coder, !!label) %>%
        pivot_wider(names_from = coder, values_from = !!label) %>%
        select(-img_id) %>%
        as.matrix()
      # transpose: kripp.alpha wants rows=raters, cols=items
      kripp.alpha(t(mat), method = "nominal")$value
    })
    
    # collect summary and label-level alphas
    all_summary[[var_name]] <- data.frame(
      variable = var_name,
      avg_kripp_alpha = mean(alpha_vals, na.rm = TRUE),
      min_kripp_alpha = min(alpha_vals, na.rm = TRUE),
      max_kripp_alpha = max(alpha_vals, na.rm = TRUE),
      sd_kripp_alpha  = sd(alpha_vals, na.rm = TRUE),
      t(avg_pairwise_jaccard),
      row.names = NULL
    )
    
    all_alpha_long[[var_name]] <- data.frame(
      variable = var_name,
      label = gsub("^label_", "", names(alpha_vals)),
      kripp_alpha = alpha_vals,
      row.names = NULL
    )
  }
  
  summary_table <- bind_rows(all_summary)
  alpha_long <- bind_rows(all_alpha_long)
  
  return(list(
    summary_table = summary_table,
    alpha_long = alpha_long
  ))
}

human_multilabel_results <- agreement_multiple_cols(responses_human_all, multi_label_vars)
human_multilabel_results$summary_table   # summary stats per variable
human_multilabel_results$alpha_long 
# !!!! there is still NAs for some reason????!!!

human_multilabel_summary <- human_multilabel_results$summary_table %>%
  mutate(mean_jaccard = rowMeans(select(., starts_with("coder")), na.rm = TRUE))


# pivot coder pairs and mean into long format for plotting
human_multilabel_jaccard_long <- human_multilabel_summary %>%
  pivot_longer(cols = starts_with("coder"), names_to = "pair", values_to = "jaccard") %>%
  bind_rows(
    human_multilabel_summary %>%
      select(variable, mean_jaccard) %>%
      mutate(pair = "Mean") %>%
      rename(jaccard = mean_jaccard)
  )

ggplot(human_multilabel_jaccard_long, aes(x = variable, y = jaccard, fill = pair)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = round(jaccard, 2)),
            position = position_dodge(0.7), 
            vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(
    "coder1_coder2" = "#66c2a5",
    "coder1_coder3" = "#fc8d62",
    "coder2_coder3" = "#8da0cb",
    "Mean" = "#1b7837"     
  )) +
  labs(title = "Human Agreement for Multi-Label Variables",
       x = "Variable",
       y = "Jaccard Similarity",
       fill = "Pair / Mean") +
  ylim(0, 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/P70090005/Documents/AI-validation/gpu outputs/plots/human_mltpl_jaccard_sim.png", width = 9, height = 5)



