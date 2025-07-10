library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(irr)
library(tidyverse)
library(irrCAC) # to calculate Gwet's AC1/AC2 agreement coefficient
library(psych) # for kappa confidence interval
#library(rel) # for all sorts of agreement metrics - not available for this R version

root_folder <- "C:/Users/P70090005/Desktop/phd/AI-validation/gpu outputs/"
responses_human_all <- read_excel(paste(root_folder, "responses_human_final.xlsx", sep=""))

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

gemma <- gemma %>% filter(img_id %in% responses_human_all$img_id)
gpt <- gpt %>% filter(img_id %in% responses_human_all$img_id)
pixtral <- pixtral %>% filter(img_id %in% responses_human_all$img_id)
qwen <- qwen %>% filter(img_id %in% responses_human_all$img_id)

models <- list(gemma = gemma, pixtral = pixtral, gpt = gpt, qwen = qwen)


single_choice_vars <- c("new_type_ad", "target_group", "alcohol")
multi_label_vars <- c("who_cat_clean", "prem_offer", "marketing_str")

########## COMPARE HUMANS WITH THE AIS
ai_mapping <- list(
  "new_type_ad" = "new_type_ad",  # still compared to same AI col
  "target_group" = "target_group",
  "alcohol" = "is_alcohol",
  "who_cat_clean" = "new_who_cat",
  "prem_offer" = "prem_offer",
  "marketing_str" = "marketing_str"
)

human_mapping <- list(
  "new_type_ad" = "new_type_ad",
  "target_group" = "target_group",
  "alcohol" = "alcohol",
  "who_cat_clean" = "who_cat_clean",
  "prem_offer" = "prem_offer",
  "marketing_str" = "marketing_str"
)


# ====== SINGLE-CHOICE COLUMNS ======

compare_human_ai_kappa <- function(humans, models, ai_mapping, human_mapping) {
  # fix image IDs in human data
  humans$Image_ID <- gsub("\\.png$", "", humans$Image_ID)
  
  results <- list()
  
  for (question in single_choice_vars) {
    ai_column <- ai_mapping[[question]]
    human_base <- human_mapping[[question]] 
    # grab coder1/2/3 + consensus col names 
    human_cols <- grep( 
      paste0("^", human_base, "_(coder[1-3]|cons)$"), 
      names(humans), value = TRUE 
    ) 
    if (length(human_cols) < 1) next 
    
    for (model_name in names(models)) {
      model_df <- models[[model_name]]
    
      for (hc in human_cols) { 
        if (!(ai_column %in% names(model_df)) || !(hc %in% names(humans))) next
        
        merged <- merge(
          humans[, c("Image_ID", hc)],
          model_df[, c("img_id", ai_column)],
          by.x = "Image_ID", by.y = "img_id"
        ) 
        
        merged <- merged[!is.na(merged[[hc]]) & !is.na(merged[[ai_column]]), ]
        
        if (nrow(merged) > 0) {
          kappa_val <- tryCatch({
            kappa2(merged[, c(hc, ai_column)])$value
          }, error = function(e) NA)
          
          agreement_val <- mean(merged[[hc]] == merged[[ai_column]], na.rm = TRUE)
        } else {
          kappa_val <- NA
          agreement_val <- NA
        }
      
        results[[length(results) + 1]] <- data.frame(
          question = question,
          model = model_name,
          coder = hc,
          kappa = kappa_val,
          agreement = agreement_val
        )
      }
    }
  }
  
  return(do.call(rbind, results))
}

results_kappa <- compare_human_ai_kappa(responses_human_all, models, ai_mapping, human_mapping)
print(results_kappa)

plot_df <- results_kappa %>%
  mutate(rater = sub("^.+_(coder[1-3]|cons)$", "\\1", coder),
         rater = recode(rater,
                        coder1 = "Coder 1",
                        coder2 = "Coder 2",
                        coder3 = "Coder 3",
                        cons   = "Consensus"
         )) %>%
  pivot_longer(
    cols = c(kappa, agreement),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = recode(metric,
                         kappa      = "Cohen's Kappa",
                         agreement  = "% Agreement"
  ))



ggplot(plot_df, aes(x = model, y = rater, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  facet_grid(metric ~ question, scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
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

ggsave(paste(root_folder, "plots/human_ai_single.png", sep=""), width = 9, height = 5)


# COMPARE EVERYTHING TOGETHER
compare_all_raters <- function(humans, models, ai_mapping, human_mapping, single_choice_vars) {
  humans$Image_ID <- gsub("\\.png$", "", humans$Image_ID)
  
  results <- list()
  
  for (question in single_choice_vars) {
    ai_column <- ai_mapping[[question]]
    human_base <- human_mapping[[question]]
    
    # get all human raters (e.g., coder1/2/3/cons)
    human_cols <- grep(paste0("^", human_base, "_(coder[1-3]|cons)$"), names(humans), value = TRUE)
    if (length(human_cols) < 1) next
    
    # Build full rater list
    rater_list <- list()
    for (hc in human_cols) {
      df <- humans[, c("Image_ID", hc)]
      names(df) <- c("img_id", "label")
      rater_list[[hc]] <- df
    }
    for (model_name in names(models)) {
      if (!(ai_column %in% names(models[[model_name]]))) next
      df <- models[[model_name]][, c("img_id", ai_column)]
      names(df) <- c("img_id", "label")
      rater_list[[model_name]] <- df
    }
    
    # Compute pairwise metrics
    rater_pairs <- combn(names(rater_list), 2, simplify = FALSE)
    for (pair in rater_pairs) {
      df1 <- rater_list[[pair[1]]]
      df2 <- rater_list[[pair[2]]]
      merged <- merge(df1, df2, by = "img_id")
      merged <- merged[!is.na(merged$label.x) & !is.na(merged$label.y), ]
      
      if (nrow(merged) == 0) next
      
      kappa_val <- tryCatch(kappa2(merged[, c("label.x", "label.y")])$value, error = function(e) NA)
      agreement_val <- mean(merged$label.x == merged$label.y, na.rm = TRUE)
      
      results[[length(results) + 1]] <- data.frame(
        question = question,
        rater1 = pair[1],
        rater2 = pair[2],
        kappa = kappa_val,
        agreement = agreement_val
      )
    }
  }
  
  return(bind_rows(results))
}

comparison_df <- compare_all_raters(responses_human_all, models, ai_mapping, human_mapping, single_choice_vars)

plot_df <- comparison_df %>%
  pivot_longer(cols = c(kappa, agreement), names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(coder[1-3]|cons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|cons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1",
                    coder2 = "Coder 2",
                    coder3 = "Coder 3",
                    cons   = "Consensus",
                    .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1",
                    coder2 = "Coder 2",
                    coder3 = "Coder 3",
                    cons   = "Consensus",
                    .default = rater2),
    metric = recode(metric, kappa = "Cohen's Kappa", agreement = "% Agreement")
  )

plot_df_symmetric <- plot_df %>%
  bind_rows(
    plot_df %>%
      rename(rater1 = rater2, rater2 = rater1)
  ) %>%
  distinct(question, metric, rater1, rater2, .keep_all = TRUE)


ggplot(plot_df_symmetric, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3) +
  facet_grid(metric ~ question, scales = "free", space = "free") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0.5, limits = c(0, 1), name = "Agreement"
  ) +
  labs(
    title = "Pairwise Agreement: Human and AI Raters",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_single.png", sep=""), width = 10, height = 5)



# ====== MULTI-LABEL COLUMNS ======
# Jaccard similarity
jaccard_similarity <- function(str1, str2) {
  set1 <- trimws(unlist(strsplit(str1, ",")))
  set2 <- trimws(unlist(strsplit(str2, ",")))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  if (union == 0) return(1)  # to handle empty annotations
  return(intersection / union)
}


# Measuring Agreement on Set-valued Items (MASI) distance from text string
# retrieved from https://gdmcdonald.github.io/multi-label-inter-rater-agreement/Multi-Label_Agreement.html
masi <- function(x, y, sep = ", ", jaccard_only = F, type = "sim"){
  # define the labels for each rater
  lab_x <- str_split(x, ", ", simplify = F)[[1]]
  lab_y <- str_split(y, ", ", simplify = F)[[1]]
  
  # compute set diff and intersection size
  diff_xy_size <- length(setdiff(lab_x,lab_y)) # number of elements in set x but not in set y
  diff_yx_size <- length(setdiff(lab_y,lab_x)) # number of elements in set y but not in set x
  intersection_size <- length(intersect(lab_x,lab_y)) # number of elements in common between two sets
  
  # monotonicity similarity coefficient, M, see http://www.lrec-conf.org/proceedings/lrec2006/pdf/636_pdf.pdf Rebecca Passonneau. 2006. Measuring Agreement on Set-valued Items (MASI) for Semantic and Pragmatic Annotation. In Proceedings of the Fifth International Conference on Language Resources and Evaluation (LREC’06), Genoa, Italy. European Language Resources Association (ELRA).
  m_sim <- case_when(
    (diff_xy_size == 0) & (diff_yx_size == 0) ~ 1, # the sets are identical, return 1
    (diff_xy_size == 0) | (diff_yx_size == 0) ~ 2/3, # one set is a subset of the other, return 2/3
    (diff_xy_size != 0) & (diff_yx_size != 0) & (intersection_size !=0) ~ 1/3, # some overlap, some non-overlap in each set, return 1/3
    intersection_size ==0 ~ 0 # disjoint sets, return 0
  )
  
  # calculate Jaccard simmilarity; J=1 means same, J=0 means no overlap at all. See https://en.wikipedia.org/wiki/Jaccard_index
  jaccard_sim <- intersection_size/(length(lab_x) + length(lab_y) - intersection_size)
  
  #MASI sim is M*J; MASI dist is 1-M*J
  masi_sim <- if_else(jaccard_only,
                      jaccard_sim,
                      m_sim*jaccard_sim)
  
  return(if_else(type == "sim",
                 masi_sim,
                 1-masi_sim))
}

MASI_simmilarity_matrix <- function(df, sep = ", ") {
  labels_all_combos <- sort(unique(unlist(df))) # alphabetical sorted list of all strings of labels
  
  num_label_combos <- length(labels_all_combos) # number of combinations above
  
  masi_sim_mat <- matrix(nrow = num_label_combos,
                         ncol = num_label_combos,
                         dimnames = list(labels_all_combos,
                                         labels_all_combos))
  
  for(i in 1:num_label_combos){
    for(j in 1:num_label_combos)
    {
      masi_sim_mat[i,j] <- masi(x = labels_all_combos[i],
                                y = labels_all_combos[j],
                                sep = sep)
    }}
  
  return(masi_sim_mat)
}


# wt <- MASI_simmilarity_matrix(data.frame(gpt$prem_offer, qwen$prem_offer), sep = ", ")
# krippen.alpha.raw(ratings=data.frame(gpt$prem_offer, qwen$prem_offer), weights = wt)$est
# krippen.alpha.raw(ratings=data.frame(gpt$prem_offer, qwen$prem_offer))$est


# Krippendorff's alpha calculation (with Jaccard distance function)
kripp_alpha_interval <- function(data_matrix) { 
  data_matrix <- as.matrix(data_matrix) 
  # single‐pair case 
  v <- data_matrix[1, ] 
  v <- v[!is.na(v)] 
  if (length(v) < 2) 
    return(NA_real_) 
  # observed disagreement = mean(v^2) 
  Do <- sum(v^2) / length(v) 
  # expected disagreement = mean squared differences of all pairs 
  combs <- combn(v, 2) 
  De <- sum((combs[1,] - combs[2,])^2) / ncol(combs) 
  1 - Do/De 
} 

# Krippendorff's alpha using binary decomposition
compute_kripp_alpha_binary <- function(human, ai, labels) {
  # build 2 × N matrix: rows = raters, cols = items
  out <- sapply(labels, function(lbl) {
    h_bin <- grepl(paste0("(^|,)", lbl, "(,|$)"), human)
    a_bin <- grepl(paste0("(^|,)", lbl, "(,|$)"), ai)
    
    mat <- rbind(h_bin, a_bin)
    # remove items with all NAs or constant values
    if (length(unique(mat[1, ])) < 2 && length(unique(mat[2, ])) < 2) {
      return(NA_real_)
    }
    kripp.alpha(mat, method = "nominal")$value
  })
  
  mean(out, na.rm = TRUE)
}

# EVERYTHING TOGETHER
# if consesnsus = TRUE, compute agreement ONLY with the human consensus column
compare_multilabel_human_ai <- function(humans, models, multi_label_vars, consensus = TRUE) { 
  out <- list(); idx <- 1 
  # normalize image ID
  humans <- humans %>% mutate(Image_ID = gsub("\\.png$", "", Image_ID))
  
  for (question in multi_label_vars) { 
    ai_col <- ai_mapping[[question]] 
    human_base <- human_mapping[[question]] 
    
    if (consensus) {
      human_cols <- paste0(human_base, "_cons")
    } else {
      # grab coder1/2/3 + consensus col names 
      human_cols <- grep( 
        paste0("^", human_base, "_(coder[1-3]|cons)$"), 
        names(humans), value = TRUE 
      ) 
    }
    
    if (length(human_cols) < 1) next 
    
    # build full rater list
    rater_list <- list()
    for (hc in human_cols) {
      df <- humans[, c("Image_ID", hc)]
      names(df) <- c("img_id", "label")
      rater_list[[hc]] <- df
    }
    for (model_name in names(models)) {
      if (!(ai_col %in% names(models[[model_name]]))) next
      df <- models[[model_name]][, c("img_id", ai_col)]
      names(df) <- c("img_id", "label")
      rater_list[[model_name]] <- df
    }
    
    # compute pairwise metrics of everything
    rater_pairs <- combn(names(rater_list), 2, simplify = FALSE)
    
    for (pair in rater_pairs) {
      print(paste0("calculating agreement for ", pair[[1]], " and ", pair[[2]]))
      df1 <- rater_list[[pair[1]]]
      df2 <- rater_list[[pair[2]]]
      merged <- merge(df1, df2, by = "img_id")
      merged <- merged[!is.na(merged$label.x) & !is.na(merged$label.y), ]
      
      if (nrow(merged) == 0) next
      
      # 1) avg Jaccard 
      sims <- mapply(jaccard_similarity, merged$label.x, merged$label.y) 
      jacc <- mean(sims, na.rm = TRUE) 
      # 2) IoU‐alpha: build 1×N distance vector 
      d <- 1 - sims 
      alpha <- kripp_alpha_interval(matrix(d, nrow = 1)) 
      
      # 3) binary Krippendorff's alpha
      # get all possible labels across human & ai strings
      all_labels <- unique(unlist(strsplit(
        na.omit(c(merged$label.x, merged$label.y)), split = ",\\s*"
      )))
      
      alpha_bin <- compute_kripp_alpha_binary(merged$label.x, merged$label.y, all_labels)
      
      # 4) Krippendorff's kappa using irrCOC
      alpha_unweighted <- krippen.alpha.raw(ratings=data.frame(merged$label.x, merged$label.y))$est
      
      # 5) Krippendorff's Alpha using MASI weights
      wt <- MASI_simmilarity_matrix(data.frame(merged$label.x, merged$label.y), sep = ", ")
      alpha_masi <- krippen.alpha.raw(ratings=data.frame(merged$label.x, merged$label.y), weights = wt)$est
      
      out[[idx]] <- data.frame( 
        question = question, 
        rater1 = pair[1],
        rater2 = pair[2],
        jaccard = jacc, 
        kripp_alpha_iou = alpha,
        kripp_alpha_binary = alpha_bin,
        kripp_alpha_unweighted = alpha_unweighted$coeff.val,
        kripp_alpha_masi = alpha_masi$coeff.val,
        stringsAsFactors = FALSE 
      ) 
      idx <- idx + 1 
    }
  } 
  
  bind_rows(out) 
} 

res_ml <- compare_multilabel_human_ai(humans=responses_human_all, models = list(gpt=gpt, qwen=qwen), 
                                      multi_label_vars)

plot_df_ml <- res_ml %>%
  pivot_longer(cols = c(jaccard, kripp_alpha_iou, kripp_alpha_binary, kripp_alpha_unweighted), 
               names_to = "metric", values_to = "value") %>%
  mutate(
    rater1 = sub(".*_(coder[1-3]|cons)$", "\\1", rater1),
    rater2 = sub(".*_(coder[1-3]|cons)$", "\\1", rater2),
    rater1 = recode(rater1,
                    coder1 = "Coder 1",
                    coder2 = "Coder 2",
                    coder3 = "Coder 3",
                    cons   = "Consensus",
                    .default = rater1),
    rater2 = recode(rater2,
                    coder1 = "Coder 1",
                    coder2 = "Coder 2",
                    coder3 = "Coder 3",
                    cons   = "Consensus",
                    .default = rater2),
    metric = recode(metric,
                                    jaccard             = "Jaccard Similarity",
                                    kripp_alpha_binary  = "Krippendorff’s α (Binary)",
                                    kripp_alpha_iou     = "Krippendorff’s α (IoU)")
  )


plot_df_sym <- plot_df_ml %>%
  bind_rows(
    plot_df_ml %>%
      rename(rater1 = rater2, rater2 = rater1)
  )

ggplot(plot_df_sym, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.75) +
  facet_grid(metric ~ question, scales = "free", space = "free_x") +
  scale_fill_gradient2(
    low = "red", mid = "yellow", high = "darkgreen",
    midpoint = 0, limits = c(-1, 1), name = "Agreement"
  ) +
  labs(
    title = "Agreement Across All Raters (Humans + AI Models, Multi-Label)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )


ggsave(paste(root_folder, "plots/everything_multiple.png", sep=""), width = 10, height = 5)





# ==============================================================================
# MORE ANALYSIS - by category of the brand
table(responses_human_all$category)

# how consistent are AI models in different product categories?
agreement_by_category_single <- function(models, column, humans = NULL) {
  raters <- list()
  
  # add all AI models
  for (model_name in names(models)) {
    df <- models[[model_name]][, c("img_id", "category", ai_mapping[[column]])]
    colnames(df)[3] <- "label"
    df$model <- model_name
    raters[[model_name]] <- df
  }
  
  # optionally add human consensus
  if (!is.null(humans)) {
    human_col <- human_mapping[[column]]
    consensus_col <- paste0(human_col, "_cons")
    
    if (consensus_col %in% names(humans)) {
      df <- humans[, c("Image_ID", "category", consensus_col)]
      colnames(df) <- c("img_id", "category", "label")
      df$model <- "consensus"
      raters[["consensus"]] <- df
    }
  }

  # create all pairwise combinations
  rater_names <- names(raters)
  pairs <- combn(rater_names, 2, simplify = FALSE)
  
  results <- list()
  
  for (pair in pairs) {
    r1 <- raters[[pair[1]]]
    r2 <- raters[[pair[2]]]
    
    merged <- inner_join(r1, r2, by = c("img_id", "category"), suffix = c("_1", "_2")) %>%
      filter(!is.na(label_1) & !is.na(label_2)) %>%
      mutate(label_1 = as.character(label_1),
             label_2 = as.character(label_2))
    
    if (nrow(merged) == 0) next
    
    kappa_df <- merged %>%
      group_by(category) %>%
      summarise(
        kappa = kappa2(data.frame(label_1, label_2))$value,
        kappa2 = cohen.kappa(data.frame(label_1, label_2))$kappa,
        weight_kappa2 = cohen.kappa(data.frame(label_1, label_2))$weighted.kappa,
        #prop_agreement = mean(label_1 == label_2, na.rm = TRUE),
        gwet_coeff = gwet.ac1.raw(data.frame(label_1, label_2))$est$coeff.val,
        prop_agreement = gwet.ac1.raw(data.frame(label_1, label_2))$est$pa,
        gwet_ci = gwet.ac1.raw(data.frame(label_1, label_2))$est$conf.int,
        n = n(),
        .groups = "drop"
      ) %>%
      mutate(model_pair = paste(pair, collapse = "_"))
    
    results[[paste(pair, collapse = "_")]] <- kappa_df
  }
  
  bind_rows(results)
}




category_agreement_all <- lapply(single_choice_vars, function(col) {
  df <- agreement_by_category_single(models = list(gpt = gpt, qwen = qwen), column = col, humans = humans)
  df$question <- col
  df
}) %>% bind_rows()

# just for one question
category_agreement_single <- agreement_by_category_single(models = list(gpt = gpt, qwen = qwen), 
                                                   column = single_choice_vars[3])


ggplot(category_agreement_all, aes(x = category, y = kappa, fill = model_pair)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(kappa, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  #facet_wrap(~ question, scales = "free_x") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Model Agreement by Brand Category",
    subtitle = "Cohen's Kappa per Question and Model Pair",
    y = "Kappa Agreement",
    x = "Brand Category"
  )


ggplot(category_kappas, aes(x = model_pair, y = category, fill = kappa)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(kappa, 2)), size = 3) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "darkgreen",
                       midpoint = 0.5, limits = c(0, 1), name = "Kappa") +
  theme_minimal() +
  labs(title = "Model Agreement (Kappa) by Brand Category",
       x = "Model Pair", y = "Category")





top_bottom <- category_agreement_all %>%
  group_by(category, question) %>%
  summarise(avg_kappa = mean(kappa, na.rm = TRUE)) %>%
  arrange(avg_kappa)

head(top_bottom, 5)    # worst categories
tail(top_bottom, 5) 


# next steps:
# - compare model-model agreement with human-human agreement
# - find ads where all models give a different label or no model matches human consensus


table(gpt$new_type_ad, responses_human_all$new_type_ad_cons)








#=========== COMPARE DISAGREEMENTS
# extract_single_choice_disagreements <- function(humans, models) {
#   disagreements <- list()
#   
#   for (question in single_choice_vars) {
#     ai_col <- ai_mapping[[question]]
#     human_base <- human_mapping[[question]]
#     
#     human_cols <- grep(paste0("^", human_base, "_(coder[1-3]|cons)$"), names(humans), value = TRUE)
#     
#     for (hc in human_cols) {
#       for (model_name in names(models)) {
#         model_df <- models[[model_name]]
#         if (!(ai_col %in% names(model_df))) next
#         
#         merged <- merge(
#           humans[, c("img_id", hc)],
#           model_df[, c("img_id", ai_col)],
#           by = "img_id"
#         ) %>%
#           rename(human = !!hc, ai = !!ai_col) %>%
#           filter(!is.na(human), !is.na(ai), human != ai) %>%
#           mutate(
#             question = question,
#             model = model_name,
#             coder = hc
#           )
#         
#         disagreements[[length(disagreements) + 1]] <- merged[, c("img_id", "question", "coder", "model", "human", "ai")]
#       }
#     }
#   }
#   bind_rows(disagreements)
# }
# 
# temp <- extract_single_choice_disagreements(responses_human_all, models)







