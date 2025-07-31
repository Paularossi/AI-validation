library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(irr)
library(tidyverse)
library(irrCAC) # to calculate Gwet's AC1/AC2 agreement coefficient
library(psych) # for kappa confidence interval

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

# labels for all questions
label_df <- read.csv("C:/Users/P70090005/Desktop/phd/AI-validation/data/label_mappings.csv", 
                     stringsAsFactors = FALSE, na = c(""))

# function to merge data for a given column of ai models and human consensus
merge_ai_human_column <- function(models, humans, column) {
  model_names <- names(models)
  
  # extract relevant model columns
  model_preds <- lapply(model_names, function(model) {
    df <- models[[model]]
    col <- ai_mapping[[column]]
    df %>% select(img_id, !!sym(col)) %>% rename(!!model := !!sym(col))
  }) %>% reduce(full_join, by = "img_id")
  
  # add human consensus
  consensus_col <- paste0(human_mapping[[column]], "_cons")
  human_cons <- humans %>%
    select(img_id, consensus = !!sym(consensus_col), category)
  
  merged <- full_join(model_preds, human_cons, by = "img_id")
  
  return(merged)
}


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
          kappa2 <- cohen.kappa(merged[, c(hc, ai_column)]) # using psych package
          gwet_coeff <- gwet.ac1.raw(merged[, c(hc, ai_column)])$est$coeff.val
          prop_agreement <- gwet.ac1.raw(merged[, c(hc, ai_column)])$est$pa
          gwet_ci <- gwet.ac1.raw(merged[, c(hc, ai_column)])$est$conf.int
          
        } else {
          kappa_val <- NA
          agreement_val <- NA
        }
      
        results[[length(results) + 1]] <- data.frame(
          question = question,
          model = model_name,
          coder = hc,
          prop_agreement = prop_agreement,
          kappa = kappa2$kappa,
          kappa_conf_low = kappa2$confid[1, "lower"],
          kappa_conf_upp = kappa2$confid[1, "upper"],
          gwet_coeff = gwet_coeff,
          gwet_ci = gwet_ci
        )
      }
    }
  }
  
  return(do.call(rbind, results))
}

results_kappa <- compare_human_ai_kappa(responses_human_all, models, ai_mapping, human_mapping)
head(results_kappa)

# unnest the conf int for gwet
results_kappa <- results_kappa %>%
  mutate(gwet_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", gwet_ci)),
         gwet_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", gwet_ci))) 

# pivot to long format for plotting
plot_df <- results_kappa %>%
  mutate(rater = sub("^.+_(coder[1-3]|cons)$", "\\1", coder),
         rater = recode(rater,
                        coder1 = "Coder 1",
                        coder2 = "Coder 2",
                        coder3 = "Coder 3",
                        cons   = "Consensus"
         )) %>%
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

ggsave(paste(root_folder, "plots/human_ai_single.png", sep=""), width = 9, height = 5)

# plot with the confidence intervals
plot_df_confint <- results_kappa %>%
  pivot_longer(cols = c(kappa, gwet_coeff),
               names_to = "metric", values_to = "value") %>%
  mutate(
    ci_low = ifelse(metric == "kappa", kappa_conf_low, gwet_ci_low),
    ci_upp = ifelse(metric == "kappa", kappa_conf_upp, gwet_ci_upp),
    metric = recode(metric, kappa = "Cohen's Kappa", gwet_coeff = "Gwet’s AC1"),
    coder = sub("^.+_(coder[1-3]|cons)$", "\\1", coder)
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

ggsave(paste(root_folder, "plots/human_ai_single_confint.png", sep=""), width = 9, height = 5)

# COMPARE EVERYTHING TOGETHER
compare_all_raters <- function(humans, models, single_choice_vars) {
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
      
      kappa2 <- cohen.kappa(merged[, c("label.x", "label.y")]) # using psych package
      gwet_coeff <- gwet.ac1.raw(merged[, c("label.x", "label.y")])$est$coeff.val
      prop_agreement <- gwet.ac1.raw(merged[, c("label.x", "label.y")])$est$pa
      gwet_ci <- gwet.ac1.raw(merged[, c("label.x", "label.y")])$est$conf.int
      
      results[[length(results) + 1]] <- data.frame(
        question = question,
        rater1 = pair[1],
        rater2 = pair[2],
        prop_agreement = prop_agreement,
        kappa = kappa2$kappa,
        kappa_conf_low = kappa2$confid[1, "lower"],
        kappa_conf_upp = kappa2$confid[1, "upper"],
        gwet_coeff = gwet_coeff,
        gwet_ci = gwet_ci
      )
    }
  }
  
  return(bind_rows(results))
}

comparison_df <- compare_all_raters(responses_human_all, models, single_choice_vars)

plot_df <- comparison_df %>%
  pivot_longer(cols = c(kappa, prop_agreement, gwet_coeff), names_to = "metric", values_to = "value") %>%
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
    metric = recode(metric, kappa = "Cohen's Kappa", prop_agreement = "% Agreement",
                    gwet_coeff = "Gwet's AC1")
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
    title = "Agreement Across All Raters (Single-Choice)",
    x = "Rater 1", y = "Rater 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y = element_text(angle = -90),
    panel.spacing = unit(0.5, "lines")
  )

ggsave(paste(root_folder, "plots/everything_single.png", sep=""), width = 10, height = 5)

comparison_df %>%
  mutate(
    across(where(is.numeric), ~round(.x, 2)),
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
    kappa_conf = paste0("(", kappa_conf_low, ",", kappa_conf_upp, ")", sep="")) %>%
  select(-c("kappa_conf_low", "kappa_conf_upp")) %>%
  # save to excel
  #write_xlsx(paste(root_folder, "plots/agreement_single_all.xlsx", sep=""))
  DT::datatable(options = list(scrollX = TRUE))


multi_confint <- read_excel(paste(root_folder, "plots/agreement_multiple_all.xlsx", sep=""))

# plot with the confidence intervals
plot_df_confint <- multi_confint %>%
  filter(!(rater1 %in% c("gpt", "qwen", "gemma", "pixtral")),
         rater2 %in% c("gpt", "qwen", "gemma", "pixtral")) %>%
  mutate(kripp_masi_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", kripp_alpha_masi_ci)),
         kripp_masi_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", kripp_alpha_masi_ci))) %>%
  pivot_longer(cols = c(jaccard, kripp_alpha_masi),
               names_to = "metric", values_to = "value") %>%
  mutate(
    ci_low = ifelse(metric == "jaccard", value, kripp_masi_ci_low),
    ci_upp = ifelse(metric == "jaccard", value, kripp_masi_ci_upp),
    metric = recode(metric, jaccard = "Jaccard Similarity", kripp_alpha_masi = "Krippendorff's Alpha MASI")
  )

ggplot(plot_df_confint, aes(x = rater1, y = value, color = rater2)) +
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

ggsave(paste(root_folder, "plots/human_ai_multiple_confint.png", sep=""), width = 9, height = 5)






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
      # d <- 1 - sims 
      # alpha <- kripp_alpha_interval(matrix(d, nrow = 1)) 
      
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
        #kripp_alpha_iou = alpha,
        kripp_alpha_binary = alpha_bin,
        kripp_alpha_unweighted = alpha_unweighted$coeff.val,
        kripp_alpha_unweighted_ci = alpha_unweighted$conf.int,
        kripp_alpha_masi = alpha_masi$coeff.val,
        kripp_alpha_masi_ci = alpha_masi$conf.int,
        stringsAsFactors = FALSE 
      ) 
      idx <- idx + 1 
    }
  }
  
  bind_rows(out) 
}

res_ml <- compare_multilabel_human_ai(humans=responses_human_all, models, 
                                      multi_label_vars, consensus = FALSE)

plot_df_ml <- res_ml %>%
  select(-kripp_alpha_unweighted) %>%
  pivot_longer(cols = c(jaccard, kripp_alpha_binary, kripp_alpha_masi), 
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
    metric = recode(metric, jaccard = "Jaccard Similarity",
                    kripp_alpha_binary = "Krippendorff’s α (Binary)",
                    kripp_alpha_masi = "Krippendorff’s α (MASI)")
  )


plot_df_sym <- plot_df_ml %>%
  bind_rows(
    plot_df_ml %>%
      rename(rater1 = rater2, rater2 = rater1)
  )

ggplot(plot_df_sym, aes(x = rater1, y = rater2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.9) +
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


ggsave(paste(root_folder, "plots/everything_multiple.png", sep=""), width = 10, height = 6)

res_ml %>%
  mutate(
    across(where(is.numeric), ~round(.x, 2)),
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
                    .default = rater2)) %>%
  # save to excel
  #write_xlsx(paste(root_folder, "plots/agreement_multiple_all.xlsx", sep=""))
  DT::datatable(options = list(scrollX = TRUE))
  
  # to save the table as an image:
  # gt() %>%
  # fmt_number(
  #   columns = where(is.numeric),
  #   decimals = 2
  # ) %>%
  # data_color(
  #   columns = where(is.numeric),
  #   colors = scales::col_numeric(
  #     palette = c("red", "yellow", "green"),
  #     domain = c(0, 1)
  #   )
  # ) %>%
  # gtsave(paste(root_folder, "plots/res_ml_table.png", sep=""))



# ==============================================================================
# MORE ANALYSIS - by category of the brand
table(responses_human_all$category)

# how consistent are AI models in different product categories?
agreement_by_category_all <- function(models, column, humans = NULL, multiple = FALSE) {
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
      df <- humans[, c("img_id", "category", consensus_col)]
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
    
    if (!multiple) { # single-choice columns
      kappa_df <- merged %>%
        group_by(category) %>%
        summarise(
          kappa2 = cohen.kappa(data.frame(label_1, label_2))$kappa,
          kappa_conf_low = cohen.kappa(data.frame(label_1, label_2))$confid[1, "lower"],
          kappa_conf_upp = cohen.kappa(data.frame(label_1, label_2))$confid[1, "upper"],
          gwet_coeff = gwet.ac1.raw(data.frame(label_1, label_2))$est$coeff.val,
          prop_agreement = gwet.ac1.raw(data.frame(label_1, label_2))$est$pa,
          gwet_ci = gwet.ac1.raw(data.frame(label_1, label_2))$est$conf.int,
          n = n(),
          .groups = "drop"
        ) %>%
        mutate(gwet_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", gwet_ci)),
               gwet_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", gwet_ci)),
               model_pair = paste(pair, collapse = "_")) 
      
      results[[paste(pair, collapse = "_")]] <- kappa_df
    } else { # multi-label columns
      all_labels <- unique(unlist(strsplit(
        na.omit(c(merged$label_1, merged$label_2)), split = ",\\s*"
      )))
      
      multi_agreement <- merged %>%
        group_by(category) %>%
        summarise(
          jacc = mean(jaccard_similarity(label_1, label_2), na.rm = TRUE),
          alpha_bin = compute_kripp_alpha_binary(label_1, label_2, all_labels),
          alpha_unweighted = krippen.alpha.raw(ratings=data.frame(label_1, label_2))$est$coeff.val,
          alpha_masi = krippen.alpha.raw(ratings=data.frame(label_1, label_2), 
                                         weights = MASI_simmilarity_matrix(data.frame(label_1, label_2), sep = ", "))$est$coeff.val,
          alpha_masi_ci = krippen.alpha.raw(ratings=data.frame(label_1, label_2), 
                                         weights = MASI_simmilarity_matrix(data.frame(label_1, label_2), sep = ", "))$est$conf.int,
          n = n(),
          .groups = "drop"
        ) %>%
        mutate(alpha_ci_low = as.numeric(sub("\\(([^,]+),.*", "\\1", alpha_masi_ci)),
               alpha_ci_upp = as.numeric(sub(".*,(.+)\\)", "\\1", alpha_masi_ci)),
               model_pair = paste(pair, collapse = "_")) 
      
      results[[paste(pair, collapse = "_")]] <- multi_agreement
    }
  }
  
  bind_rows(results)
}


category_agreement_single <- lapply(single_choice_vars, function(col) {
  df <- agreement_by_category_all(models = list(gpt = gpt, qwen = qwen), 
                                  column = col, humans = responses_human_all)
  df$question <- col
  df
}) %>% bind_rows()

category_agreement_multiple <- lapply(multi_label_vars, function(col) {
  df <- agreement_by_category_all(models = list(gpt = gpt, qwen = qwen), column = col, 
                                  humans = responses_human_all, multiple = TRUE)
  df$question <- col
  df
}) %>% bind_rows()

custom_colors <- c(
  "gpt_consensus" = "#00BFFF",
  "qwen_consensus" = "#1C86EE",
  "gpt_qwen" = "#EEC900"
)

# single choice
category_agreement_single %>%
  mutate(model_pair = factor(model_pair, levels = c("gpt_consensus", "qwen_consensus", "gpt_qwen")),
         category_n = paste0(category, " (", n, ")")) %>%
  #filter(model_pair != "gpt_qwen") %>%
  filter(question == "new_type_ad") %>%
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
overall_alphas <- multi_confint %>%
  filter(question == "marketing_str", rater1 == "Consensus") %>%
  select(rater2, kripp_alpha_masi)
alpha_gpt <- overall_alphas %>% filter(rater2 == "gpt") %>% pull(kripp_alpha_masi)
alpha_qwen <- overall_alphas %>% filter(rater2 == "qwen") %>% pull(kripp_alpha_masi)


category_agreement_multiple %>%
  mutate(model_pair = factor(model_pair, levels = c("gpt_consensus", "qwen_consensus", "gpt_qwen")),
         category_n = paste0(category, " (", n, ")")) %>%
  #filter(model_pair != "gpt_qwen") %>%
  filter(question == "marketing_str") %>%
  ggplot(aes(x = category_n, y = alpha_masi, fill = model_pair)) +
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
find_disagreeing_ads <- function(models, column, humans) {
  
  merged <- merge_ai_human_column(models, humans, column)
  
  # keep rows where no model matches consensus
  disagreed <- merged %>%
    rowwise() %>%
    mutate(
      n_match = sum(c_across(all_of(names(models))) == consensus, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(consensus), n_match == 0)
  
  return(disagreed)
}


disagreed_ads_single <- lapply(single_choice_vars, function(col) {
  df <- find_disagreeing_ads(models = list(gpt = gpt, qwen = qwen), 
                             column = col, humans = responses_human_all)
  df$question <- col
  df
}) %>% bind_rows()

#disagreed_ads_adtype %>% group_by(category) %>% count() %>% arrange(desc(n))

# !!! doesn't really make sense for multiple columns
disagreed_ads_multiple <- lapply(multi_label_vars, function(col) {
  df <- find_disagreeing_ads(models = list(gpt = gpt, qwen = qwen), 
                             column = col, humans = responses_human_all)
  df$question <- col
  df
}) %>% bind_rows()



# by label for single columns 
single_disagreement_by_label <- function(models, humans, column) {
  
  merged <- merge_ai_human_column(models, humans, column) %>%
    filter(!is.na(consensus))  # remove rows with no consensus
  
  merged %>% filter(gemma == "8")
  
  # count for each consensus label
  results <- merged %>%
    rowwise() %>%
    mutate(
      n_match = sum(c_across(all_of(names(models))) == consensus, na.rm = TRUE),
      all_failed = n_match == 0,
      across(all_of(names(models)), ~ .x != consensus, .names = "failed_{.col}")
    ) %>%
    ungroup() %>%
    group_by(consensus) %>%
    summarise(
      total = n(),
      n_all_failed = sum(all_failed),
      pct_all_failed = mean(all_failed),
      across(starts_with("failed_"), list(
        n = ~sum(.x, na.rm = TRUE),
        pct = ~mean(.x, na.rm = TRUE)
      )),
      .groups = "drop"
    ) %>%
    arrange(desc(pct_all_failed))
  
  return(results)
}


disagreed_by_label_single <- lapply(single_choice_vars, function(col) {
  df <- single_disagreement_by_label(models = models, 
                             humans = responses_human_all, column = col)
  df$question <- col
  df
}) %>% bind_rows()

# 1. which labels are most missed overall? (pct_all_failed) - ambiguity in visual/textual cues.
# 2. which model misses more?

plot_df <- disagreed_by_label_single %>%
  left_join(label_df, by = join_by(consensus == code, question == category)) %>%
  filter(question == "target_group") %>%
  select(consensus, failed_gpt_pct, failed_qwen_pct, failed_pixtral_pct, failed_gemma_pct, text_label, total) %>%
  pivot_longer(-c("consensus", "text_label", "total"), names_to = "model", values_to = "pct_failed") %>%
  mutate(model = recode(model,
                        failed_gpt_pct = "GPT",
                        failed_qwen_pct = "Qwen",
                        failed_pixtral_pct = "Pixtral",
                        failed_gemma_pct = "Gemma"))

ggplot(plot_df, aes(x = model, y = reorder(text_label, -pct_failed), fill = pct_failed)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(round(pct_failed, 2), " (", total, ")", sep="")), size = 4) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
  labs(title = "Missed Consensus Labels by Model and Label (Target Group)",
       x = "Model", y = "Label Code", fill = "% Missed") +
  theme_minimal()

ggsave(paste(root_folder, "plots/ai_missed_target_group.png", sep=""), width = 10, height = 6)




# MULTI-LABEL
multi_disagreement_by_label <- function(models, humans, column) {
  
  merged <- merge_ai_human_column(models, humans, column) %>%
    filter(!is.na(consensus))  # filter empty rows
  
  # get unique labels from consensus
  all_labels <- unique(unlist(strsplit(merged$consensus, ",\\s*")))
  all_labels <- all_labels[all_labels != ""]
  
  results <- list()
  
  for (label in all_labels) {
    rows_with_label <- merged %>%
      filter(grepl(paste0("(^|,\\s*)", label, "(\\s*,|$)"), consensus))
    
    n_total <- nrow(rows_with_label)
    
    if (n_total == 0) next
    
    # check if label is present in any model output
    n_failed_all <- rows_with_label %>%
      rowwise() %>%
      mutate(
        n_match = sum(sapply(names(models), function(m) {
          grepl(paste0("(^|,\\s*)", label, "(\\s*,|$)"), get(m))
        })),
        all_failed = n_match == 0,
        across(all_of(names(models)), ~ !grepl(paste0("(^|,\\s*)", label, "(\\s*,|$)"), .x), .names = "failed_{.col}")
      ) %>%
      ungroup()
    
    summary_counts <- n_failed_all %>%
      summarise(
        n_all_failed = sum(all_failed),
        across(starts_with("failed_"), sum)
      )
    
    results[[label]] <- data.frame(
      label = label,
      n = n_total,
      n_all_failed = summary_counts$n_all_failed,
      pct_all_failed = summary_counts$n_all_failed / n_total,
      # add per-model missed counts and percentages
      do.call(cbind, lapply(names(models), function(m) {
        n_failed <- summary_counts[[paste0("failed_", m)]]
        setNames(
          data.frame(n_failed, n_failed / n_total),
          c(paste0("n_", m, "_failed"), paste0("pct_", m, "_failed"))
        )
      }))
    )
  }
  
  bind_rows(results) %>% arrange(desc(pct_all_failed))
}

disagreed_by_label_multiple <- lapply(multi_label_vars, function(col) {
  df <- multi_disagreement_by_label(models = models, 
                                     humans = responses_human_all, column = col)
  df$question <- col
  df
}) %>% bind_rows()


plot_df <- disagreed_by_label_multiple %>%
  left_join(label_df, by = join_by(label == code, question == category)) %>%
  filter(question == "marketing_str") %>%
  select(label, pct_gpt_failed, pct_qwen_failed, pct_pixtral_failed, pct_gemma_failed, text_label, n) %>%
  pivot_longer(-c("label", "text_label", "n"), names_to = "model", values_to = "pct_failed") %>%
  mutate(model = recode(model,
                        pct_gpt_failed = "GPT",
                        pct_qwen_failed = "Qwen",
                        pct_pixtral_failed = "Pixtral",
                        pct_gemma_failed = "Gemma"))

ggplot(plot_df, aes(x = model, y = reorder(text_label, -pct_failed), fill = pct_failed)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(round(pct_failed, 2), " (", n, ")", sep="")), size = 4) +
  scale_fill_gradient2(
    low = "darkgreen", mid = "yellow", high = "red", midpoint = 0.5, limits = c(0, 1))+
  labs(title = "Missed Consensus Labels by Model and Label (Marketing Strategies)",
       x = "Model", y = "Label Code", fill = "% Missed") +
  theme_minimal()

ggsave(paste(root_folder, "plots/ai_missed_marketing_str.png", sep=""), width = 10, height = 6)






