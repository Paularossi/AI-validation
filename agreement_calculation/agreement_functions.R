library(dplyr)
library(irr)
library(tidyverse)
library(irrCAC) # to calculate Gwet's AC1/AC2 agreement coefficient
library(psych) # for kappa confidence interval

single_choice_vars <- c("new_type_ad", "target_group", "alcohol")
multi_label_vars <- c("who_cat_clean", "prem_offer", "marketing_str")

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


# function to merge data for a given column of ai models and human consensus
merge_ai_human_column <- function(models, humans = NULL, column, dieticians = NULL) {
  model_names <- names(models)
  
  # extract relevant model columns
  model_preds <- lapply(model_names, function(model) {
    df <- models[[model]]
    col <- ai_mapping[[column]]
    df %>% select(img_id, !!sym(col)) %>% rename(!!model := !!sym(col))
  }) %>% reduce(full_join, by = "img_id")
  
  if (!is.null(humans)) {
    # add human consensus
    consensus_col <- paste0(human_mapping[[column]], "_cons")
    human_cons <- humans %>%
      select(img_id, consensus = !!sym(consensus_col), category)
  }
  
  if (!is.null(dieticians)) {
    diet_cols <- grep( 
      paste0("^", human_mapping[[column]], "_(diet[1-3]|dietcons)$"), 
      names(dieticians), value = TRUE 
    ) 
    diet_preds <- dieticians %>%
      select(c(img_id, diet_cols)) %>%
      rename(diet1 = paste(human_mapping[[column]], "_diet1", sep=""),
             diet2 = paste(human_mapping[[column]], "_diet2", sep=""),
             diet3 = paste(human_mapping[[column]], "_diet3", sep=""),
             dietcons = paste(human_mapping[[column]], "_dietcons", sep=""))
  }
  
  if (!is.null(humans) & !is.null(dieticians))
    merged <- ull_join(full_join(model_preds, human_cons, by = "img_id"), diet_preds, by="img_id")
  else if (is.null(humans))
    merged <- full_join(model_preds, diet_preds, by="img_id")
  else if (is.null(dieticians))
    merged <- full_join(model_preds, human_cons, by = "img_id")
  else {
    print("Warning: both humans and dieticians missing.")
    merged <- model_preds
  }
  return(merged)
}


# ====== SINGLE-CHOICE COLUMNS ======
compare_human_ai_kappa <- function(humans, models, dieticians = NULL) {
  
  results <- list()
  
  if (!is.null(dieticians)) {
    # merge with the rest of the human classifications
    humans <- inner_join(humans, dieticians, by = "img_id")
  }
  
  for (question in single_choice_vars) {
    ai_column <- ai_mapping[[question]]
    human_base <- human_mapping[[question]] 
    # grab coder1/2/3 + consensus col names 
    human_cols <- grep( 
      paste0("^", human_base, "_(coder[1-3]|diet[1-3]|cons|dietcons)$"), 
      names(humans), value = TRUE 
    ) 
    if (length(human_cols) < 1) next 
    
    for (model_name in names(models)) {
      model_df <- models[[model_name]]
      
      for (hc in human_cols) { 
        if (!(ai_column %in% names(model_df)) || !(hc %in% names(humans))) next
        
        merged <- merge(
          humans[, c("img_id", hc)],
          model_df[, c("img_id", ai_column)],
          by = "img_id"
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

# COMPARE EVERYTHING TOGETHER
compare_all_raters <- function(humans, models, dieticians = NULL) {
  
  results <- list()
  
  if (!is.null(dieticians)) {
    # merge with the rest of the human classifications
    humans <- inner_join(humans, dieticians, by = "img_id")
  } 
  
  for (question in single_choice_vars) {
    ai_column <- ai_mapping[[question]]
    human_base <- human_mapping[[question]]
    
    # get all human raters (e.g., coder123/cons/diet123)
    human_cols <- grep(paste0("^", human_base, "_(coder[1-3]|diet[1-3]|cons|dietcons)$"), names(humans), value = TRUE)
    if (length(human_cols) < 1) next
    
    # build full rater list
    rater_list <- list()
    for (hc in human_cols) {
      df <- humans[, c("img_id", hc)]
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
compare_multilabel_human_ai <- function(humans, models, consensus = TRUE, dieticians = NULL) { 
  out <- list(); idx <- 1 
  
  if (!is.null(dieticians)) {
    # merge with the rest of the human classifications
    humans <- inner_join(humans, dieticians, by = "img_id")
  }
  
  for (question in multi_label_vars) { 
    ai_col <- ai_mapping[[question]] 
    human_base <- human_mapping[[question]] 
    
    if (consensus) {
      human_cols <- paste0(human_base, "_(cons|dietcons)$")
    } else {
      # grab coder1/2/3 + consensus col names 
      human_cols <- grep( 
        paste0("^", human_base, "_(coder[1-3]|diet[1-3]|cons|dietcons)$"), 
        names(humans), value = TRUE 
      ) 
    }
    
    if (length(human_cols) < 1) next 
    
    # build full rater list
    rater_list <- list()
    for (hc in human_cols) {
      df <- humans[, c("img_id", hc)]
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


# ===== MORE ANALYSIS - by category of the brand =====
# how consistent are AI models in different product categories?
agreement_by_category_all <- function(models, column, humans = NULL, dieticians = NULL, multiple = FALSE) {
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
  
  # optionally add dieticians consensus
  if (!is.null(dieticians)) {
    diet_col <- human_mapping[[column]]
    consensus_col <- paste0(diet_col, "_dietcons")
    
    if (consensus_col %in% names(dieticians)) {
      df <- dieticians[, c("img_id", "category", consensus_col)]
      colnames(df) <- c("img_id", "category", "label")
      df$model <- "diet_consensus"
      raters[["diet_consensus"]] <- df
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

# by label for single columns
single_disagreement_by_label <- function(models, humans = NULL, column, dieticians = NULL) {
  
  merged <- merge_ai_human_column(models, humans, column, dieticians)
    #filter(!is.na(consensus))  # remove rows with no consensus
  
  label_col <- if (!is.null(humans)) "consensus" else "dietcons"
  # count for each consensus label
  results <- merged %>%
    rowwise() %>%
    mutate(
      label = .data[[label_col]],
      n_match = sum(c_across(all_of(names(models))) == label, na.rm = TRUE),
      all_failed = n_match == 0,
      across(all_of(names(models)), ~ .x != label, .names = "failed_{.col}")
    ) %>%
    ungroup() %>%
    group_by(label) %>%
    summarise(
      total = n(),
      failed_all_n = sum(all_failed),
      failed_all_pct = mean(all_failed),
      across(starts_with("failed_"), list(
        n = ~sum(.x, na.rm = TRUE),
        pct = ~mean(.x, na.rm = TRUE)
      )),
      .groups = "drop"
    ) %>%
    arrange(desc(failed_all_pct))
  
  return(results)
}

# by label for multi-label
multi_disagreement_by_label <- function(models, humans = NULL, column, dieticians = NULL) {
  
  merged <- merge_ai_human_column(models, humans, column, dieticians)
    #filter(!is.na(consensus))  # filter empty rows
  
  # get unique labels from consensus
  label_col <- if (!is.null(humans)) "consensus" else "dietcons"
  all_labels <- unique(unlist(strsplit(merged[[label_col]], ",\\s*")))
  all_labels <- all_labels[all_labels != ""]
  
  results <- list()
  
  for (label in all_labels) {
    rows_with_label <- merged %>%
      filter(grepl(paste0("(^|,\\s*)", label, "(\\s*,|$)"),  if (!is.null(humans)) consensus else dietcons))
    
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
        all_failed = sum(all_failed),
        across(starts_with("failed_"), sum)
      )
    
    results[[label]] <- data.frame(
      label = label,
      total = n_total,
      failed_all_n = summary_counts$all_failed,
      failed_all_pct = summary_counts$all_failed / n_total,
      # add per-model missed counts and percentages
      do.call(cbind, lapply(names(models), function(m) {
        n_failed <- summary_counts[[paste0("failed_", m)]]
        setNames(
          data.frame(n_failed, n_failed / n_total),
          c(paste0("failed_", m, "_n"), paste0("failed_", m, "_pct"))
        )
      }))
    )
  }
  
  bind_rows(results) %>% arrange(desc(failed_all_pct))
}






