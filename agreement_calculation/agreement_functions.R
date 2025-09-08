library(dplyr)
library(irr)
library(tidyverse)
library(irrCAC) # to calculate Gwet's AC1/AC2 agreement coefficient
library(psych) # for kappa confidence interval
library(stringi)   # for accent/diacritic removal

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

# labels for all questions
label_df <- read.csv("C:/Users/P70090005/Desktop/phd/AI-validation/data/label_mappings.csv", 
                     stringsAsFactors = FALSE, na = c(""))

`%!in%` = Negate(`%in%`) # define new `not in` function

# function to merge data for a given column of ai models and human consensus
merge_ai_human_column <- function(models, humans = NULL, column, dieticians = NULL) {
  model_names <- names(models)
  
  # extract relevant model columns
  model_preds <- lapply(model_names, function(model) {
    df <- models[[model]]
    col <- ifelse(column != "brand", ai_mapping[[column]], column)
    df %>% select(img_id, !!sym(col)) %>% rename(!!model := !!sym(col))
  }) %>% reduce(full_join, by = "img_id")
  
  if (!is.null(humans)) {
    # add human consensus
    consensus_col <- paste0(human_mapping[[column]], "_cons")
    human_cons <- humans %>%
      select(img_id, consensus = !!sym(consensus_col), category)
  }
  
  if (!is.null(dieticians)) {
    col <- ifelse(column != "brand", human_mapping[[column]], column)
    diet_cols <- grep(paste0("^", col, "_(diet[1-3]|dietcons)$"), names(dieticians), value = TRUE)
    
    if (length(diet_cols) > 0) {
      diet_dt <- as.data.table(dieticians)[, c("img_id", diet_cols), with = FALSE]
      setnames(diet_dt, 
               old = diet_cols,
               new = c("diet1", "diet2", "diet3", "dietcons")[1:length(diet_cols)])
      result <- merge(result, diet_dt, by = "img_id", all = TRUE)
    }
  }
  
  if (!is.null(humans) & !is.null(dieticians))
    merged <- full_join(full_join(model_preds, human_cons, by = "img_id"), diet_preds, by="img_id")
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
          n = nrow(merged),
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
        n = nrow(merged),
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
  # handle NA cases early
  if (is.na(x) || is.na(y)) return(NA_real_)
  
  # define the labels for each rater
  lab_x <- strsplit(x, sep, fixed = TRUE)[[1]]
  lab_y <- strsplit(y, sep, fixed = TRUE)[[1]]
  
  # compute set diff and intersection size
  intersection_size <- length(intersect(lab_x,lab_y)) # number of elements in common between two sets
  diff_xy_size <- length(lab_x) - intersection_size # number of elements in set x but not in set y
  diff_yx_size <- length(lab_y) - intersection_size # number of elements in set y but not in set x
  
  # monotonicity similarity coefficient, M, see http://www.lrec-conf.org/proceedings/lrec2006/pdf/636_pdf.pdf Rebecca Passonneau. 2006. Measuring Agreement on Set-valued Items (MASI) for Semantic and Pragmatic Annotation. In Proceedings of the Fifth International Conference on Language Resources and Evaluation (LREC’06), Genoa, Italy. European Language Resources Association (ELRA).
  m_sim <- if (diff_xy_size == 0 && diff_yx_size == 0) {
    1 # the sets are identical, return 1
  } else if (diff_xy_size == 0 || diff_yx_size == 0) {
    2/3 # one set is a subset of the other, return 2/3
  } else if (intersection_size > 0) {
    1/3 # some overlap, some non-overlap in each set, return 1/3
  } else {
    0 # disjoint sets, return 0
  }
  
  # calculate Jaccard simmilarity; J=1 means same, J=0 means no overlap at all. See https://en.wikipedia.org/wiki/Jaccard_index
  jaccard_sim <- intersection_size/(length(lab_x) + length(lab_y) - intersection_size)
  
  #MASI sim is M*J; MASI dist is 1-M*J
  masi_sim <- if (jaccard_only) jaccard_sim else m_sim * jaccard_sim
  return(if (type == "sim") masi_sim else 1 - masi_sim)
}


MASI_simmilarity_matrix <- function(df, sep = ", ") {
  labels_all_combos <- sort(unique(unlist(df))) # alphabetical sorted list of all strings of labels
  
  n_labels <- length(labels_all_combos) # number of combinations above
  
  # use outer() for vectorized computation
  masi_sim_mat <- outer(labels_all_combos, labels_all_combos, 
                        Vectorize(function(x, y) masi(x, y, sep = sep)))
  
  dimnames(masi_sim_mat) <- list(labels_all_combos, labels_all_combos)
  
  return(masi_sim_mat)
}


# Krippendorff's alpha using binary decomposition
compute_kripp_alpha_binary <- function(human, ai, labels) {
  human_norm <- gsub("\\s*,\\s*", ",", tolower(human))
  ai_norm    <- gsub("\\s*,\\s*", ",", tolower(ai))
  
  # build 2 × N matrix: rows = raters, cols = items
  out <- sapply(labels, function(lbl) {
    lbl_esc <- paste0("\\Q", tolower(lbl), "\\E")  # escape all metacharacters
    h_bin <- grepl(paste0("(^|,)", lbl_esc, "(,|$)"), human_norm, perl = TRUE)
    a_bin <- grepl(paste0("(^|,)", lbl_esc, "(,|$)"), ai_norm, perl = TRUE)
    
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
# if filter_other = TRUE, remove the ads where humans selected 'Other' for marketing strategies and prem offers
compare_multilabel_human_ai <- function(humans, models, consensus = TRUE, dieticians = NULL, multi_label_vars = multi_label_vars,
                                        filter_other = FALSE) { 
  out <- list(); idx <- 1 
  
  if (!is.null(dieticians)) {
    # merge with the rest of the human classifications
    humans <- inner_join(humans, dieticians, by = "img_id")
  }
  
  for (question in multi_label_vars) { 
    ai_col <- ifelse(question != "brand", ai_mapping[[question]], question)
    human_base <- ifelse(question != "brand", human_mapping[[question]], question)
    
    human_cols <- if (consensus) {
      grep(paste0("^", human_base, "_(cons|dietcons)$"), names(humans), value = TRUE)
    } else {
      grep(paste0("^", human_base, "_(coder[1-3]|diet[1-3]|cons|dietcons)$"), names(humans), value = TRUE)
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
      
      if (filter_other && question %in% c("prem_offer", "marketing_str")) {
        is_ai1 <- pair[1] %in% names(models)
        is_ai2 <- pair[2] %in% names(models)
        
        # only if exactly one rater is AI (AI–human pair)
        if (xor(is_ai1, is_ai2)) {
          human_rater_col <- if (is_ai1) pair[2] else pair[1]
          # find corresponding _text_ column for that human/diet rater (skip consensus)
          human_text_col <- if (human_rater_col %in% names(humans)) {
            find_text_col(human_base, human_rater_col, names(humans))
          } else NULL
          
          if (!is.null(human_text_col)) {
            merged <- merge(merged, humans[, c("img_id", human_text_col), drop = FALSE],
                            by = "img_id", all.x = TRUE)
            # keep only rows where human's text col is NA (didn't pick 'Other')
            merged <- merged[is.na(merged[[human_text_col]]), , drop = FALSE]
            if (nrow(merged) == 0) next
            merged <- merged[, setdiff(names(merged), human_text_col), drop = FALSE]
          }
          # if NULL (consensus) -> no filtering
        }
        # human–human or AI–AI pairs -> no filtering
      }
      
      # 1) avg Jaccard 
      sims <- mapply(jaccard_similarity, merged$label.x, merged$label.y) 
      jacc <- mean(sims, na.rm = TRUE) 
      
      # 2) binary Krippendorff's alpha
      # get all possible labels across human & ai strings
      all_labels <- unique(unlist(strsplit(
        na.omit(c(merged$label.x, merged$label.y)), split = ",\\s*"
      )))
      
      alpha_bin <- compute_kripp_alpha_binary(merged$label.x, merged$label.y, all_labels)
      
      # 3) Krippendorff's kappa using irrCOC
      alpha_unweighted <- krippen.alpha.raw(ratings=data.frame(merged$label.x, merged$label.y))$est
      
      # 4) Krippendorff's Alpha using MASI weights
      wt <- MASI_simmilarity_matrix(data.frame(merged$label.x, merged$label.y), sep = ", ")
      alpha_masi <- krippen.alpha.raw(ratings=data.frame(merged$label.x, merged$label.y), weights = wt)$est
      
      out[[idx]] <- data.frame( 
        question = question, 
        rater1 = pair[1],
        rater2 = pair[2],
        n = nrow(merged),
        jaccard = jacc,
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
  
  if (!is.null(dieticians)) 
    merged <- filter(merged, !is.na(merged$dietcons))
  
  #models <- lapply(models, function(model){ filter(model, model[img_id] %in% merged$img_id)})
  
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
  
  if (!is.null(dieticians)) 
    merged <- filter(merged, !is.na(merged$dietcons))
  
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


# normalize a single brand field into a vector of canonical brand tokens
clean_brand <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  
  if (trimws(x) %in% c("-", "No brand", "No brands", "No brand.", "No brands.")) 
    return("-") # keep "-" exactly as is (meaning 'no brand')
   
  x <- tolower(x)
  x <- str_remove_all(x, "\\(.*?\\)")                  # drop parenthetical descriptors (e.g., (bière), (fast food))
  x <- stri_trans_general(x, "Latin-ASCII")            # remove accents: é -> e
  x <- str_replace_all(x, " ", "") # remove whitespaces
  x <- str_replace_all(x, "[\"']", "")                 # <<< remove single & double quotes
  x <- str_replace_all(x, "&", " and ")                # optional normalization
  x <- str_replace_all(x, "[;|]", ",")                 # unify separators
  x <- str_replace_all(x, "\\s+", " ")                 # squeeze spaces
  
  brands <- str_split(x, ",", simplify = FALSE)[[1]]
  brands <- trimws(brands)
  brands <- brands[brands != ""]
  
  brands <- unique(brands)
  
  # return as a single comma-separated string
  paste(brands, collapse = ", ")
}


# helper: for human/diet columns like "<base>_coder1"/"<base>_diet2" return "<base>_text_coder1"/"_diet2"
# returns NULL for consensus columns
find_text_col <- function(human_base, rater_col, humans_names) {
  suf <- sub(paste0("^", human_base, "_"), "", rater_col)
  if (suf %in% c("cons", "dietcons")) return(NULL)  # never filter consensus
  hits <- grep(paste0("^", human_base, "_text_", suf, "$"),
               humans_names, value = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) return(NULL)
  hits[1]
}


