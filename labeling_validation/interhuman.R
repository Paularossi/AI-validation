library(dplyr)
library(tidyr)
library(irr)
library(ggplot2)
library(writexl)
library(purrr)
library(readxl)


root_folder <- "C:/Users/P70090005/Documents/survey results + invoices/prolific/"

responses_dutch <- read.csv(paste(root_folder, "dutch/Dutch survey values.csv", sep=""))
responses_french <- read.csv(paste(root_folder, "french/French survey values.csv", sep=""))
responses_english <- read.csv(paste(root_folder, "english/English survey values.csv", sep=""))

# remove row 1 and 2
responses_dutch <- responses_dutch[-c(1,2), ]
responses_french <- responses_french[-c(1,2), ]
responses_english <- responses_english[-c(1,2), ]

demo_dutch <- read.csv(paste(root_folder, "dutch/demographic data NL.csv", sep=""))
demo_french <- read.csv(paste(root_folder, "french/demographic data FR.csv", sep=""))
demo_english <- read.csv(paste(root_folder, "english/demographic data EN.csv", sep=""))

# keep only the approved responses from the demographic data
responses_dutch_clean <- responses_dutch %>%
  semi_join(demo_dutch, by = c("workerId" = "Participant.id")) %>%
  select(-c("Progress", "ResponseId", "RecordedDate"))

responses_french_clean <- responses_french %>%
  semi_join(demo_french, by = c("workerId" = "Participant.id")) %>%
  select(-c("Progress", "ResponseId", "RecordedDate"))

responses_english_clean <- responses_english %>%
  semi_join(demo_english, by = c("workerId" = "Participant.id")) %>%
  select(-c("Progress", "ResponseId", "RecordedDate"))

# add the language
responses_dutch_clean$language <- "Dutch"
responses_french_clean$language <- "French"
responses_english_clean$language <- "English"

# bind all responses together
responses_all <- bind_rows(responses_dutch_clean, responses_french_clean, responses_english_clean)
names(responses_all)

# remove unnecessary columns and add duration in minutes
responses_all <- responses_all %>% 
  select(-c("Status", "IPAddress", "Finished", "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
            "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage")) %>%
  mutate(Duration..in.seconds. = as.numeric(Duration..in.seconds.)) %>%
  mutate(Duration_minutes = Duration..in.seconds. / 60)

# filter out the responses with no generated images
responses_all <- responses_all %>% filter(Image1_ID != "")

nrow(responses_all)  # total number of worker-image answers
n_distinct(responses_all$workerId)  # total number of unique participants

###========================================================================
# add the tables from aws???


# check duration per response
ggplot(responses_all, aes(x = language, y = Duration_minutes)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Response Duration by Language",
       x = "Language",
       y = "Duration (minutes)") +
  theme_minimal()

ggsave("C:/Users/P70090005/Documents/AI-validation/gpu outputs/tt/survey_times.png", width = 9, height = 5)

# do a quick hypothesis test
kruskal_test <- kruskal.test(Duration_minutes ~ language, data = responses_all)
print(kruskal_test) # there seems to be a significant difference indeed



variables <- c("ad_type", "alcohol", "non_alcohol", "target_group", "prem_offer", "marketing_str", "who_cat")
# reshape into long format - one row has coding for one image only
reshape_qualtrics_responses <- function(responses_clean, variables = variables) {
  # to store reshaped image responses
  reshaped_list <- list()
  
  for (i in 1:5) {
    # map old column names to new names
    var_oldnames <- paste0(variables, "_image", i)
    var_newnames <- variables
    
    meta_oldnames <- c(paste0("Image", i, "_ID"),
                       paste0("Image", i, "_Brand"),
                       paste0("Image", i, "_Text"),
                       paste0("Image", i, "_URL"))
    meta_newnames <- c("Image_ID", "Image_Brand", "Image_Text", "Image_URL")
    
    # check if all expected columns exist
    cols_exist <- var_oldnames %in% names(responses_clean)
    meta_exist <- meta_oldnames %in% names(responses_clean)
    
    # otherwise throw a warning
    if (!all(cols_exist) || !all(meta_exist)) {
      warning(paste("Skipping image", i, "- missing columns"))
      next
    }
    
    # rename the columns
    temp <- responses_clean %>%
      select(workerId, all_of(var_oldnames), all_of(meta_oldnames)) %>%
      rename_with(~var_newnames, all_of(var_oldnames)) %>%
      rename_with(~meta_newnames, all_of(meta_oldnames)) %>%
      mutate(image_number = i)
    
    reshaped_list[[i]] <- temp
  }
  
  responses_long <- bind_rows(reshaped_list) %>%
    select(workerId, Image_ID, everything())
  
  return(responses_long)
}

responses_long_all <- reshape_qualtrics_responses(responses_all, variables = variables)

n_distinct(responses_long_all$workerId)  # total number of unique participants
n_distinct(responses_long_all$Image_ID)  # total number of unique images

# check which images have less than 3 codings
responses_long_all %>% group_by(Image_ID) %>% count() %>%
  filter(n < 3)

# now do the alcohol manipulation (new ad type and who_cat)
responses_long_all <- responses_long_all %>%
  mutate(
    # adjust type_ad: overwrite with 9 if alcohol == 1
    new_type_ad = ifelse(alcohol == 1, "9", ad_type),
    
    # adjust who_cat: append "A" or replace with "A" if NA/empty
    new_who_cat = case_when(
      alcohol == 1 & (is.na(who_cat) | who_cat == "") ~ "A",
      alcohol == 1 & !(is.na(who_cat) | who_cat == "") ~ paste("A", who_cat, sep = ","),
      TRUE ~ who_cat
    )
  )

responses_long_all %>%
  filter(alcohol == 1) %>%
  select(workerId, Image_ID, alcohol, ad_type, new_type_ad, who_cat, new_who_cat) %>%
  head(10)

# check for duplicates
duplicates_overall <- responses_long_all %>%
  group_by(Image_ID, workerId) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

# extract all rows from responses_long_clean that match these keys
duplicated_rows <- responses_long_all %>%
  semi_join(duplicates_overall, by = c("Image_ID", "workerId")) %>%
  arrange(workerId)

responses_long_all <- responses_long_all %>%
  group_by(Image_ID, workerId) %>%
  slice(1) %>%
  ungroup()


# standardize the values in the WHO cat columns
clean_who_cat <- function(x) {
  if (is.na(x)) return("NA")
  
  # split on comma and trim spaces
  cats <- unlist(strsplit(x, ",\\s*"))
  
  # remove 99
  cats <- cats[cats != "99"]
  
  if (length(cats) == 0) return("NA")  # 99 should become "NA"
  
  # convert 4.1 to 4a, and so on
  cats <- gsub("4\\.1", "4a", cats)
  cats <- gsub("4\\.2", "4b", cats)
  cats <- gsub("4\\.3", "4c", cats)
  cats <- gsub("4\\.4", "4d", cats)
  cats <- gsub("4\\.5", "4e", cats)
  cats <- gsub("4\\.6", "4f", cats)
  
  cats <- sort(cats)
  
  # recombine without space
  return(paste(cats, collapse = ","))
}

responses_long_all <- responses_long_all %>%
  mutate(who_cat_clean = sapply(new_who_cat, clean_who_cat))
# who_cat_clean is the correct column now


# re-index coder numbers per image_id (drop workerId labels)
responses_long_indexed <- responses_long_all %>%
  group_by(Image_ID) %>%
  arrange(Image_ID) %>%
  mutate(coder_number = paste0("coder", row_number())) %>%
  ungroup()

write_xlsx(responses_long_indexed, paste(root_folder, "clean results all/responses_all_long.xlsx", sep=""))


# now pivot wider for all variables
responses_wide_all <- responses_long_indexed %>%
  pivot_wider(
    id_cols = Image_ID,
    names_from = coder_number,
    values_from = c(ad_type, new_type_ad, target_group, alcohol, non_alcohol, prem_offer, marketing_str, who_cat, who_cat_clean)
  )

write_xlsx(responses_wide_all, paste(root_folder, "clean results all/responses_all_wide.xlsx", sep=""))

names(responses_wide_all)

# build the consensus columns for single choice questions
responses_human_clean <- responses_wide_all %>%
  select(Image_ID)

single_choice_vars <- c("ad_type", "new_type_ad", "target_group", "alcohol", "non_alcohol")

for (var in single_choice_vars) {
  
  coder_cols <- paste0(var, "_coder", 1:3)
  
  # add only 3 coder columns
  responses_human_clean[coder_cols] <- responses_wide_all[coder_cols]
  
  # compute consensus via majority vote (most frequent value)
  consensus_col <- paste0(var, "_cons")
  responses_human_clean[[consensus_col]] <- responses_wide_all[coder_cols] %>%
    pmap_chr(~ {
      vals <- c(...)
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return("NA")
      tab <- table(vals)
      # in case of tie return first
      names(tab)[which.max(tab)]
    })
}

# for multi label columns take a mix of intersection/union
multi_label_vars <- c("prem_offer", "marketing_str", "who_cat", "who_cat_clean")

normalize_label_set <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  sort(trimws(unlist(strsplit(x, ","))))
}

multi_label_consensus <- function(..., return_string = TRUE) {
  labels <- list(...)
  sets <- lapply(labels, normalize_label_set)
  
  # if any are empty, remove them
  sets <- sets[lengths(sets) > 0]
  if (length(sets) == 0) return("NA")
  
  # compare all pairwise
  matches <- sapply(1:length(sets), function(i) {
    sum(sapply(1:length(sets), function(j) identical(sets[[i]], sets[[j]])))
  })
  
  # if any exact match (at least 2 coders gave same string)
  if (any(matches >= 2)) {
    agreed_set <- sets[[which.max(matches)]]
    return(if (return_string) paste(agreed_set, collapse = ",") else agreed_set)
  }
  
  # no agreement: take intersection
  intersection <- Reduce(intersect, sets)
  if (length(intersection) > 0) {
    return(if (return_string) paste(sort(intersection), collapse = ",") else intersection)
  }
  
  # if even intersection is empty, return union
  union_set <- Reduce(union, sets)
  return(if (return_string) paste(sort(union_set), collapse = ",") else union_set)
}


responses_multi_clean <- responses_wide_all %>%
  select(Image_ID)

for (var in multi_label_vars) {
  
  coder_cols <- paste0(var, "_coder", 1:3)
  responses_multi_clean[coder_cols] <- responses_wide_all[coder_cols]
  
  consensus_col <- paste0(var, "_cons")
  responses_multi_clean[[consensus_col]] <- pmap_chr(responses_wide_all[coder_cols], multi_label_consensus)
}

responses_human_all <- left_join(responses_human_clean, responses_multi_clean, by = "Image_ID")
# THIS IS THE MAIN DATAFRAME TO BASE THE ANALYSIS ON!!!
write_xlsx(responses_human_all, paste(root_folder, "clean results all/responses_human_final.xlsx", sep=""))


# ================ DATA PROCESSING ENDS HERE ========================



# ==============================================================================
# below, we have an analysis of agreement between humans only!!!!

responses_human_all <- read_excel(paste(root_folder, "clean results all/responses_human_final.xlsx", sep=""))
single_choice_vars <- c("ad_type", "new_type_ad", "target_group", "alcohol", "non_alcohol")

# check the single choice questions
compute_agreement_columns <- function(responses_wide, variable_cols) {
  
  results <- lapply(variable_cols, function(var_name) {
    # auto-select the coder columns for this variable
    coder_cols <- grep(paste0("^", var_name, "_coder[123]$"), names(responses_wide), value = TRUE)
    
    if (length(coder_cols) != 3) {
      warning(paste("Skipping", var_name, ": found", length(coder_cols), "coder columns (expected 3)"))
      return(NULL)
    }
    
    human_matrix <- responses_wide %>%
      select(all_of(coder_cols)) %>%
      as.matrix()
    
    # Fleiss' Kappa
    fleiss_result <- kappam.fleiss(human_matrix)
    
    # Full agreement prop
    prop_full_agreement <- mean(apply(human_matrix, 1, function(x) length(unique(x)) == 1))
    
    data.frame(
      variable = var_name,
      fleiss_kappa = fleiss_result$value,
      prop_full_agreement = prop_full_agreement
    )
  })
  
  bind_rows(results[!sapply(results, is.null)])
}


agreement_summary <- compute_agreement_columns(responses_wide = responses_wide_all, variable_cols = single_choice_vars)

agreement_long <- agreement_summary %>%
  pivot_longer(cols = c(fleiss_kappa, prop_full_agreement),
               names_to = "metric", values_to = "value")

ggplot(agreement_long, aes(x = variable, y = value, fill = metric)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = round(value, 2)),
            position = position_dodge(0.7), 
            vjust = -0.3, size = 3.5) +
  labs(title = "Inter-Human Agreement per Variable",
       x = "Variable",
       y = "Agreement Value",
       fill = "Metric") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/P70090005/Documents/AI-validation/gpu outputs/tt/human_single_fleiss_kappa.png", width = 9, height = 5)


# now for multi-label columns
jaccard_similarity <- function(str1, str2) {
  set1 <- trimws(unlist(strsplit(str1, ",")))
  set2 <- trimws(unlist(strsplit(str2, ",")))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  if (union == 0) return(NA)  # to handle empty annotations
  return(intersection / union)
}

compute_multilabel_human_jaccard <- function(responses_wide_all, variable_list) {
  
  all_results <- lapply(variable_list, function(var_name) {
    # Select coder columns for this variable (e.g., who_cat_coder1, who_cat_coder2, etc.)
    coder_cols <- grep(paste0("^", var_name, "_coder[123]$"), names(responses_wide_all), value = TRUE)
    
    if (length(coder_cols) != 3) {
      warning(paste("Skipping", var_name, ": found", length(coder_cols), "coder columns (expected 3)"))
      return(NULL)
    }
    
    # Build a dataframe similar to AI models format: each coder as a column
    df <- responses_wide_all %>%
      select(all_of(coder_cols))
    
    names(df) <- paste0("coder", 1:3)
    
    # Compute pairwise Jaccard similarities, just like AI function
    model_pairs <- combn(names(df), 2, simplify = FALSE)
    
    avg_pairwise_jaccard <- sapply(model_pairs, function(pair) {
      sims <- mapply(jaccard_similarity, df[[pair[1]]], df[[pair[2]]])
      mean(sims, na.rm = TRUE)
    })
    
    names(avg_pairwise_jaccard) <- sapply(model_pairs, function(p) paste(p, collapse = "_"))
    
    data.frame(
      variable = var_name,
      t(avg_pairwise_jaccard),
      row.names = NULL
    )
  })
  summary_table <- bind_rows(all_results[!sapply(all_results, is.null)])
  
  return(summary_table)
}


# Variables with multi-label answers
multi_label_vars <- c("who_cat", "prem_offer", "marketing_str")

# Run Jaccard agreement analysis for humans
human_multilabel_jaccard <- compute_multilabel_human_jaccard(responses_wide_all, multi_label_vars)

human_multilabel_jaccard <- human_multilabel_jaccard %>%
  mutate(mean_jaccard = rowMeans(select(., starts_with("coder")), na.rm = TRUE))

# Pivot coder pairs and mean into long format for plotting
human_multilabel_jaccard_long <- human_multilabel_jaccard %>%
  pivot_longer(cols = starts_with("coder"), names_to = "pair", values_to = "jaccard") %>%
  bind_rows(
    human_multilabel_jaccard %>%
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
    "coder1_coder2" = "#66c2a5",  # greenish
    "coder1_coder3" = "#fc8d62",  # orange
    "coder2_coder3" = "#8da0cb",  # blueish
    "Mean" = "#1b7837"            # dark green for mean
  )) +
  labs(title = "Human Agreement for Multi-Label Variables",
       x = "Variable",
       y = "Jaccard Similarity",
       fill = "Pair / Mean") +
  ylim(0, 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/P70090005/Documents/AI-validation/gpu outputs/plots/human_mltpl_jaccard_sim.png", width = 9, height = 5)




########## COMPARE HUMANS WITH THE AIS
ai_mapping <- list(
  "ad_type" = "type_ad",
  "new_type_ad" = "type_ad",  # still compared to same AI col
  "target_group" = "target_group",
  "alcohol" = "is_alcohol"
)

human_mapping <- list(
  "ad_type" = "ad_type_cons",
  "new_type_ad" = "new_type_ad_cons",
  "target_group" = "target_group_cons",
  "alcohol" = "alcohol_cons"
)

compare_human_ai_kappa <- function(humans, models, ai_mapping, human_mapping) {
  # Fix image IDs in human data
  humans$Image_ID <- gsub("\\.png$", "", humans$Image_ID)
  
  results <- list()
  
  for (question in names(ai_mapping)) {
    ai_column <- ai_mapping[[question]]
    human_column <- human_mapping[[question]]
    
    for (model_name in names(models)) {
      model_df <- models[[model_name]]
      
      # Safety check
      if (!(ai_column %in% names(model_df)) || !(human_column %in% names(humans))) {
        warning(paste("Missing:", ai_column, "or", human_column))
        next
      }
      
      # Join on image ID
      merged <- merge(
        humans[, c("Image_ID", human_column)],
        model_df[, c("img_id", ai_column)],
        by.x = "Image_ID", by.y = "img_id"
      )
      
      # Drop rows with missing labels
      merged <- merged[!is.na(merged[[human_column]]) & !is.na(merged[[ai_column]]), ]
      
      # Compute kappa
      kappa_val <- if (nrow(merged) > 0) {
        tryCatch({
          irr::kappa2(merged[, c(human_column, ai_column)])$value
        }, error = function(e) NA)
      } else {
        NA
      }
      
      results[[length(results) + 1]] <- data.frame(
        question = question,
        model = model_name,
        kappa = kappa_val
      )
    }
  }
  
  return(do.call(rbind, results))
}

results_kappa <- compare_human_ai_kappa(responses_human_all, models, ai_mapping, human_mapping)
print(results_kappa)

ggplot(results_kappa, aes(x = question, y = kappa, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(kappa, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.3, size = 3) +
  labs(
    title = "Cohen's Kappa: Human Consensus vs AI Models",
    x = "Question",
    y = "Kappa Value"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )
ggsave("C:/Users/P70090005/Documents/AI-validation/gpu outputs/plots/human_ai_single.png", width = 9, height = 5)



compare_multilabel_ai_vs_human <- function(human_df, ai_models, multi_label_vars) {
  results <- list()
  
  for (question in multi_label_vars) {
    human_col <- paste0(question, "_cons")
    
    for (model_name in names(ai_models)) {
      ai_df <- ai_models[[model_name]]
      
      if (!(question %in% colnames(ai_df))) next
      
      df <- merge(
        human_df[, c("Image_ID", human_col)],
        ai_df[, c("img_id", question)],
        by.x = "Image_ID", by.y = "img_id"
      )
      
      sims <- mapply(jaccard_similarity, df[[human_col]], df[[question]])
      mean_sim <- mean(sims, na.rm = TRUE)
      
      results[[length(results) + 1]] <- data.frame(
        variable = question,
        model = model_name,
        jaccard = mean_sim
      )
    }
  }
  
  do.call(rbind, results)
}


jaccard_summary <- compare_multilabel_ai_vs_human(responses_human_all, models, multi_label_vars)

ggplot(jaccard_summary, aes(x = variable, y = jaccard, fill = model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Jaccard Similarity: Human vs AI (Multi-label Questions)",
    x = "Question",
    y = "Jaccard Similarity"
  ) +
  geom_text(aes(label = round(jaccard, 2)), position = position_dodge(0.9), vjust = -0.3) +
  theme_minimal()





