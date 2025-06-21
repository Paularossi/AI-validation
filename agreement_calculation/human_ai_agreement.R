library(readxl)
library(ggplot2)
library(ggplot2)
library(dplyr)

root_folder <- "C:/Users/P70090005/Documents/AI-validation/gpu outputs/"
responses_human_all <- read_excel(paste(root_folder, "responses_human_final.xlsx", sep=""))

gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all_1000.xlsx", sep=""))

single_choice_vars <- c("new_type_ad", "target_group", "alcohol")
multi_label_vars <- c("who_cat_clean", "prem_offer", "marketing_str")

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
  # fix image IDs in human data
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





