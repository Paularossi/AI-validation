
library(readxl)
library(dplyr)
library(irr)
library(tidyr)
library(ggplot2)


root_folder <- "C:/Users/P70090005/Documents/gpu outputs/"

###### START ANALYSIS HERE
#original_labeling <- read_excel(paste(root_folder, "validation results/original_coding.xlsx", sep=""))
dig_coding <- read_excel(paste(root_folder, "digital_coding_clean.xlsx", sep=""))

### LOAD THE AI-LABELLED DATA
gemma <- read_excel(paste(root_folder, "gemma_all.xlsx", sep=""))
#gpt <- read_excel(paste(root_folder, "validation results/gpu/gpt4o_20250327_133400.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all.xlsx", sep=""))

dig_coding <- dig_coding[dig_coding$img_id %in% qwen$img_id, ]
dig_coding <- dig_coding[order(dig_coding$img_id), ]
dig_coding[] <- lapply(dig_coding, as.character)

gemma <- gemma[gemma$img_id %in% qwen$img_id, ]

# sort each dataframe by img_id
gemma <- gemma[order(gemma$img_id), ]
gpt <- gpt[order(gpt$img_id), ]
pixtral <- pixtral[order(pixtral$img_id), ]
qwen <- qwen[order(qwen$img_id), ]

compare_agreement <- function(colname, models, dig_coding = NULL) {
  # filter the models that contain the colname
  valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
  
  # extract the relevant column from each model into a named list
  model_cols <- lapply(valid_models, function(df) df[[colname]])
  names(model_cols) <- names(valid_models)
  
  df <- as.data.frame(model_cols)
  
  n <- nrow(df)
  
  # if dig_coding exists and has the column, include it
  has_coder <- !is.null(dig_coding) && colname %in% colnames(dig_coding)
  if (has_coder) {
    df$coder <- dig_coding[[colname]]
  }
  
  # full agreement proportion
  full_agreement_prop <- mean(apply(df, 1, function(x) length(unique(x)) == 1))
  
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
    full_agreement_prop = full_agreement_prop,
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
      prop_full_agreement = res$full_agreement_prop,
      fleiss_kappa = res$fleiss_kappa,
      kappa_df
    )
  })
  
  do.call(rbind, rows)
}

questions <- c("type_ad", "target_group", "is_alcohol")
models <- list(gemma = gemma, pixtral = pixtral, qwen = qwen)
all_results <- lapply(questions, function(q) {compare_agreement(q, models, dig_coding)})
names(all_results) <- questions

agreement_summary <- summarize_agreement(all_results)

print(agreement_summary)

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

ggsave(paste(root_folder, "validation results/gpu/pairwise_kappa_heatmap.png", sep=""), width = 9, height = 5)


ggplot(agreement_summary, aes(x = reorder(question, fleiss_kappa), y = fleiss_kappa)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Fleiss' Kappa per Question", x = "Question", y = "Fleiss' Kappa") +
  theme_minimal()

ggsave(paste(root_folder, "validation results/gpu/fleiss_kappa_barplot.png", sep=""), width = 8, height = 5)


# multiple choice columns
# Jaccard similarity measures how much two models agree on the set of labels they assign to the same ad
# example: model1 = {1, 2} ; model2 = {2, 3}
# intersection = {2}, union = {1, 2, 3} => Jaccard = 1 / 3 = 0.33
jaccard_similarity <- function(str1, str2) {
  set1 <- trimws(unlist(strsplit(str1, ",")))
  set2 <- trimws(unlist(strsplit(str2, ",")))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  if (union == 0) return(NA)  # to handle empty annotations
  return(intersection / union)
}


compare_multilabel_agreement <- function(colname, models, dig_coding) {
  
  valid_models <- models[sapply(models, function(df) colname %in% colnames(df))]
  
  # extract the relevant column from each model into a named list
  model_cols <- lapply(valid_models, function(df) df[[colname]])
  names(model_cols) <- names(valid_models)
  
  if (!is.null(dig_coding) && colname %in% colnames(dig_coding)) {
    model_cols$coder <- dig_coding[[colname]]
  }
  
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
  
multi_label_cols <- c("marketing_str", "prem_offer", "who_cat")

multi_results <- lapply(multi_label_cols, function(col) {
  compare_multilabel_agreement(col, models, dig_coding)
})
names(multi_results) <- multi_label_cols

multi_summary <- bind_rows(lapply(multi_results, function(res) {
  data.frame(
    question = res$column,
    t(as.data.frame(res$pairwise_jaccard))
  )
}), .id = NULL)

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

ggsave(paste(root_folder, "validation results/gpu/jaccard_multilabel_heatmap.png", sep=""), width = 9, height = 5)


# confusion matrices
addmargins(table(GPT=gpt$type_ad, Qwen=qwen$type_ad))
addmargins(table(GPT=gpt$type_ad, coder=dig_coding$type_ad))
addmargins(table(GPT=gpt$target_group, Aya=aya$target_group))
addmargins(table(GPT=gpt$target_group, Gemma=gemma$target_group))



# look at the distribution of response time
response_df <- bind_rows(
  data.frame(model = "aya", response_time = aya$response_time),
  data.frame(model = "gemma", response_time = gemma$response_time),
  data.frame(model = "gpt", response_time = gpt$response_time),
  data.frame(model = "pixtral", response_time = pixtral$response_time),
  data.frame(model = "qwen", response_time = qwen$response_time)
)

ggplot(response_df, aes(x = model, y = response_time, fill = model)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Response Time per Model (log scale)",
       x = "Model",
       y = "Response Time (log seconds)") +
  theme(legend.position = "none")

ggsave(paste(root_folder, "validation results/gpu/response_time_distr.png", sep=""), width = 8, height = 5)

response_df %>%
  group_by(model) %>%
  summarise(
    mean_time = mean(response_time, na.rm = TRUE),
    median_time = median(response_time, na.rm = TRUE),
    sd_time = sd(response_time, na.rm = TRUE),
    min_time = min(response_time, na.rm = TRUE),
    max_time = max(response_time, na.rm = TRUE)
  )





##################################################
# analyze the rest of the columns with multiple categories

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

plot_kappa_results <- function(kappa_df, title = "Cohen's Kappa by Category") {
  ggplot(kappa_df, aes(x = all_offers_text[category], y = kappa)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = round(p_value, 3)), vjust = -0.5) +
    theme_minimal() +
    labs(title = title, x = "Category", y = "Cohen's Kappa") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


############## PREMIUM OFFERS
kappa_qwen_vs_gpt <- compare_multilabel_kappa(qwen, gpt, "prem_offer", "qwen", "gpt")
kappa_gpt_vs_coder <- compare_multilabel_kappa(gpt, dig_coding, "prem_offer", "gpt", "coder")


# look at them together
kappa_qwen_vs_gpt$compared_to <- "qwen"
kappa_gpt_vs_coder$compared_to <- "coder"
kappa_combined <- rbind(kappa_qwen_vs_gpt, kappa_gpt_vs_coder)

ggplot(kappa_combined, aes(x = all_offers_text[category], y = kappa, fill = compared_to)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(p_value, 3)), position = position_dodge(0.9), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Premium Offers Agreement of GPT",
       x = "Category", y = "Kappa", fill = "Compared to") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))
ggsave(paste(root_folder, "validation results/gpu/prem_offers_gpt_qwen_coder.png", sep=""), width = 8, height = 5)


############## MARKETING STRATEGIES
all_marketing_str_text <- c("0" = "None", "1" = "Cartoons", "2" = "Licensed char", "3" = "Amateur sports", "4" = "Celebrity",
                            "5" = "Movie char", "6" = "Famous sports", "7" = "Events", "8" = "Age-targeted", 
                            "9" = "Awards", "10" = "Sport events")

kappa_qwen_pixtral <- compare_multilabel_kappa(qwen, pixtral, "marketing_str", "qwen", "pixtral")
kappa_gpt_pixtral <- compare_multilabel_kappa(gpt, pixtral, "marketing_str", "gpt", "pixtral")
kappa_gpt_coder <- compare_multilabel_kappa(gpt, dig_coding, "marketing_str", "gpt", "coder")
kappa_qwen_gpt <- compare_multilabel_kappa(qwen, gpt, "marketing_str", "qwen", "gpt")
kappa_qwen_pixtral<- compare_multilabel_kappa(qwen, pixtral, "marketing_str", "qwen", "pixtral")
kappa_qwen_gemma <- compare_multilabel_kappa(qwen, gemma, "marketing_str", "qwen", "gemma")

kappa_qwen_gpt$compared_to <- "gpt"
kappa_qwen_pixtral$compared_to <- "pixtral"
kappa_qwen_gemma$compared_to <- "gemma"

#kappa_qwen_pixtral$compared_to <- "pixtral"
kappa_combined <- rbind(kappa_qwen_gpt, kappa_qwen_pixtral, kappa_qwen_gemma)

ggplot(kappa_combined, aes(x = all_marketing_str_text[category], y = kappa, fill = compared_to)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(p_value, 3)), position = position_dodge(0.9), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Marketing Strategies Agreement of Qwen",
       x = "Category", y = "Kappa", fill = "Compared to") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1))
ggsave(paste(root_folder, "validation results/gpu/marketing_str_qwen_gpt_pixtral_gemma.png", sep=""), width = 9, height = 5)


table(dig_coding$marketing_str)







