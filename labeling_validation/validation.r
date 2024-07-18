library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(irr)  
library(readr)

# find the ids of the images in the folder
all_files  <- list.files('data/validation sample/')
image_names <- all_files[grepl("\\.jpg$|\\.png$", all_files)]
image_names <- sub("\\.jpg$|\\.png$", "", image_names)
image_names_df <- data.frame(img_id = as.character(image_names), stringsAsFactors = FALSE)
write.csv(image_names_df, "data/validation sample/validation results/img_names.csv", row.names = FALSE)

### LOTS OF DATA PROCESSING
### FIRST, FOR THE ORIGINAL CODING
coding <- read.csv("data/Voedingsadvertenties_finaal.csv", sep = ";")
coding <- coding[ , -c(1, 3:22)] # remove the columns we don't need
coding <- as_tibble(coding)
head(coding, 2)
names(coding)

# first rename the columns
new_names <- c("img_id", "type_ad", "public_transport", "marketing_str", "marketing_str_descr", "prem_offer", "premium_descr",
                "nr_food", "food_name", "brand_name", "who_cat", "processed", "healthy_living", "health_recomm", "comments")
names(coding) <- new_names
head(coding, 2)

# then adjust the img ids
coding <- coding %>%
    mutate(img_id = gsub(".jpg", "", img_id))

# select the relevant columns
original_labeling <- coding %>% 
    select(img_id, type_ad, marketing_str, prem_offer, who_cat, processed, healthy_living) %>%
    mutate(across(everything(), as.character))
head(original_labeling)

matched_rows <- original_labeling %>% filter(img_id %in% image_names)

# missing images
image_names[!image_names %in% matched_rows$img_id]
write_xlsx(matched_rows, "data/validation sample/validation results/originaltemp.xlsx")

org <- read_excel("data/validation sample/validation results/original.xlsx")
matched2 <- org %>% filter(img_id %in% image_names)
image_names[!image_names %in% org$img_id]





# START ANALYSIS HERE
original_labeling <- read_excel("data/validation sample/validation results/original.xlsx")

# standardize empty strings to NA
standardize_missing <- function(column) {
    column <- ifelse(is.na(column) | column == "", "0", column)
    return(column)
}

original_labeling <- original_labeling %>%
    mutate(across(c(marketing_str, prem_offer), standardize_missing))
# this is the processed original labeling data
write_xlsx(original_labeling, "data/validation sample/validation results/original.xlsx")

# check the number of missing entries
missing_values_count <- original_labeling %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "question", values_to = "missing_count")

### LOAD THE AI-LABELLED DATA
ai_data_tree <- read_csv("data/validation sample/validation results/temp_tree_csv.csv",
                col_types = cols(.default = col_character()))
ai_data_two <- read_csv("data/validation sample/validation results/temp_two_csv.csv",
                col_types = cols(.default = col_character()))

names(ai_data_tree)

### START PERFORMANCE COMPARISON HERE

type_ad_test <- kappa2(cbind(as.numeric(original_labeling$type_ad), as.numeric(ai_data_tree$type_ad)))
prem_offer_test <- kappa2(cbind(as.numeric(original_labeling$prem_offer), as.numeric(ai_data_tree$prem_offer)))
marketing_str_test <- kappa2(cbind(as.numeric(original_labeling$marketing_str), as.numeric(ai_data_tree$marketing_str)))
who_cat_test <- kappa2(cbind(as.numeric(original_labeling$who_cat), as.numeric(ai_data_tree$who_cat)))
processed_test <- kappa2(cbind(as.numeric(original_labeling$processed), as.numeric(ai_data_tree$processed)))
healthy_test <- kappa2(cbind(as.numeric(original_labeling$healthy_living), as.numeric(ai_data_tree$healthy_living)))

cat("***TYPE AD*** Kappa Value:", type_ad_test$value, "; p-value:", type_ad_test$p.value, " \n")
cat("***PREMIUM OFFER*** Kappa Value:", prem_offer_test$value, "; p-value:", prem_offer_test$p.value, " \n")
cat("***MARKETING STR*** Kappa Value:", marketing_str_test$value, "; p-value:", marketing_str_test$p.value, " \n")
cat("***WHO CATEGORY*** Kappa Value:", who_cat_test$value, "; p-value:", who_cat_test$p.value, " \n")
cat("***PROCESSED*** Kappa Value:", processed_test$value, "; p-value:", processed_test$p.value, " \n")
cat("***HEALTHY LIVING*** Kappa Value:", healthy_test$value, "; p-value:", healthy_test$p.value, " \n")


table(original_labeling$who_cat, ai_data_tree$who_cat)




















"""
columns_to_compare <- c("type_ad", "marketing_str", "prem_offer", "who_cat", "processed", "healthy_living")

# compare AI and original coding and calculate accuracy
compare_columns <- function(df, column) {
    original_col <- df[[paste0(column, "_original")]]
    ai_col <- df[[paste0(column, "_AI")]]

    valid_indices <- !is.na(original_col)

    match_col <- original_col[valid_indices] == ai_col[valid_indices]
    accuracy <- sum(match_col, na.rm = TRUE) / length(match_col)
    return(accuracy)
}

#accuracy_results <- sapply(columns_to_compare, compare_columns, df = comparison)
# calculate aggregate accuracy
#aggregate_score <- mean(accuracy_results, na.rm = TRUE)

# merge original data with AI labeling data for each iteration and calculate accuracy
accuracy_list <- lapply(ai_data, function(ai_df) {
  comparison <- original_labeling %>%
    inner_join(ai_df, by = "img_id", suffix = c("_original", "_AI"))
  
  accuracy_results <- sapply(columns_to_compare, compare_columns, df = comparison)
  accuracy_results
})

# combine all accuracies
accuracy_df <- bind_rows(lapply(seq_along(accuracy_list), function(i) {
  df <- as.data.frame(t(accuracy_list[[i]]))
  df$iteration <- i
  df
}), .id = "iteration")


# calculate overall accuracy
overall_accuracy <- accuracy_df %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# reshape for plotting
accuracy_long <- pivot_longer(accuracy_df, cols = -iteration, names_to = "question", values_to = "accuracy")

# show individual accuracy per iteration per question
p1 <- ggplot(accuracy_long, aes(x = question, y = accuracy, fill = as.factor(iteration))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = 0.5, hjust = 0.99, angle = 90, size = 4.5) +
  ylim(0, 1) +
  labs(title = "AI Labeling Accuracy for Each Iteration by Question",
       x = "Question",
       y = "Accuracy",
       fill = "Iteration") +
  theme_minimal()
print(p1)

# pivot longer and add missing values
overall_accuracy_long <- overall_accuracy %>%
  pivot_longer(cols = everything(), names_to = "question", values_to = "accuracy") %>%
  left_join(missing_values_count, by = "question")

aggregate_score <- mean(overall_accuracy_long$accuracy, na.rm = TRUE)

# show average accuracy per question + missing values
p2 <- ggplot(overall_accuracy_long, aes(x = question, y = accuracy, fill = question)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 0.1)), vjust = -0.5, size = 5) +
  geom_text(aes(y = accuracy + 0.1, label = paste("Missing:", missing_count)), vjust = -0.5, size = 3, color = "red") +
  ylim(0, 1.2) +
  labs(title = "Average (5 iterations) AI Labeling Accuracy",
       x = "Question",
       y = "Accuracy") +
  theme_minimal() + 
  annotate("text", x = Inf, y = Inf, label = paste("Aggregate Score:", round(aggregate_score, 2)),
           hjust = 1.1, vjust = 2, size = 5, colour = "blue", fontface = "bold")
print(p2)


ggsave(filename = "accuracy_plot_individual.png", plot = p1, width = 10, height = 6)
ggsave(filename = "accuracy_plot_average_new.png", plot = p2, width = 10, height = 6)


p3 <- ggplot(accuracy_long, aes(x = as.factor(iteration), y = accuracy, fill = as.factor(iteration))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(accuracy, accuracy = 0.1)), 
            position = position_dodge(width = 0.9), vjust = 0.45, hjust = 0.99, angle = 90, size = 3.5) +
  facet_wrap(~ question, scales = "free_y") +
  ylim(0, 1) +
  labs(title = "AI Labeling Accuracy for Each Iteration by Question",
       x = "Iteration",
       y = "Accuracy",
       fill = "Iteration") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 12))

print(p3)
ggsave(filename = "accuracy_plot_facet.png", plot = p3, width = 10, height = 6)


# check stats for the other coded variables
# mutate variables to integers
generate_new_variables <- function(df) {
  df %>%
    mutate(
      healthiness_lvl = as.integer(runif(n(), min = 1, max = 10)),
      persuasion_all = as.integer(runif(n(), min = 1, max = 10)),
      persuasion_kids = as.integer(runif(n(), min = 1, max = 10))
    )
}

ai_data <- lapply(ai_data, generate_new_variables)
# combine all data together
combined_ai_data <- bind_rows(lapply(seq_along(ai_data), function(i) {
  ai_data[[i]] %>%
    mutate(iteration = i)
}))

write_xlsx(combined_ai_data, "data/validation sample/validation results/labelling_outputs_old.xlsx")

# healthiness distribution overall
p4 <- ggplot(combined_ai_data, aes(x = healthiness_lvl)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of the Overall Healthiness Level",
       x = "Healthiness Level",
       y = "Count") +
  theme_minimal()

ggsave(filename = "healthiness_distr.png", plot = p4, width = 10, height = 6)

# healthiness distribution by iteration
ggplot(combined_ai_data, aes(x = healthiness_lvl, fill = as.factor(iteration))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Healthiness Level Across Iterations",
       x = "Healthiness Level",
       y = "Count",
       fill = "Iteration") +
  theme_minimal()

# persuasion overall
ggplot(combined_ai_data, aes(x = persuasion_all)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Overall Persuasion",
       x = "Persuasion",
       y = "Count") +
  theme_minimal()

# persuasion by iteration
ggplot(combined_ai_data, aes(x = persuasion_all, fill = as.factor(iteration))) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Overall Persuasion Across Iterations",
       x = "Persuasion",
       y = "Count",
       fill = "Iteration") +
  theme_minimal()

# persuasion kids overall
ggplot(combined_ai_data, aes(x = persuasion_kids)) +
  geom_histogram(binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Persuasion for Kids",
       x = "Persuasion for Kids",
       y = "Count") +
  theme_minimal()

# persuasion all vs. kids
p5 <- ggplot(combined_ai_data, aes(x = persuasion_all, y = persuasion_kids)) +
  geom_jitter(alpha = 0.7, color = "blue", width = 0.2, height = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparison of Persuasion vs. Persuasion for Kids",
       x = "Persuasion",
       y = "Persuasion for Kids") +
  theme_minimal()

ggsave(filename = "pers_comparison.png", plot = p5, width = 10, height = 6)




# check downloaded ad data
ad_data <- read_excel("output/Belgium/ads_data/Belgium_original_data.xlsx")
 
ad_data %>% group_by(page_id, page_name) %>% summarise(row_count = n()) %>% print(n=61)
min(ad_data$ad_delivery_start_time)

ad_data$ad_delivery_start_time <- as.Date(ad_data$ad_delivery_start_time)
ad_data$ad_delivery_stop_time <- as.Date(ad_data$ad_delivery_stop_time)

# filter for ads before November 1st, 2023
filtered_data <- ad_data %>%
  filter(ad_delivery_start_time < as.Date("2023-12-01"))

nrow(filtered_data)

min(na.omit(filtered_data$ad_delivery_stop_time))

write_xlsx(filtered_data, "output/Belgium/ads_data/Belgium_data_Dec1.xlsx")
"""