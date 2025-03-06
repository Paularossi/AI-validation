library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(irr)
library(readr)
library(caret)
library(stringr)

root_folder <- "//unimaas.nl/users/Employees/P70090005/data/Desktop/phd/ad libraries and brands/AI-validation/"

# Belgium_ads_subset is the main file with all the ads data
ads_data <- read_excel(paste(root_folder, "data/Mturk data.xlsx", sep=""))

# the images are in unique_images
mturk <- read.csv(paste(root_folder, "data/MTurk_urls no text.csv", sep = ""))



# to create the csv for qualtrics
# to do - add the rest of the images to the qualtrics library and do this again
data <- read.csv(paste(root_folder, "data/MTurk_urls no text.csv", sep=""))
data <- data %>%
  mutate(new_url = paste("https://maastrichtuniversity.eu.qualtrics.com/ControlPanel/Graphic.php?IM=", url, sep=""))

write.csv(temp, paste(root_folder,"data/MTurk_urls no text.csv", sep=""), row.names = FALSE)


temp <- dig_coding %>%
  select(img_id, ad_creative_bodies, page_name) %>%
  mutate(img_id = paste(img_id, ".png", sep="")) %>%
  right_join(data, by = "img_id") %>%
  mutate(ad_bodies_clean = gsub("^\\[|\\]$", "", ad_creative_bodies))


# Metadata from the ads subset
ads_subset <- read_excel(paste(root_folder, "data/Belgium_ads_subset.xlsx", sep=""))
ads_subset[c(1,9:14)] <- lapply(ads_subset[c(1,9:14)], as.factor)
ads_subset[c(2,17)] <- lapply(ads_subset[c(2,17)], as.Date)

# flatten the target_ages column
ads_subset <- ads_subset %>%
  mutate(target_ages = str_remove_all(target_ages, "[\\[\\]']")) %>%
  separate(target_ages, into = c("target_age_min", "target_age_max"), sep = ",\\s*") %>%
  mutate(target_age_min = as.numeric(target_age_min),
         target_age_max = as.numeric(target_age_max))

# 80 page_ids but 79 page_names
ads_subset %>% group_by(page_id) %>% count(page_name)
sum(is.na(ads_subset$ad_creative_bodies)) # 55 ads missing a caption

ads_subset %>% group_by(target_age_max) %>% count() 
ads_subset %>% summarise(
    mean_min = mean(target_age_min, na.rm = TRUE),
    mean_max = mean(target_age_max, na.rm = TRUE),
    median_min = median(target_age_min, na.rm = TRUE),
    median_max = median(target_age_max, na.rm = TRUE),
    overall_range = max(target_age_max, na.rm = TRUE) - min(target_age_min, na.rm = TRUE)
  )

range(ads_subset$ad_delivery_stop_time, na.rm = T)

###### START ANALYSIS HERE
#original_labeling <- read_excel(paste(root_folder, "validation results/original_coding.xlsx", sep=""))
dig_coding <- read_excel(paste(root_folder, "validation results/digital_coding_clean.xlsx", sep=""))

### LOAD THE AI-LABELLED DATA
ai_labeling <- read_excel(paste(root_folder, "validation results/labeling_outputs.xlsx", sep=""))
dig_coding <- dig_coding[dig_coding$img_id %in% ai_labeling$img_id, ]
#write_xlsx(dig_coding, paste(root_folder, "validation results/digital_coding_500.xlsx", sep=""))

ai_labeling <- ai_labeling %>% arrange(img_id)
dig_coding <- dig_coding %>% arrange(img_id)

# first check the confusion matrix and kappa for the categories with one option

##################################################
# AD TYPE
table(dig_coding$type_ad, ai_labeling$type_ad)

type_ad_test <- kappa2(cbind(dig_coding$type_ad, ai_labeling$type_ad))
cat("***TYPE AD*** Kappa Value:", type_ad_test$value, "; p-value:", type_ad_test$p.value, " \n")

# accuracy
sum(dig_coding$type_ad == ai_labeling$type_ad) / nrow(dig_coding)

# find the ads with misclassifications
misclass_ad_type <- dig_coding$type_ad != ai_labeling$type_ad
discrepancies_ad_type <- data.frame(
  img_id = dig_coding$img_id[misclass_ad_type],
  actual = dig_coding$type_ad[misclass_ad_type],
  predicted = ai_labeling$type_ad[misclass_ad_type],
  ad_text = dig_coding$ad_creative_bodies[misclass_ad_type],
  page_name = dig_coding$page_name[misclass_ad_type],
  ai_reason = ai_labeling$type_ad_expl[misclass_ad_type]
)

misclass_ad_type_ads <- discrepancies_ad_type %>%
  group_by(predicted, actual) %>%
  summarise(n = n(), ads = paste(img_id, collapse = ", ")) %>%
  #arrange(desc(n), .by_group = TRUE)
  arrange(desc(n)) %>% head(5)
  
# look at the ad ids
print(paste(misclass_ad_type_ads$ads[1]))
print(paste(misclass_ad_type_ads$ads[2]))
print(paste(misclass_ad_type_ads$ads[3]))


# save the misclassifications in an excel file
pred_1_act_6 <- discrepancies_ad_type %>% 
  filter(actual == "6" & predicted == "1")
write_xlsx(pred_1_act_6, paste(root_folder, "validation results/miscl/ad type/predicted 1 actual 6/pred_1_act_6_adtype.xlsx", sep=""))


##################################################
# PROCESSING LEVEL
table(dig_coding$processed, ai_labeling$processed)

processed_test <- kappa2(cbind(dig_coding$processed, ai_labeling$processed))
cat("***PROCESSED*** Kappa Value:", processed_test$value, "; p-value:", processed_test$p.value, " \n")

sum(dig_coding$processed == ai_labeling$processed) / nrow(dig_coding)

# find the ads with misclassifications
misclass_processing <- dig_coding$processed != ai_labeling$processed
discrepancies_processing <- data.frame(
  img_id = dig_coding$img_id[misclass_processing],
  actual = dig_coding$processed[misclass_processing],
  predicted = ai_labeling$processed[misclass_processing],
  ad_text = dig_coding$ad_creative_bodies[misclass_processing],
  page_name = dig_coding$page_name[misclass_processing],
  ai_reason = ai_labeling$type_ad_expl[misclass_processing]
)

misclass_processing_ads <- discrepancies_processing %>%
  group_by(predicted, actual) %>%
  summarise(n = n(), ads = paste(img_id, collapse = ", ")) %>%
  #arrange(desc(n), .by_group = TRUE)
  arrange(desc(n)) %>% head(5)


# save the misclassifications in an excel file
pred_1_act_NA <- discrepancies_processing %>% 
  filter(actual == "NA" & predicted == "1")
write_xlsx(pred_1_act_NA, paste(root_folder, "validation results/miscl/upf/predicted 1 actual NA/pred_1_act_NA_upf.xlsx", sep=""))






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
calculate_kappa <- function(merged_data, all_cats, column_prefix) {
  kappa_results <- lapply(all_cats, function(cat) {
    dig_col <- paste0(column_prefix, "_", cat, "_dig")
    ai_col  <- paste0(column_prefix, "_", cat, "_ai")
    
    kappa_test <- kappa2(cbind(merged_data[[dig_col]], merged_data[[ai_col]]))
    
    kappa_value <- kappa_test$value
    p_value <- kappa_test$p.value
    
    # Return a data frame with the category, Kappa value, and p-value
    data.frame(category = cat, kappa = kappa_value, p_value = p_value)
  })
  
  kappa_df <- do.call(rbind, kappa_results)
  
  return(kappa_df)
}
# network model for agreement (check the AI polling paper)



############## PREMIUM OFFERS
table(dig_coding$prem_offer)
table(ai_labeling$prem_offer)

# first get the unique premium offers from both dig_coding and ai_labeling
all_offers <- unique(c(
  unlist(strsplit(dig_coding$prem_offer, ", ")),
  unlist(strsplit(ai_labeling$prem_offer, ", "))
))

all_offers_text <- c("0" = "None", "1" = "App downloads", "2" = "Contests", "3" = "Pay 2 take 3", "4" = "20% extra",
                      "5" = "Limited edition", "6" = "Social charity", "7" = "Gifts", "8" = "Price discount", "9" = "Loyalty programs")

# merge the AI and manual coding dummy vars
dig_coding_prem_offers <- create_dummy_vars(dig_coding, all_offers, "prem_offer")
ai_labeling_prem_offers <- create_dummy_vars(ai_labeling, all_offers, "prem_offer")
merged_prem_offer <- full_join(dig_coding_prem_offers, ai_labeling_prem_offers, by = "img_id", suffix = c("_dig", "_ai"))

kappa_prem_offer <- calculate_kappa(merged_prem_offer, all_offers, "prem_offer")

# visualize the kappas per category and their p-values
# all_offers_text[category]
ggplot(kappa_prem_offer, aes(x = all_offers_text[category], y = kappa)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(p_value, 3)), vjust = -0.5, ) +
  theme_minimal() +
  labs(title = "Cohen's Kappa for Premium Offers",
       x = "Category", y = "Kappa") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("premium_offers_kappa.png", path = root_folder)
# put proper category names

# # compute a weighted average kappa for premium offers
# num_ads_prem_offer <- colSums(merged_prem_offer[, grepl("prem_offer_", names(merged_prem_offer))])
# 
# num_ads_prem_offer <- data.frame(
#   category = as.numeric(gsub("prem_offer_|_dig", "", names(num_ads_prem_offer))),
#   num_ads = as.vector(num_ads_prem_offer)
# )
# kappa_merged <- merge(kappa_prem_offer, num_ads_prem_offer, by = "category")
# weighted_avg_kappa <- weighted.mean(kappa_merged$kappa, kappa_merged$num_ads)
# print(weighted_avg_kappa)


# CHECK FOR DISCREPANCIES BETWEEN AI AND MANUAL CODING
# for each prem_offer category, create a discrepancy column
for (offer in all_offers) {
  dig_col <- paste0("prem_offer_", offer, "_dig")
  ai_col  <- paste0("prem_offer_", offer, "_ai")
  discrepancy_col <- paste0("discrepancy_prem_offer_", offer)
  
  # create a column indicating where there's a discrepancy (TRUE for disagreement)
  merged_prem_offer[[discrepancy_col]] <- merged_prem_offer[[dig_col]] != merged_prem_offer[[ai_col]]
}

discrep_prem <- merged_prem_offer %>%
  summarise(across(starts_with("discrepancy_prem_offer_"), sum)) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "discrepancy_count") %>%
  arrange(category) %>%
  mutate(offer_nr = as.character(0:9))
# discrepancies show ANY disagreement (for ex. offer was 9 but AI picked 5, 9)
ggplot(discrep_prem, aes(x = all_offers_text[offer_nr], y = discrepancy_count)) +
  geom_col(fill = "orchid") +
  theme_minimal() +
  labs(title = "Number of disagreements per Premium Offer category",
       x = "Premium Offer Category",
       y = "Number of discrepancies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("prem_offer_discr.png", path = root_folder, width = 12)

# see exactly where the discrepancies happen
misclass_prem <- list()

# loop through each offer in dig_coding
for (offer in all_offers) {
  # identify rows where the manual coding has '1' for the current offer
  dig_col <- paste0("prem_offer_", offer, "_dig")
  ai_cols <- grep("^prem_offer_.*_ai$", names(merged_prem_offer), value = TRUE)
  
  # filter rows where manual coding says offer is present (1)
  relevant_rows <- merged_prem_offer %>% filter(!!sym(dig_col) == 1)
  
  # count how often the AI predicted other offers
  ai_misclassifications <- relevant_rows %>%
    select(img_id, all_of(ai_cols)) %>%
    pivot_longer(cols = starts_with("prem_offer_"), names_to = "ai_offer", values_to = "ai_prediction") %>%
    filter(ai_prediction == 1 & ai_offer != paste0("prem_offer_", offer, "_ai")) %>%  # remove correct predictions
    group_by(ai_offer) %>%
    summarise(n = n(), ads = paste(img_id, collapse = ", ")) %>%
    arrange(desc(n)) %>% head(5) # take only top 5 misclassifications
  
  misclass_prem[[paste0("offer_", offer)]] <- ai_misclassifications
}

# print or plot the misclassifications for a specific offer
print(misclass_prem$offer_0)

# get 5 most frequent misclassifications
big_misclass_prem <- do.call(rbind, lapply(names(misclass_prem), function(name) {
  df <- misclass_prem[[name]]
  df$source_df <- name  
  return(df)
}))

top_5_misclass_prem <- big_misclass_prem %>% arrange(desc(n)) %>% head(5)


pred_2_act_0 <- merged_prem_offer %>% 
  filter(prem_offer_0_dig == 1 & prem_offer_2_ai == 1)
write_xlsx(pred_2_act_0, paste(root_folder, "validation results/miscl/premium/predicted 2 actual 0/pred_2_act_0_prem.xlsx", sep=""))



############## MARKETING STRATEGIES
# get the unique marketing strategies
all_marketing_str <- unique(c(
  unlist(strsplit(dig_coding$marketing_str, ", ")),
  unlist(strsplit(ai_labeling$marketing_str, ", "))
))

all_marketing_str_text <- c("0" = "None", "1" = "Cartoons", "2" = "Licensed char", "3" = "Amateur sports", "4" = "Celebrity",
                     "5" = "Movie char", "6" = "Famous sports", "7" = "Events", "8" = "Age-targeted (kids)", 
                     "9" = "Awards", "10" = "Sport events")

dig_coding_marketing_str <- create_dummy_vars(dig_coding, all_marketing_str, "marketing_str")
ai_labeling_marketing_str <- create_dummy_vars(ai_labeling, all_marketing_str, "marketing_str")
merged_marketing_str <- full_join(dig_coding_marketing_str, ai_labeling_marketing_str, by = "img_id", suffix = c("_dig", "_ai"))

kappa_marketing_str <- calculate_kappa(merged_marketing_str, all_marketing_str, "marketing_str")

ggplot(kappa_marketing_str, aes(x = all_marketing_str_text[category], y = kappa)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(p_value, 3)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Cohen's Kappa for Marketing Strategies",
       x = "Category", y = "Kappa") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("marketing_str_kappa.png", path = root_folder)

# for marketing strategies
# merged_marketing_str <- merged_marketing_str %>% select(-marketing_str_expl)
# num_ads_marketing_str <- colSums(merged_marketing_str[, grepl("marketing_str_", names(merged_marketing_str))])
# 
# num_ads_marketing_str <- data.frame(
#   category = as.numeric(gsub("marketing_str_|_dig", "", names(num_ads_marketing_str))),
#   num_ads = as.vector(num_ads_marketing_str)
# )
# kappa_merged <- merge(kappa_marketing_str, num_ads_marketing_str, by = "category")
# weighted_avg_kappa <- weighted.mean(kappa_merged$kappa, kappa_merged$num_ads)
# print(weighted_avg_kappa)

for (strategy in all_marketing_str) {
  dig_col <- paste0("marketing_str_", strategy, "_dig")
  ai_col  <- paste0("marketing_str_", strategy, "_ai")
  discrepancy_col <- paste0("discrepancy_marketing_str_", strategy)
  
  # create a column indicating where there's a discrepancy (TRUE for disagreement)
  merged_marketing_str[[discrepancy_col]] <- merged_marketing_str[[dig_col]] != merged_marketing_str[[ai_col]]
}

discrep_marketing <- merged_marketing_str %>%
  summarise(across(starts_with("discrepancy_marketing_str"), sum)) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "discrepancy_count") %>%
  arrange(category) %>%
  mutate(str_nr = c(as.character(0:1), "10", as.character(2:9)))
# multiple misclassified labels for one ad will count ONCE

ggplot(discrep_marketing, aes(x = all_marketing_str_text[str_nr], y = discrepancy_count)) +
  geom_col(fill = "orchid") +
  theme_minimal() +
  labs(title = "Number of disagreements per Marketing strategy category",
       x = "Marketing strategy Category",
       y = "Number of discrepancies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("marketing_str_discr.png", path = root_folder, width = 12)


# misclassified labels
misclass_marketing <- list()
for (strategy in all_marketing_str) {
  dig_col <- paste0("marketing_str_", strategy, "_dig")
  ai_cols <- grep("^marketing_str_.*_ai$", names(merged_marketing_str), value = TRUE)
  
  relevant_rows <- merged_marketing_str %>% filter(!!sym(dig_col) == 1)
  
  ai_misclassifications <- relevant_rows %>%
    select(img_id, all_of(ai_cols)) %>%
    pivot_longer(cols = starts_with("marketing_str"), names_to = "ai_offer", values_to = "ai_prediction") %>%
    filter(ai_prediction == 1 & ai_offer != paste0("marketing_str_", strategy, "_ai")) %>%  # remove correct predictions
    group_by(ai_offer) %>%
    summarise(n = n(), ads = paste(img_id, collapse = ", ")) %>%
    arrange(desc(n))
  
  misclass_marketing[[paste0("strategy_", strategy)]] <- ai_misclassifications
}

# print or plot the misclassifications for a specific offer
print(misclass_marketing$strategy_0)

# get 5 most frequent misclassificaitons
big_misclass_marketing <- do.call(rbind, lapply(names(misclass_marketing), function(name) {
  df <- misclass_marketing[[name]]
  df$source_df <- name
  return(df)
}))

top_5_misclass_marketing <- big_misclass_marketing %>% 
  arrange(desc(n)) %>% head(5)

pred_8_act_0 <- merged_marketing_str %>% 
  filter(marketing_str_0_dig == 1 & marketing_str_8_ai == 1)
write_xlsx(pred_8_act_0, paste(root_folder, "validation results/miscl/marketing/predicted 8 actual 0/pred_8_act_0_markt.xlsx", sep=""))

# add the Pearson correlation + brior score





############## WHO CATEGORIES
# get the unique who categories
all_who_cats <- unique(c(
  unlist(strsplit(dig_coding$who_cat, ", ")),
  unlist(strsplit(ai_labeling$who_cat, ", "))
))

all_who_cats_text <- c(
  "1" = "Confectionery", "2" = "Bakery goods", "3" = "Snacks", "4" = "Beverages",
  "4a" = "Juices", "4b" = "Milk drinks", "4c" = "Energy drinks", "4d" = "Other beverages",
  "4e" = "Water/tea/coffee", "5" = "Ices", "6" = "Cereals", "7" = "Dairy foods",
  "8" = "Cheese", "9" = "Ready-made foods", "10" = "Fats/oils", "11" = "Bread",
  "12" = "Grains/pasta", "13" = "Fresh meat/fish", "14" = "Processed meat", 
  "15" = "Fresh produce", "16" = "Processed produce", "17" = "Plant-based foods",
  "18" = "Sauces/dressings", "A" = "Alcohol", "S" = "Supplements", 
  "B" = "Baby food", "I" = "Infant formulas", "NA" = "Non-food brand"
)

dig_coding_who_cats <- create_dummy_vars(dig_coding, all_who_cats, "who_cat")
ai_labeling_who_cats <- create_dummy_vars(ai_labeling, all_who_cats, "who_cat")
merged_who_cat <- full_join(dig_coding_who_cats, ai_labeling_who_cats, by = "img_id", suffix = c("_dig", "_ai"))

kappa_who_cat <- calculate_kappa(merged_who_cat, all_who_cats, "who_cat")

ggplot(kappa_who_cat, aes(x = all_who_cats_text[category], y = kappa)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(p_value, 2)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Cohen's Kappa for WHO Categories",
       x = "Category", y = "Kappa") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("who_cat_kappa.png", path = root_folder, width = 12)


for (who_cat in all_who_cats) {
  dig_col <- paste0("who_cat_", who_cat, "_dig")
  ai_col  <- paste0("who_cat_", who_cat, "_ai")
  discrepancy_col <- paste0("discrepancy_who_cat_", who_cat)
  
  # create a column indicating where there's a discrepancy (TRUE for disagreement)
  merged_who_cat[[discrepancy_col]] <- merged_who_cat[[dig_col]] != merged_who_cat[[ai_col]]
}

discrep_who_cat <- merged_who_cat %>%
  summarise(across(starts_with("discrepancy_who_cat_"), sum)) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "discrepancy_count") %>%
  arrange(category) %>%
  mutate(cat_nr = str_extract(category, "(\\d+[a-z]*|[A-Z]+)"))
  
# discrepancies show ANY disagreement (for ex. offer was 9 but AI picked 5, 9)
ggplot(discrep_who_cat, aes(x = all_who_cats_text[cat_nr], y = discrepancy_count)) +
  geom_col(fill = "orchid") +
  theme_minimal() +
  labs(title = "Number of disagreements per WHO category",
       x = "WHO Category",
       y = "Number of discrepancies") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#ggsave("who_cat_discr.png", path = root_folder, width = 12)

# see exactly where the discrepancies happen
misclass_who_cat <- list()

# loop through each offer in dig_coding
for (who_cat in all_who_cats) {
  # identify rows where the manual coding has '1' for the current offer
  dig_col <- paste0("who_cat_", who_cat, "_dig")
  ai_cols <- grep("^who_cat_.*_ai$", names(merged_who_cat), value = TRUE)
  
  # filter rows where manual coding says offer is present (1)
  relevant_rows <- merged_who_cat %>% filter(!!sym(dig_col) == 1)
  
  # count how often the AI predicted other offers
  ai_misclassifications <- relevant_rows %>%
    select(img_id, all_of(ai_cols)) %>%
    pivot_longer(cols = starts_with("who_cat_"), names_to = "ai_who_cat", values_to = "ai_prediction") %>%
    filter(ai_prediction == 1 & ai_who_cat != paste0("who_cat_", who_cat, "_ai")) %>%  # remove correct predictions
    group_by(ai_who_cat) %>%
    summarise(n = n(), ads = paste(img_id, collapse = ", ")) %>%
    arrange(desc(n)) %>% head(5) # take only top 5 misclassifications
  
  misclass_who_cat[[paste0("cat_", who_cat)]] <- ai_misclassifications
}

# print or plot the misclassifications for a specific offer
print(misclass_who_cat$cat_1)

# get 5 most frequent misclassifications
big_misclass_who_cat <- do.call(rbind, lapply(names(misclass_who_cat), function(name) {
  df <- misclass_who_cat[[name]]
  df$source_df <- name 
  df <- df 
  return(df)
})) %>% arrange(desc(n))

top_5_misclass_who_cat <- big_misclass_who_cat %>% head(5)


# do this !!!
pred_2_act_0 <- merged_prem_offer %>% 
  filter(prem_offer_0_dig == 1 & prem_offer_2_ai == 1)
write_xlsx(pred_2_act_0, paste(root_folder, "validation results/miscl/premium/predicted 2 actual 0/pred_2_act_0_prem.xlsx", sep=""))











### LOTS OF DATA PROCESSING
### FIRST, FOR THE ORIGINAL CODING
# coding <- read.csv(paste(root_folder, "data/Voedingsadvertenties_finaal.csv", sep=""), sep = ";")
# coding <- coding[ , -c(1, 3:22)] # remove the columns we don't need
# coding <- as_tibble(coding)
# head(coding, 2)
# names(coding)
# 
# # first rename the columns
# new_names <- c("img_id", "type_ad", "public_transport", "marketing_str", "marketing_str_descr", "prem_offer", "premium_descr",
#                 "nr_food", "food_name", "brand_name", "who_cat", "processed", "healthy_living", "health_recomm", "comments")
# names(coding) <- new_names
# head(coding, 2)
# 
# # then adjust the img ids
# coding <- coding %>%
#     mutate(img_id = gsub(".jpg", "", img_id))
# 
# # select the relevant columns
# original_labeling <- coding %>% 
#     select(img_id, type_ad, marketing_str, prem_offer, who_cat, processed, healthy_living) %>%
#     mutate(across(everything(), as.character))
# head(original_labeling)
# 
# cleaned_labeling <- original_labeling %>%
#   # first remove all rows with more than 2 NAs
#   filter(rowSums(is.na(across(everything()))) <= 2) %>%
#   group_by(img_id) %>%
#   filter(if (n() > 1) {
#     if (n_distinct(across(everything())) == 1) {
#       row_number() == 1 # if all rows are the same, keep only one row
#     } else {
#       na_counts <- rowSums(is.na(across(everything())))
#       # check if NA counts are identical for all rows and if rows are different
#       if (n_distinct(na_counts) == 1) {
#         # if all rows have the same number of NAs and are different, remove them
#         FALSE
#       } else {
#         # otherwise, keep the row with the fewest NAs
#         row_number() == which.min(na_counts)
#       }
#     }
#   } else {
#     TRUE # if there is only one row, keep it
#   }) %>%
#   ungroup()
# 
# # standardize empty strings to NA
# standardize_missing <- function(column) {
#   column <- ifelse(is.na(column) | column == "", "0", column)
#   return(column)
# }
# 
# cleaned_labeling <- cleaned_labeling %>%
#   mutate(across(c(marketing_str, prem_offer), standardize_missing))
# 
# # save the cleaned data
# write_xlsx(cleaned_labeling, paste(root_folder, "validation results/original_coding.xlsx", sep=""))

# DATA PROCESSING FOR DIGITAL ADS
# dig_coding <- read_excel(paste(root_folder, "data/digital_coding.xlsx", sep=""))
# dig_coding <- dig_coding[ , -c(2,3,5:9,12,14,15,17,19:21)] # remove the columns we don't need
# 
# # rename the columns
# new_names <- c("img_id", "type_ad", "who_cat", "processed", "marketing_str", "prem_offer", "ad_creative_bodies")
# names(dig_coding) <- new_names
# 
# 
# dig_coding <- dig_coding %>%
#   mutate(across(everything(), as.character))
# 
# 
# standardize_market_prom <- function(column) {
#   column <- ifelse(is.na(column) | column == "", "0", column)
#   return(column)
# }
# 
# standardize_processed_who <- function(column) {
#   column <- ifelse(is.na(column) | column == "", "NA", column)
#   return(column)
# }
# 
# dig_coding <- dig_coding %>%
#   mutate(across(c(marketing_str, prem_offer), standardize_market_prom)) %>%
#   mutate(across(c(processed, who_cat), standardize_processed_who))
# 
# 
# # add the brand name
# orig_data <- read_excel(paste(root_folder, "data/Belgium_ads_subset.xlsx", sep=""))
# orig_data <- orig_data %>%
#   select(id, page_name, ad_creative_bodies)
# 
# dig_coding <- dig_coding %>%
#   mutate(clean_ad_id = sub("_img$", "", img_id))
# 
# dig_coding <- dig_coding %>%
#   mutate(clean_ad_id = sub("(.*_[0-9]+)_[0-9]+$", "\\1", clean_ad_id))
# 
# dig_coding <- dig_coding %>%
#   mutate(clean_ad_id = paste0(clean_ad_id, "_img"))
# 
# merged_df <- dig_coding %>%
#   left_join(orig_data, by = c("clean_ad_id" = "id")) %>%
#   select(-c(clean_ad_id, ad_creative_bodies.y))
# 
# write_xlsx(merged_df, paste(root_folder, "validation results/digital_coding_clean.xlsx", sep=""))
