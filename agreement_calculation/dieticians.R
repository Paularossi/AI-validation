library(tidyverse)
library(readxl)
library(writexl)


root_folder <- "C:/Users/P70090005/Desktop/phd/AI-validation/data/dieticians/"

diet1 <- read_excel(paste(root_folder, "dietician_1_dutch.xlsx", sep=""))
diet23 <- read_excel(paste(root_folder, "dietician_2_3_french.xlsx", sep=""))
ads <- read_excel(paste(root_folder, "dietician_400_ads.xlsx", sep="")) %>%
  arrange(img_id)

names <- names(diet1)
tail(names)


df_long <- diet23[-1, -c(1, 2, 3, 13204)] %>% 
  select(-contains("processed"))%>%
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "value"
  ) %>%
  extract(name, into = c("ad_id", "question"), regex = "^(\\d+)_(.+)$") %>%
  mutate(ad_id = as.integer(ad_id))

# add dietician based on order (only for french)
df_long <- df_long %>%
  group_by(ad_id, question) %>%
  mutate(coder = row_number()) %>%
  ungroup()
#%>% mutate(colname = paste0(question, "_diet", coder_index))


df_wide <- df_long %>%
  #select(ad_id, colname, value)%>%
  pivot_wider(
    names_from = question,
    values_from = value
  ) %>%
  arrange(ad_id)

# for the dutch dietician
df_long2 <- diet1[-1, -c(1, 2, 3, 13204)] %>% 
  select(-contains("processed"))%>%
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "value"
  ) %>%
  extract(name, into = c("ad_id", "question"), regex = "^(\\d+)_(.+)$") %>%
  mutate(ad_id = as.integer(ad_id),
         coder = 3)

df_wide2 <- df_long2 %>%
  pivot_wider(
    names_from = question,
    values_from = value
  ) %>%
  arrange(ad_id)


# process the data
# create new ad_types and who_cats based on whether there is alcohol
alcohol_changes <- function(df) {
  # adjust ad_type: overwrite with "9" if alcohol == 1
  df[["new_type_ad"]] <- ifelse(df[["alcohol"]] == 1, "9", df[["ad_type"]])
  
  # adjust who_cat: append "A" or replace with "A" if NA/empty
  df[["new_who_cat"]] <- ifelse(
    df[["alcohol"]] == 1,
    ifelse(is.na(df[["who_cat"]]) | df[["who_cat"]] == "",
           "A",
           paste("A", df[["who_cat"]], sep = ", ")),
    df[["who_cat"]]
  )
  
  return(df)
}

df_wide <- alcohol_changes(df_wide)
df_wide2 <- alcohol_changes(df_wide2)

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

df_wide <- df_wide %>%
  mutate(who_cat_clean = sapply(new_who_cat, clean_who_cat))

df_wide2 <- df_wide2 %>%
  mutate(who_cat_clean = sapply(new_who_cat, clean_who_cat))


# combine the two dataframes
responses_long <- rbind(df_wide, df_wide2) %>%
  mutate(diet_number = paste("diet", coder, sep=""))

responses_final <- responses_long %>%
  pivot_wider(
    id_cols = ad_id,
    names_from = diet_number,
    values_from = c(ad_type, new_type_ad, target_group, alcohol, non_alcohol, prem_offer, marketing_str, who_cat, who_cat_clean,
                    marketing_str_11_TEXT, prem_offer_10_TEXT)
  ) %>% 
  arrange(ad_id)

df_final <- cbind(img_id = ads$img_id, responses_final)

write_xlsx(df_final, paste(root_folder, "dieticians_all_wide.xlsx", sep=""))



# build the consensus columns for single choice questions
#responses_wide_all <- read_excel("C:/Users/P70090005/Desktop/phd/AI-validation/data/dieticians/dieticians_all_wide.xlsx")
responses_wide_all <- read_excel(paste(root_folder, "dieticians_all_wide.xlsx", sep=""))

responses_dieticians <- responses_wide_all %>%
  select(img_id)

single_choice_vars <- c("ad_type", "new_type_ad", "target_group", "alcohol", "non_alcohol")

for (var in single_choice_vars) {
  
  diet_cols <- paste0(var, "_diet", 1:3)
  
  # add only 3 coder columns
  responses_dieticians[diet_cols] <- responses_wide_all[diet_cols]
  
  # compute consensus via majority vote (most frequent value)
  consensus_col <- paste0(var, "_dietcons")
  responses_dieticians[[consensus_col]] <- responses_wide_all[diet_cols] %>%
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

multi_label_consensus_majority <- function(..., return_string = TRUE, threshold = 2, brand = TRUE) {
  labels <- list(...)
  
  sets <- lapply(labels, function(x) {
    # if (is.null(x) || is.na(x) || trimws(x) == "-" || trimws(x) == "") {
    #   return(character(0))
    # }
    trimws(strsplit(as.character(x), ",")[[1]])
  })
  
  sets <- sets[lengths(sets) > 0]
  if (length(sets) == 0) return(if (return_string) "-" else character(0))
  
  # flatten all labels into one vector
  all_labels <- unlist(sets, use.names = FALSE)
  if (length(all_labels) == 0) return(if (return_string) "-" else character(0))
  
  # count frequency across coders
  label_counts <- table(all_labels)
  consensus <- names(label_counts[label_counts >= threshold])
  
  if (length(consensus) == 0) {
    union_set <- Reduce(union, sets)
    return(if (return_string) paste(sort(union_set), collapse = ",") else union_set)
  }
  #if (length(consensus) == 0) return(if (brand) "-" else names(label_counts)[which.max(label_counts)])
  
  consensus <- sort(consensus)
  if (return_string) {
    paste(consensus, collapse = ", ")
  } else {
    consensus
  }
}

responses_multi_clean <- responses_wide_all %>%
  select(img_id)

# the new multi-label consensus still doesn't work.... 
for (var in multi_label_vars) {
  
  diet_cols <- paste0(var, "_diet", 1:3)
  responses_multi_clean[diet_cols] <- responses_wide_all[diet_cols]
  
  consensus_col <- paste0(var, "_dietcons")
  
  responses_multi_clean[[consensus_col]] <-
    pmap_chr(.l = responses_wide_all[diet_cols],
             ~ multi_label_consensus_majority(..1, ..2, ..3, threshold = 2))
}

responses_dieticians_all <- left_join(responses_dieticians, responses_multi_clean, by = "img_id")

responses_dieticians_all <- responses_dieticians_all %>%
  left_join(select(responses_wide_all, matches("_TEXT_diet[1-3]$"), "img_id"),
            by = "img_id")

# responses_dieticians_all <- responses_wide_all %>% 
#   select(img_id, brand_diet1, brand_diet2, brand_diet3) %>%
#   left_join(responses_dieticians_all, by = "img_id")

# THIS IS THE MAIN DATAFRAME TO BASE THE ANALYSIS ON!!!
write_xlsx(responses_dieticians_all, paste(root_folder, "dieticians_all_final.xlsx", sep=""))




# ======= OUTDOOR ADS (100) =======
root_folder2 <- "C:/Users/P70090005/Desktop/phd/AI-validation/data/outdoor ads/"

diet1 <- read_excel(paste(root_folder2, "dieticians/dietician_1_outdoor_dutch.xlsx", sep=""))
diet23 <- read_excel(paste(root_folder2, "dieticians/dietician_23_outdoor_french.xlsx", sep=""))
ads <- read_delim(paste(root_folder2, "dietician_100_outdoor_ads_image_ids.csv", sep=""), col_types = list(col_character())) %>%
  arrange(img_id)

ads$img_id_clean <- gsub("\\.jpg$", "", ads$img_id)


df_long <- diet23[-1, -c(1, 2, 3, 3404)] %>% 
  select(-contains("processed"))%>%
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "value"
  ) %>%
  extract(name, into = c("ad_id", "question"), regex = "^(\\d+)_(.+)$") %>%
  mutate(ad_id = as.integer(ad_id))

# add dietician based on order (only for french)
df_long <- df_long %>%
  group_by(ad_id, question) %>%
  mutate(coder = row_number()) %>%
  ungroup()

df_wide <- df_long %>%
  pivot_wider(
    names_from = question,
    values_from = value
  ) %>%
  arrange(ad_id)

# for the dutch dietician
df_long2 <- diet1[-1, -c(1, 2, 3, 3404)] %>% 
  select(-contains("processed"))%>%
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "value"
  ) %>%
  extract(name, into = c("ad_id", "question"), regex = "^(\\d+)_(.+)$") %>%
  mutate(ad_id = as.integer(ad_id),
         coder = 3)

df_wide2 <- df_long2 %>%
  pivot_wider(
    names_from = question,
    values_from = value
  ) %>%
  arrange(ad_id)


df_wide <- alcohol_changes(df_wide)
df_wide2 <- alcohol_changes(df_wide2)

df_wide <- df_wide %>%
  mutate(who_cat_clean = sapply(new_who_cat, clean_who_cat))

df_wide2 <- df_wide2 %>%
  mutate(who_cat_clean = sapply(new_who_cat, clean_who_cat))


# combine the two dataframes
responses_long <- rbind(df_wide, df_wide2) %>%
  mutate(diet_number = paste("diet", coder, sep=""))

responses_final <- responses_long %>%
  pivot_wider(
    id_cols = ad_id,
    names_from = diet_number,
    values_from = c(brand, ad_type, new_type_ad, target_group, alcohol, non_alcohol, prem_offer, marketing_str, who_cat, who_cat_clean)
  ) %>% 
  arrange(ad_id)

df_final <- cbind(img_id = ads$img_id, responses_final)

write_xlsx(df_final, paste(root_folder2, "dieticians_outdoor_all_wide.xlsx", sep=""))










