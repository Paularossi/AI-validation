library(tidyverse)
library(readxl)
library(writexl)


root_folder <- "C:/Users/P70090005/OneDrive - Maastricht University/Desktop/phd/AI-validation/data/"

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



# ========= UPDATED CONSENSUS CALCULATION FOR MULTI-OPTION COLUMNS (from merging_outputs.R)
df <- read_excel(paste(root_folder, "dieticians_all_final.xlsx", sep=""))

split_set <- function(x) {
  # treat "-", "", NA as empty set, otherwise split by comma
  if (is.null(x) || is.na(x)) return(character(0))
  x <- trimws(as.character(x))
  if (x == "" || x == "-" ) return(character(0))
  trimws(unlist(strsplit(x, ",")))
}

collapse_set <- function(x) {
  x <- sort(unique(x))
  if (length(x) == 0) "-" else paste(x, collapse = ", ")
}

cons_union <- function(a, b, c) {
  sets <- list(split_set(a), split_set(b), split_set(c))
  collapse_set(Reduce(union, sets))
}

cons_intersection <- function(a, b, c) {
  sets <- list(split_set(a), split_set(b), split_set(c))
  # intersection over non-empty coders, if all empty -> empty
  if (all(lengths(sets) == 0)) return("-")
  collapse_set(Reduce(intersect, sets))
}

cons_threshold <- function(a, b, c, threshold = 2, fallback = c("union", "empty")) {
  fallback <- match.arg(fallback)
  sets <- list(split_set(a), split_set(b), split_set(c))
  
  all_labels <- unlist(sets, use.names = FALSE)
  if (length(all_labels) == 0) return("-")
  
  tab <- table(all_labels)
  keep <- names(tab)[tab >= threshold]
  
  if (length(keep) > 0) return(collapse_set(keep))
  
  if (fallback == "union") {
    return(collapse_set(Reduce(union, sets)))
  } else {
    return("-")  # explicit empty consensus if no label reaches threshold
  }
}


# ---- create new consensus columns ----
for (v in multi_label_vars) {
  ccols <- paste0(v, "_diet", 1:3)
  stopifnot(all(ccols %in% names(df)))
  
  df[[paste0(v, "_dietcons_thr2_union")]] <- pmap_chr(df[ccols], ~ cons_threshold(..1, ..2, ..3, threshold = 2, fallback = "union")) # original one that we used
  df[[paste0(v, "_dietcons_thr2_empty")]] <- pmap_chr(df[ccols], ~ cons_threshold(..1, ..2, ..3, threshold = 2, fallback = "empty"))
  df[[paste0(v, "_dietcons_union")]]      <- pmap_chr(df[ccols], ~ cons_union(..1, ..2, ..3))
  df[[paste0(v, "_dietcons_intersection")]] <- pmap_chr(df[ccols], ~ cons_intersection(..1, ..2, ..3))
  #df[[paste0(v, "_cons_thr3")]]       <- pmap_chr(df[ccols], ~ cons_threshold(..1, ..2, ..3, threshold = 3, fallback = "empty"))
}

# remove old consensus columns
old_cons_cols <- c("prem_offer_dietcons", "marketing_str_dietcons", "who_cat_dietcons", "who_cat_clean_dietcons")
old_cons_cols <- old_cons_cols[old_cons_cols %in% names(df)]
df <- df %>% select(-all_of(old_cons_cols))

# ---- quantify how often the original fallback would trigger (threshold=2, fallback=union) ----
fallback_summary <- map_dfr(multi_label_vars, function(v) {
  ccols <- paste0(v, "_diet", 1:3)
  stopifnot(all(ccols %in% names(df)))
  
  # fallback is used when threshold set is empty BUT union is non-empty
  used_fallback <- pmap_lgl(df[ccols], function(...) {
    vals <- list(...)
    sets <- lapply(vals, split_set)
    
    all_labels <- unlist(sets, use.names = FALSE)
    if (length(all_labels) == 0) return(FALSE)
    
    tab <- table(all_labels)
    thr <- names(tab)[tab >= 2]
    
    (length(thr) == 0) && (length(Reduce(union, sets)) > 0)
  })
  
  tibble(
    question = v,
    n = length(used_fallback),
    n_fallback = sum(used_fallback),
    pct_fallback = mean(used_fallback)
  )
})


write_xlsx(
  list(
    responses_diet_final_sens = df,
    fallback_rates = fallback_summary
  ), file.path(root_folder, "dieticians_all_sensitivity.xlsx")
)






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
