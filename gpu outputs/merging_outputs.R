
library(readxl)
library(dplyr)
library(writexl)
library(purrr)

root_folder <- "C:/Users/P70090005/Documents/AI-validation/gpu outputs/"

###### START ANALYSIS HERE
#original_labeling <- read_excel(paste(root_folder, "validation results/original_coding.xlsx", sep=""))
dig_coding <- read_excel(paste(root_folder, "digital_coding_clean.xlsx", sep=""))

### LOAD THE AI-LABELLED DATA
# Gemma
gemma_old <- read_excel(paste(root_folder, "gemma_all.xlsx", sep=""))
gemma1 <- read_excel(paste(root_folder, "gemma_20250512_022018.xlsx", sep=""))

gemma1 <- gemma1[!is.na(gemma1$type_ad), ]

gemma_old[names(gemma_old) != "response_time"] <- 
  lapply(gemma_old[names(gemma_old) != "response_time"], as.character)

gemma_all <- bind_rows(gemma_old, gemma1)
sum(duplicated(gemma_all$img_id)) # check for duplicates

write_xlsx(gemma_all, paste(root_folder, "gemma_all.xlsx", sep=""))

sum(gemma1$response_time, na.rm = TRUE)/3600

# Qwen
qwen_old <- read_excel(paste(root_folder, "qwen_all.xlsx", sep=""))
qwen1 <- read_excel(paste(root_folder, "qwen20250609_194509.xlsx", sep=""))

qwen_old[names(qwen_old) != "response_time"] <- 
  lapply(qwen_old[names(qwen_old) != "response_time"], as.character)

qwen_all <- bind_rows(qwen_old, qwen1)
qwen_all <- qwen_all[!is.na(qwen_all$type_ad), ]

sum(duplicated(qwen_all$img_id)) # check for duplicates
qwen_all <- qwen_all[!duplicated(qwen_all$img_id),]

setdiff(gemma_all$img_id, qwen_all$img_id)
setdiff(qwen_all$img_id, gemma_all$img_id)

write_xlsx(qwen_all, paste(root_folder, "qwen_all.xlsx", sep=""))

sum(qwen1$response_time, na.rm = TRUE)/3600


temp <- qwen_all[sample(nrow(qwen_all), 70), ]
sum(temp$response_time, na.rm = TRUE)/3600

# GPT
gpt_old <- read_excel(paste(root_folder, "gpt_all.xlsx", sep=""))
gpt2 <- read_excel(paste(root_folder, "gpt_20250518_190311.xlsx", sep=""))

gpt_old[names(gpt_old) != "response_time"] <- 
  lapply(gpt_old[names(gpt_old) != "response_time"], as.character)

gpt2 <- gpt2[!is.na(gpt2$type_ad), ]

gpt_all <- bind_rows(gpt_old, gpt2)
sum(duplicated(gpt_all$img_id)) # check for duplicates

gpt_all <- gpt_all[!is.na(gpt_all$type_ad), ]

write_xlsx(gpt_all, paste(root_folder, "gpt_all.xlsx", sep=""))
sum(gpt2$response_time)/60

# Pixtral
pixtral1 <- read_excel(paste(root_folder, "pixtral_20250502_103532.xlsx", sep=""))
pixtral2 <- read_excel(paste(root_folder, "pixtral_all.xlsx", sep=""))
#pixtral3[names(pixtral3) != "response_time"] <- 
#  lapply(pixtral3[names(pixtral3) != "response_time"], as.character)

pixtral_all <- bind_rows(pixtral1, pixtral2)
sum(duplicated(pixtral_all$img_id))

write_xlsx(pixtral_all, paste(root_folder, "pixtral_all.xlsx", sep=""))


setdiff(qwen_all$img_id, pixtral_all$img_id)
setdiff(pixtral_all$img_id, qwen_all$img_id)


# outdoor ads
root_folder <- "C:/Users/P70090005/Desktop/phd/AI-validation/data/outdoor ads/"

gpt1 <- read_excel(paste(root_folder, "ais/gpt_20250802_194421.xlsx", sep=""))
gpt2 <- read_excel(paste(root_folder, "ais/gpt_20250802_200341.xlsx", sep=""))
gpt3 <- read_excel(paste(root_folder, "ais/gpt_20250802_204553.xlsx", sep=""))

gpt_all <- bind_rows(gpt1, gpt2, gpt3)
sum(duplicated(gpt_all$img_id)) # check for duplicates

write_xlsx(gpt_all, paste(root_folder, "gpt_all.xlsx", sep=""))
sum(gpt_all$response_time)/60


pixtral1 <- read_excel(paste(root_folder, "ais/pixtral_20250803_133526.xlsx", sep=""))
pixtral2 <- read_excel(paste(root_folder, "ais/", sep=""))
pixtral3 <- read_excel(paste(root_folder, "ais/", sep=""))

pixtral_all <- bind_rows(pixtral1, pixtral2, pixtral3)
sum(duplicated(pixtral_all$img_id)) # check for duplicates

write_xlsx(pixtral_all, paste(root_folder, "pixtral_all.xlsx", sep=""))
sum(pixtral_all$response_time)/60

# =============================================================================
# for ad distribution
root_folder <- "C:/Users/P70090005/Documents/AI-validation/data/"
all_ads <- read_excel(paste(root_folder, "digital_coding_clean.xlsx", sep=""))
ads_1000 <- read_excel(paste(root_folder, "new_aws_1000.xlsx", sep=""))
languages <- read_excel(paste(root_folder, "language_flagging.xlsx", sep=""))
brands <- read_excel(paste(root_folder, "Belgium brands.xlsx", sep=""))

ads_by_cat <- all_ads %>% select(c(img_id, ad_creative_bodies.x, page_name, page_id)) %>%
  #merge(languages, by.x = "img_id", by.y = "ad_id") %>%
  merge(ads_1000, by = "img_id") %>%
  merge(brands, by = "page_id") %>%
  select(-c(ad_creative_bodies.x, page_name.x, page_name.y))

write_xlsx(ads_by_cat, paste(root_folder, "3000_ads_by_cat_language.xlsx", sep=""))


# ===== EQUAL SAMPLE SIZE BY CATEGORY ===== 
length(unique(ads_by_cat$category)) # 16 different categories = 61/62 ads per cat
nr_ads_to_select <- 400
#nr_per_cat <- 1000 / 16 # to select 1000 ads
nr_per_cat <- nr_ads_to_select / length(unique(ads_by_cat$category)) # to select 400 ads for the dietician

# check the underrepresented categories (where nr ads < 63)
cat_counts <- ads_by_cat %>% count(category)
underrep_cats <- cat_counts %>% filter(n < nr_per_cat) %>% pull(category)
overrep_cats <- cat_counts %>% filter(n >= nr_per_cat) %>% pull(category)

# take all ads from the underrepresented cats
underrep_ads <- ads_by_cat %>% filter(category %in% underrep_cats)
nr_underrep <- nrow(underrep_ads)

# target number per overrepresented categories
sample_per_cat <- floor((nr_ads_to_select - nr_underrep) / length(overrep_cats))

# sample from underrepr cats
overrep_ads <- ads_by_cat %>% filter(category %in% overrep_cats) %>%
  group_by(category) %>%
  # sample_per_cat might be bigger than what's available for some borderline categories.
  # If that's possible, replace `slice_sample(n = sample_per_cat)` with:
  # slice_sample(n = min(n(), sample_per_cat))
  slice_sample(n = sample_per_cat) %>%
  ungroup()

# bind the over and underrep ads together
ads_subsample <- bind_rows(underrep_ads, overrep_ads)

# distribute any remaining ads
set.seed(12101999)
remaining_ads <- ads_by_cat %>%anti_join(ads_subsample, by = "img_id")
extra_16 <- remaining_ads %>% filter(language != "EN") %>% sample_n(nr_per_cat - nr_underrep)
final_sample <- bind_rows(ads_subsample, extra_16)
 
# final check
final_sample %>% count(category)
final_sample %>% count(language)

final_sample <- final_sample[sample(1:nrow(final_sample)), ]
rownames(final_sample) <- NULL

write_xlsx(final_sample, paste(root_folder, "dietician_400_ads.xlsx", sep=""))

# join dieticians add to get url from qualtrics
prev <- read.csv(paste(root2, "images.csv", sep="")) # all images with url
diet <- diet %>% mutate(img_id = paste0(img_id, ".png")) %>%
  left_join(prev, by = 'img_id') %>%
  select(-c("category", "page_id", "language.y", "page_name.y", "ad_bodies_clean.y", "max_count"))
write.csv(diet, paste(root_folder, "dieticians_400_loop.csv", sep=""), row.names = FALSE)

# =============================================================================
# merge with previous data to get the url of the images
root2 <- "//unimaas.nl/users/Employees/P70090005/data/Desktop/phd/ad libraries and brands/AI-validation/data/qualtrics survey - aws setup/"
previous <- read.csv(paste(root2, "images_old.csv", sep=""))

new <- read_excel(paste(root2, "new_aws_1000.xlsx", sep=""))

new <- new %>% mutate(img_id = paste0(img_id, ".png")) %>%
  left_join(previous, by = 'img_id') %>% 
  select(-c("page_name.x", "ad_bodies_clean.y", "language.y"))

write.csv(new, paste(root2, "images.csv", sep=""), row.names = FALSE)

# separate by language
new2 <- read.csv(paste(root2, "images.csv", sep=""))
en <- new2 %>% filter(language == "EN") %>% select(-c("language"))
nl <- new2 %>% filter(language == "NL") %>% select(-c("language"))
fr <- new2 %>% filter(language == "FR") %>% select(-c("language"))
both <- new2 %>% filter(language == "BOTH") %>% select(-c("language"))
both <- both[sample(1:nrow(both)), ] # shuffle

half <- floor(nrow(both) / 2)
# split both languages in half
both_nl <- both[1:half, ]
both_fr <- both[(half + 1):nrow(both), ]

# append back to each language
nl <- bind_rows(nl, both_nl)
fr <- bind_rows(fr, both_fr)

write.csv(en, paste(root2, "images_en.csv", sep=""), row.names = FALSE)
write.csv(fr, paste(root2, "images_fr.csv", sep=""), row.names = FALSE)
write.csv(nl, paste(root2, "images_nl.csv", sep=""), row.names = FALSE)


# =============================================================================
### ===== HELPER FUNCTIONS FOR ADJUSTING THE LABELS =====
root_folder <- "C:/Users/P70090005/Documents/AI-validation/gpu outputs/"
gemma <- read_excel(paste(root_folder, "gemma_all_1000.xlsx", sep=""))
pixtral <- read_excel(paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
gpt <- read_excel(paste(root_folder, "gpt_all_1000.xlsx", sep=""))
qwen <- read_excel(paste(root_folder, "qwen_all.xlsx", sep=""))


# create new ad_types and who_cats based on whether there is alcohol
alcohol_changes <- function(df) {
  # adjust type_ad: overwrite with "9" if is_alcohol == 1hm 
  df[["new_type_ad"]] <- ifelse(df[["is_alcohol"]] == 1, "9", df[["type_ad"]])
  
  # adjust who_cat: append "A" or replace with "A" if NA/empty
  df[["new_who_cat"]] <- ifelse(
    df[["is_alcohol"]] == 1,
    ifelse(is.na(df[["who_cat"]]) | df[["who_cat"]] == "",
           "A",
           paste("A", df[["who_cat"]], sep = ", ")),
    df[["who_cat"]]
  )
  
  return(df)
}

gemma <- alcohol_changes(gemma)
pixtral <- alcohol_changes(pixtral)
gpt <- alcohol_changes(gpt)
qwen <- alcohol_changes(qwen)

gemma <- gemma[order(gemma$img_id), ]
gpt <- gpt[order(gpt$img_id), ]
pixtral <- pixtral[order(pixtral$img_id), ]
qwen <- qwen[order(qwen$img_id), ]

gemma$model <- "gemma"
gpt$model <- "gpt"
pixtral$model <- "pixtral"
qwen$model <- "qwen"

write_xlsx(gemma, paste(root_folder, "gemma_all_1000.xlsx", sep=""))
write_xlsx(pixtral, paste(root_folder, "pixtral_all_1000.xlsx", sep=""))
write_xlsx(gpt, paste(root_folder, "gpt_all_1000.xlsx", sep=""))
write_xlsx(qwen, paste(root_folder, "qwen_all_1000.xlsx", sep=""))

# outdoor
gpt_all <- alcohol_changes(gpt_all)
pixtral_all <- alcohol_changes(pixtral_all)

write_xlsx(pixtral_all, paste(root_folder, "pixtral_all_outdoor.xlsx", sep=""))
write_xlsx(gpt_all, paste(root_folder, "gpt_all_outdoor.xlsx", sep=""))

# =============================================================================
### ===== PROCESSING THE SURVEY DATA =====

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

# ===== !!! REMOVE ADS WITH <3 CODINGS !!! ===== 
responses_long_all <- responses_long_all %>%
  group_by(Image_ID) %>%
  filter(n() >= 3) %>%
  ungroup()

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
    id_cols = ad_id,
    names_from = diet_number,
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










