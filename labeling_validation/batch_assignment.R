library(readxl)
library(dplyr)

set.seed(123) # create randomized batches of 20 images

root_folder <- "//unimaas.nl/users/Employees/P70090005/data/Desktop/phd/ad libraries and brands/AI-validation/"
dig_coding <- read_excel(paste(root_folder, "validation results/digital_coding_clean.xlsx", sep=""))
dig_coding <- dig_coding %>% # remove the missing ad text
  mutate(ad_creative_bodies = ifelse(is.na(ad_creative_bodies), "NO AD TEXT AVAILABLE.", ad_creative_bodies))

image_urls <- dig_coding %>%
  select(img_id, ad_creative_bodies) %>%
  mutate(image_url = paste0("https:/storage.googleapis.com/online_ad_images/unique_images/", img_id, ".png"))

randomized_data <- image_urls %>% sample_frac()

# split into 150 batches of 20 images
batches <- split(randomized_data, ceiling(seq_along(1:nrow(randomized_data)) / 20))

# repeat each batch 3 times (for 3 coders)
batches_repeated <- rep(batches, each = 3)

# randomize the order of the assignments to coders
randomized_batches <- sample(batches_repeated)

# example link in the google cloud bucket:
# https://storage.googleapis.com/online_ad_images/unique_images/ad_1000084181569778_img.png

df_mturk <- do.call(rbind, lapply(randomized_batches, function(batch) {
  urls_and_texts <- setNames(as.list(c(batch$image_url, batch$ad_creative_bodies)),
                             c(paste0("image_url_", seq_along(batch$image_url)), 
                               paste0("ad_text_", seq_along(batch$ad_creative_bodies))))
  
  as.data.frame(urls_and_texts)
}))

# save as CSV
write.csv(df_mturk, paste(root_folder, "data/MTurk_urls.csv", sep=""), row.names = FALSE)
