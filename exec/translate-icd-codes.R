#' This R script is designed to translate the descriptions of ICD-10-GM codes 
#' from German to English using the Deepl API. It utilizes the deeplr package 
#' for translation and requires a Deepl API key, which you can obtain from their 
#' website.
#'
#' This translation was done on the 18th of March 2024 

# Load necessary libraries
# install.packages("deeplr")
library(deeplr)

# Deepl API key. You can apply for your own on their website
my_key <- "your_deepl_api_key"

# Read in the data containing ICD-10-GM codes and their descriptions
data <- readRDS("data/rivaroxaban_dataset_original_German.rds")

# Check the current usage of your translation credit
deeplr::usage2(my_key)

# Translate the descriptions from German to English using Deepl API
translation <- deeplr::translate2(
  text = data$raw_data$Description,
  target_lang = "EN",
  auth_key = my_key
)

# Save the translated descriptions
saveRDS(translation, 'data/translation.rds')

# Save the new translated data set
data$raw_data$Description <- translation
saveRDS(data, "data/rivaroxaban_dataset.rds")

