# Install and load needed packages
needed_packages <- c("tidyverse",
                     "rstudioapi")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located
# setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

# Load helper functions and other scripts
source("airbnb_listings_summaries.R", chdir = TRUE)
source(file.path(getwd(), "Helpers", "from_row_to_col.R", fsep="/"))
source(file.path(getwd(), "Helpers", "recode_all.R", fsep="/"))

## Correlation between different variables and price

# Transform categorical variables from rows to column
listings_price <- listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% from_row_to_col("cancellation_policy") %>%
  recode_all("district") %>% from_row_to_col("district") %>%
  recode_all("vbb_area") %>% from_row_to_col("vbb_area") %>%
  recode_all("neighbourhood") %>% from_row_to_col("neighbourhood") %>%
  dplyr::select(-id, -lat, -long, -listing_url)

# Create dataframe with price correlations
listings_price_correlation <- cor(listings_price$price, listings_price) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::rename(corr = V1) %>%
  tibble::rownames_to_column(var = "variable") %>%
  dplyr::filter(variable != "price") %>%
  dplyr::mutate(sign = ifelse(corr > 0, "positive",
                       ifelse(corr < 0, "negative",
                                        "null")))


## Linear regression

# Fit linear model
listings_price_lm <- lm(price ~ ., data = listings_price) 

# Extract coefficients and p-values
listings_price_lm_results <- data.frame(
  variable        = names(listings_price_lm$coefficients[!is.na(listings_price_lm$coefficients)]),
  coefficient     = listings_price_lm$coefficients[!is.na(listings_price_lm$coefficients)],
  significant     = ifelse(summary(listings_price_lm)$coefficients[,4] < 0.5, TRUE, FALSE),
  model_r_squared = summary(listings_price_lm)$r.squared) %>%
  dplyr::mutate_if(is.numeric, function(x) round(x, 4))

# Remove objects not further needed
rm("listings_price", "listings_price_lm")
rm(list=lsf.str()) # All functions

# Save workspace
save.image(file = "./ShinyApp/workspace_for_shinyapp.RData")
