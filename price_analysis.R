# Install and load needed packages
needed_packages = c("tidyverse",
                     "rstudioapi",
                     "corrplot")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located

  # This works when run directly
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  setwd(dirname(sys.frame(1)$ofile))

# Load helper functions and other scripts
source("clustering.R", chdir = TRUE)
source(file.path(getwd(), "Helpers", "from_row_to_col.R", fsep="/"))
source(file.path(getwd(), "Helpers", "recode_all.R", fsep="/"))

## Correlation between different variables and price

# Transform categorical variables from rows to column
listings_price = listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% from_row_to_col("cancellation_policy") %>%
  recode_all("security_deposit_yn") %>% from_row_to_col("security_deposit_yn") %>%
  recode_all("cleaning_fee_yn") %>% from_row_to_col("cleaning_fee_yn") %>%
  recode_all("reviewed_yn") %>% from_row_to_col("reviewed_yn") %>%
  recode_all("station_count") %>% from_row_to_col("station_count") %>%
  recode_all("attraction_count") %>% from_row_to_col("attraction_count") %>%
  recode_all("district") %>% from_row_to_col("district") %>%
  recode_all("vbb_zone") %>% from_row_to_col("vbb_zone") %>%
  dplyr::select(-id, -lat, -long, -listing_url, -neighbourhood)

# Create dataframe with price correlations
listings_price_correlation = cor(listings_price$price, listings_price) %>%
  as.data.frame() %>%
  tidyr::gather() %>%
  dplyr::rename(variable    = key,
                correlation        = value) %>%
  dplyr::filter(variable != "price")

print("Price correlation calculated.")

# Plot correlation
# Example corrplot
listings %>%
  dplyr::select(price, accommodates, bedrooms, beds, minimum_nights, 
                attraction_count, attraction_dist, station_count, station_dist) %>%
  cor() %>%
  corrplot(method = "circle", type = "upper")
dev.copy2pdf(file = "./SeminarPaper/corrplot.pdf")
dev.off()

# Correlation with price plot
correlation_plot(corr_df = listings_price_correlation, variables = c("property_type", "district"))
  
dev.copy2pdf(file = "./SeminarPaper/price_correlation.pdf")
dev.off()

## Linear regression

# Fit linear model
listings_price_lm = lm(price ~ ., data = listings_price) 

# Extract coefficients and p-values
listings_price_lm_results = data.frame(
  variable        = names(listings_price_lm$coefficients[!is.na(listings_price_lm$coefficients)]),
  coefficient     = listings_price_lm$coefficients[!is.na(listings_price_lm$coefficients)],
  significant     = ifelse(summary(listings_price_lm)$coefficients[,4] < 0.5, TRUE, FALSE),
  model_r_squared = summary(listings_price_lm)$r.squared) %>%
  dplyr::mutate_if(is.numeric, function(x) round(x, 4))

print("Finished calculating linear regression on price.")

# Remove objects not further needed
rm("listings_price_lm", "listings_price_lm_results")
rm(list=lsf.str()) # All functions
