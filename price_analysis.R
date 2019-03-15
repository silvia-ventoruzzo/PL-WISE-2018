# Install and load needed packages
needed_packages = c("tidyverse",
                     "rstudioapi",
                     "corrplot",
                     "Jmisc",
                     "xtable",
                     "lindia")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {
    install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located

  # This works when run directly
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  setwd(dirname(sys.frame(1)$ofile))

# Load helper functions and other scripts
source("exploratory_data_analysis.R", chdir = TRUE)
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

## Correlation between different variables and price

# Transform categorical variables from rows to column
listings_price = listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% 
  from_row_to_col("cancellation_policy") %>%
  recode_all("security_deposit") %>% from_row_to_col("security_deposit") %>%
  recode_all("cleaning_fee") %>% from_row_to_col("cleaning_fee") %>%
  recode_all("station_count") %>% from_row_to_col("station_count") %>%
  recode_all("attraction_count") %>% from_row_to_col("attraction_count") %>%
  recode_all("instant_bookable") %>% from_row_to_col("instant_bookable") %>%
  recode_all("host_is_superhost") %>% 
  from_row_to_col("host_is_superhost") %>%
  recode_all("host_has_profile_pic") %>% 
  from_row_to_col("host_has_profile_pic") %>%
  recode_all("host_identity_verified") %>% 
  from_row_to_col("host_identity_verified") %>%
  recode_all("district") %>% from_row_to_col("district") %>%
  recode_all("vbb_zone") %>% from_row_to_col("vbb_zone") %>%
  dplyr::select(-id, -lat, -long, -listing_url, -neighbourhood)

# Create dataframe with price correlations
listings_price_correlation = cor(listings_price$price, listings_price) %>%
  as.data.frame() %>%
  tidyr::gather() %>%
  dplyr::rename(variable    = key,
                correlation = value) %>%
  dplyr::filter(variable != "price")

print("Price correlation calculated.")

# Plot correlation
# Example corrplot
# listings %>%
#   dplyr::select(price, accommodates, bedrooms, beds, 
#                 minimum_nights, station_dist) %>%
#   cor() %>%
#   corrplot(method = "circle", type = "upper")
# dev.copy2pdf(file = "./SeminarPaper/corrplot.pdf")
# dev.off()

# Correlation with price plot
correlation_plot(corr_df   = listings_price_correlation, 
                 variables = c("property_type", "district"))
  
# dev.copy2pdf(file = "./SeminarPaper/price_correlation.pdf")
# dev.off()

## Linear regression

# Fit linear model
listings_price_lm = lm(price ~ ., data = listings_price) 

# Extract coefficients and p-values
listings_price_lm_results = data.frame(
  variable        = names(listings_price_lm$coefficients
                                [!is.na(listings_price_lm$coefficients)]),
  coefficient     = listings_price_lm$coefficients
                                [!is.na(listings_price_lm$coefficients)],
  significant     = ifelse(summary(listings_price_lm)
                                $coefficients[,4] < 0.5, TRUE, FALSE),
  model_r_squared = summary(listings_price_lm)$r.squared) %>%
  dplyr::mutate_if(is.numeric, function(x) round(x, 4))

# listings_price_lm_results %>%
#     dplyr::filter(variable %in% c("(Intercept)", "accommodates", "host_listings_count")) %>%
#     dplyr::select(-model_r_squared) %>%
#     xtable::xtable() %>%
#     print(include.rownames = FALSE)

# Descriptive statistics of residuals
listings_price_lm_residuals = data.frame(
  residuals = listings_price_lm$residuals
  )

# descriptive_statistics(listings_price_lm_residuals) %>%
#   dplyr::select(-variable) %>%
#   xtable::xtable() %>%
#   print(include.rownames = FALSE)

# Fitted vs actual plot
ggplot(listings_price_lm, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  geom_smooth(method = lm) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot") +
  theme_bw()
# dev.copy2pdf(file = "./SeminarPaper/fittedactual.pdf")
# dev.off()

# Residuals QQ Plot
lindia::gg_qqplot(listings_price_lm) +
  theme_bw()
# dev.copy2pdf(file = "./SeminarPaper/qqplot.pdf")
# dev.off()

print("Linear regression on price calculated.")

# Remove objects not further needed
rm("listings_price_lm", "listings_price_lm_results")
rm(list=lsf.str()) # All functions
