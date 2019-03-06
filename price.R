# Install and load needed packages
needed_packages <- c("tidyverse",
                     "rstudioapi")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located
setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

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
  dplyr::select(-id, -lat, -long, -listing_url, -neighbourhood)

# Create dataframe with price correlations
listings_price_correlation = cor(listings_price$price, listings_price) %>%
  as.data.frame() %>%
  tidyr::gather() %>%
  dplyr::rename(variable = key,
                corr     = value) %>%
  dplyr::filter(variable != "price") %>%
  dplyr::mutate(sign = ifelse(corr > 0, "positive",
                              ifelse(corr < 0, "negative",
                                     "null")))

print("Finished calculating price correlation.")

# Plot correlation
listings_price_correlation %>%
  dplyr::mutate(corr = abs(corr)) %>%
  ggplot(aes(x = variable, y = corr, fill = sign)) +
  geom_bar(stat="identity") +
  scale_fill_manual("sign", values = c("positive" = "green4", "negative" = "red1", "null" = "blue3")) +
  # coord_flip() +
  labs(x = "variables", y = "correlation with price") +
  theme_bw() +
  # coord_fixed(ylim = length(unique(listings_price_correlation$variable))) +
  # expand_limits(x = 3*ncol(listings_price_correlation)) +
  theme(axis.text.x     = element_text(colour = ifelse(listings_price_correlation$corr > 0, "green4",
                                                ifelse(listings_price_correlation$corr < 0, "red1",
                                                                                            "blue3")),
                                   size = rel(1.5),
                                   angle = 90, hjust = 1),
        axis.text.y     = element_text(size = rel(1.5)),
        axis.title.x    = element_text(size = rel(1.5)),
        axis.title.y    = element_text(size = rel(1.5)),
        legend.text     = element_text(size = rel(1.2)),
        legend.title    = element_text(size = rel(1.5)),
        plot.title      = element_text(hjust = 0.5, size = 17),
        # plot.margin     = unit(c(0, corr_margin, 0, corr_margin), "cm"),
        legend.position = "bottom") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) 
  

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

print("Finished calculating linear regression on price.")

# Remove objects not further needed
rm("listings_price_lm", "listings_price_lm_results")
rm(list=lsf.str()) # All functions
