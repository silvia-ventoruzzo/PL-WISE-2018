# Install and load needed packages
needed_packages <- c("tidyverse",
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
source("exploratory_data_analysis.R", chdir = TRUE)
source(file.path(getwd(), "Helpers", "from_row_to_col.R", fsep="/"))
source(file.path(getwd(), "Helpers", "recode_all.R", fsep="/"))

## Correlation between different variables and price

# Transform categorical variables from rows to column
listings_price <- listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% from_row_to_col("cancellation_policy") %>%
  recode_all("security_deposit_yn") %>% from_row_to_col("security_deposit_yn") %>%
  recode_all("cleaning_fee_yn") %>% from_row_to_col("cleaning_fee_yn") %>%
  recode_all("reviewed_yn") %>% from_row_to_col("reviewed_yn") %>%
  recode_all("district") %>% from_row_to_col("district") %>%
  recode_all("vbb_zone") %>% from_row_to_col("vbb_zone") %>%
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
                                     "null"))) %>%
  dplyr::arrange(variable)

print("Finished calculating price correlation.")

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
listings_price_correlation %>%
  dplyr::mutate(corr = abs(corr)) %>%
  ggplot(aes(x = variable, y = corr, fill = sign)) +
  geom_bar(stat="identity") +
  scale_fill_manual("sign", values = c("positive" = "green4", "negative" = "red1", "null" = "blue3")) +
  coord_flip() +
  labs(x = "variables", y = "correlation", title = "Correlation with price") +
  theme_bw() +
  # coord_fixed(ylim = length(unique(listings_price_correlation$variable))) +
  # expand_limits(x = 3*ncol(listings_price_correlation)) +
  theme(axis.text.y     = element_text(color = ifelse(listings_price_correlation$sign == "positive", "green4",
                                              ifelse(listings_price_correlation$sign == "negative", "red1",
                                                                                            "blue3"))),
        axis.text.x     = element_text(size = rel(1.5)),
        axis.title.x    = element_text(size = rel(1.5)),
        axis.title.y    = element_text(size = rel(1.5)),
        legend.text     = element_text(size = rel(1.2)),
        legend.title    = element_text(size = rel(1.5)),
        plot.title      = element_text(hjust = 0.5, size = 17),
        # plot.margin     = unit(c(0, corr_margin, 0, corr_margin), "cm"),
        legend.position = "bottom") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) 
  
dev.copy2pdf(file = "./SeminarPaper/price_correlation.pdf")
dev.off()

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
