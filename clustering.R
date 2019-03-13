# Install and load needed packages
needed_packages = c("tidyverse",
                    "rstudioapi",
                    "ggplot2",
                    "ggfortify")
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

## Calculate number of clusters

# Scale dataframe
listings_scaled = listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% from_row_to_col("cancellation_policy") %>%
  recode_all("security_deposit") %>% from_row_to_col("security_deposit") %>%
  recode_all("cleaning_fee") %>% from_row_to_col("cleaning_fee") %>%
  recode_all("station_count") %>% from_row_to_col("station_count") %>%
  recode_all("attraction_count") %>% from_row_to_col("attraction_count") %>%
  dplyr::select(-id, -lat, -long, -listing_url, - district, -vbb_zone, -neighbourhood) %>%
  scale()

# Total variance explained
number_of_clusters(scaled_df   = listings_scaled,
                   max         = 96,
                   iter_max    = 20, 
                   plot_breaks = seq(0, 100, by = 5))
dev.copy2pdf(file = "./SeminarPaper/numclusters.pdf")
dev.off()

## Clustering
n_clusters = 50
listings_kmeans = kmeans(listings_scaled, centers = n_clusters, iter.max = 20)

# Principal components
listings_pc = prcomp(x = listings_scaled, center = FALSE, scale = FALSE)

# Plot clusters
ggplot2::autoplot(listings_pc, data = listings_kmeans, colour = "cluster")

ggplot() +
    geom_sf(data = berlin_district_sf, show.legend = FALSE, color = "black") +
    coord_sf(datum = NA) +
    geom_text(data = berlin_districts_names, 
              aes(x = long, y = lat, label = lapply(name, htmltools::HTML), hjust = "center"), size = 3) +
    geom_point(data = listings, aes(x = long, y = lat, color = listings_clusters),
               alpha = 0.5) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle("Clustering of airbnb properties") +
    xlab("longitude") +
    ylab("latitude") +
    labs(color = "clusters") +
    theme_classic()

# Remove objects not further needed
rm("tve", "k", "clk", "k_values", "n_clusters")
rm(list=lsf.str()) # All functions