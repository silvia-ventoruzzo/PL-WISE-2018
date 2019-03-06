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
# setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

# Load helper functions and other scripts
source("price.R", chdir = TRUE)
source(file.path(getwd(), "Helpers", "from_row_to_col.R", fsep="/"))
source(file.path(getwd(), "Helpers", "recode_all.R", fsep="/"))

## Calculate number of clusters

# Scale dataframe
listings_scaled = listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% from_row_to_col("cancellation_policy") %>%
  dplyr::select(-id, -lat, -long, -listing_url, - district, -vbb_area, -neighbourhood) %>%
  scale()

# Total variance explained
set.seed(900114)
k_values = 2:96
tve = data.frame(clusters = k_values,
                 tve      = rep(NA, length(k_values)))
clk = list()
for (k in k_values) {
  clk[[k-1]] = kmeans(listings_scaled, centers = k, iter.max = 20)
  names(clk)[k-1] = paste(k, "clusters", sep = " ")
  tve$tve[k-1] = 1-clk[[k-1]]$tot.withinss/clk[[k-1]]$totss
  print(paste("k-means with", k, "clusters done", sep = " "))
}
ggplot(data = tve,
       aes(x = clusters,
           y = tve)) +
    geom_line(color = "grey") +
    geom_point(color = "red") +
    scale_x_continuous(breaks = seq(0, 100, by = 5))
# 40 clusters

## Clustering
n_clusters = 40
listings_kmeans = clk[[paste(n_clusters, "clusters", sep = " ")]]

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