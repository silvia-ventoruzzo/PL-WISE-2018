# Install and load needed packages
needed_packages = c("tidyverse",
                     "rstudioapi",
                     "sf")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {
    install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located

  # This works when run directly
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  # setwd(dirname(sys.frame(1)$ofile))

# Load scripts
source("clustering.R", chdir = TRUE)

# Calculate the center of Berlin polygon
berlin_center = berlin_neighbourhood_singlebuckow_sf %>%
  dplyr::group_by() %>%
  dplyr::summarize(do_union = TRUE) %>%
  sf::st_centroid() %>%
  sf::st_coordinates() %>%
  base::as.data.frame() %>%
  dplyr::rename(long = X,
                lat  = Y) %>%
  dplyr::select(lat, long)

# Join area summaries
listings_area_summary = rbind(listings_district_summary, listings_vbb_summary, listings_neighbourhood_summary)

# Join sf objects
berlin_district_sf = berlin_district_sf %>%
  dplyr::mutate(view = "Districts")
berlin_neighbourhood_singlebuckow_sf = 
  berlin_neighbourhood_singlebuckow_sf %>%
  dplyr::mutate(view = "Neighbourhoods")
berlin_vbb_AB_sf = berlin_vbb_AB_sf %>%
  dplyr::mutate(view  = "VBB Zones",
                group = id)
berlin_sf = rbind(berlin_district_sf, 
                  berlin_neighbourhood_singlebuckow_sf, 
                  berlin_vbb_AB_sf)

# Join names dataframes
berlin_districts_names = berlin_districts_names %>%
  dplyr::mutate(view = "Districts")
berlin_neighbourhoods_names = berlin_neighbourhoods_names %>%
  dplyr::mutate(view = "Neighbourhoods")
berlin_vbb_AB_names = berlin_vbb_AB_names %>%
  dplyr::mutate(view  = "VBB Zones",
                group = id)
berlin_names = rbind(berlin_districts_names, 
                     berlin_neighbourhoods_names, 
                     berlin_vbb_AB_names)

# Recode and transform from row to columns factor variables
listings_recoded = listings %>%
  recode_all("room_type") %>% from_row_to_col("room_type") %>%
  recode_all("property_type") %>% from_row_to_col("property_type") %>%
  recode_all("cancellation_policy") %>% 
  from_row_to_col("cancellation_policy")

# Remove objects not further needed
rm("listings_district_summary", "listings_vbb_summary", 
   "listings_neighbourhood_summary", "berlin_district_sf", 
   "berlin_neighbourhood_singlebuckow_sf", "berlin_vbb_AB_sf",
   "berlin_districts_names", "berlin_neighbourhoods_names", 
   "berlin_vbb_AB_names")
rm(list=lsf.str()) # All functions

# Save workspace
# save.image(file = file.path(getwd(), "ShinyApp", "workspace_for_shinyapp.RData", fsep="/"))
