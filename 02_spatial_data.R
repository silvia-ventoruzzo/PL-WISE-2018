# Install and load needed packages
needed_packages <- c("tidyverse",
                     "sf",
                     "data.table",
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
source("01_functions.R")
source(file.path(getwd(), "Berlin_VBB_Areas", "berlin_vbb_areas.R", fsep="/"),
       chdir = TRUE)
source(file.path(getwd(), "Berlin_Districts_Neighbourhoods", "berlin_districts_neighbourhoods.R", fsep="/"),
       chdir = TRUE)

# Load shapefiles
stations <- sf::st_read(file.path(getwd(), "spatial_data", "gis_osm_transport_free_1.shp", fsep="/"))

# Create sf object with train/subway stations
railway_stations_sf <- stations %>%
  dplyr::filter(fclass %like% "railway") %>%
  dplyr::rename(id = name) %>%
  dplyr::mutate(id     = gsub("Berlin ", "", id),
                id     = gsub("Berlin-", "", id),
                id     = gsub(" *\\(.*?\\) *", "", id),
                id     = gsub("S ", "", id),
                id     = gsub("U ", "", id)) %>%
  points_midpoint()



# Add groups and views to sf objects
berlin_district_sf <- berlin_district_sf %>%
  dplyr::mutate(view = "Neighbourhoods")
berlin_vbb_AB_sf <- berlin_district_sf %>%
  dplyr::mutate(view  = "VBB Areas",
                group = id)
berlin_neighbourhood_singlebuckow_sf <- berlin_neighbourhood_singlebuckow_sf %>%
  dplyr::mutate(view = "Districts")
berlin_sf <- base::rbind(berlin_district_sf, berlin_vbb_AB_sf, berlin_neighbourhood_singlebuckow_sf) %>%
  sf::st_cast(to = 'MULTIPOLYGON')

# Join dataframes with names
berlin_names_sf <- base::rbind(berlin_district_sf, vbb_AB_sf, berlin_neighbourhood_singlebuckow_sf) %>%
  sf::st_cast(to = 'MULTIPOLYGON')

rm(berlin_district_sf, berlin_vbb_AB_sf, berlin_neighbourhood_singlebuckow_sf,
   berlin_districts_names, berlin_vbb_AB_names, berlin_neighbourhoods_names)
