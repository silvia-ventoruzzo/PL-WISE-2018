# Load packages
needed_packages <- c("dplyr", "data.table", "sf", "tibble")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located
setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

# Load helpers
source("helpers.R")

# Load shapefiles
berlin <- sf::st_read(file.path(getwd(), "spatial_data", "Berlin-Ortsteile-polygon.shp", fsep="/"))
stations <- sf::st_read(file.path(getwd(), "spatial_data", "gis_osm_transport_free_1.shp", fsep="/"))

# Create dataframe with names of stations on the Ringbahn (delimits Area A)
ringbahn_names_df <- base::data.frame(id = c("Südkreuz", "Schöneberg", "Innsbrucker Platz", "Bundesplatz",
                                             "Heidelberger Platz", "Hohenzollerndamm", "Halensee", "Westkreuz",
                                             "Messe Nord/ICC", "Westend", "Jungfernheide", "Beusselstraße",
                                             "Westhafen", "Wedding", "Gesundbrunnen", "Schönhauser Allee",
                                             "Prenzlauer Allee", "Greifswalder Straße", "Landsberger Allee", 
                                             "Storkower Straße", "Frankfurter Allee", "Ostkreuz", "Treptower Park",
                                             "Sonnenallee", "Neukölln", "Hermannstraße",
                                             "Tempelhof", "Südkreuz"),
                                      stringsAsFactors = FALSE) %>% 
  tibble::rownames_to_column(var = "order") %>%
  dplyr::mutate(order = as.numeric(order))

# Create sf object of Area A
berlin_vbb_A_sf <- stations %>%
  dplyr::filter(fclass %like% "railway") %>%
  dplyr::rename(id = name) %>%
  dplyr::mutate(id     = gsub("Berlin ", "", id),
                id     = gsub("Berlin-", "", id),
                id     = gsub(" *\\(.*?\\) *", "", id),
                id     = gsub("S ", "", id),
                id     = gsub("U ", "", id)) %>%
  points_midpoint() %>%
  dplyr::right_join(ringbahn_names_df, by = "id") %>%
  dplyr::arrange(order) %>%
  dplyr::select(long, lat) %>%
  base::as.matrix() %>%
  base::list() %>%
  sf::st_polygon() %>%
  st_sfc() %>%
  st_sf(crs = sf::st_crs(berlin)) 

# Create sf object of entire Berlin
berlin_sf <- berlin %>%
  sf::st_as_sf() %>%
  sf::st_set_crs(st_crs(berlin)) %>%
  dplyr::summarize(do_union = TRUE)

# Bind and intersect to create sf object with both VBB Areas (A and B)
berlin_vbb_AB_sf <- berlin_vbb_A_sf %>%
  base::rbind(berlin_sf) %>%
  sf::st_intersection() %>% 
  dplyr::mutate(id = ifelse(n.overlaps > 1, "A", "B")) %>%
  dplyr::select(-n.overlaps, -origins) %>%
  dplyr::arrange(desc(id))

# Create dataframes with coordinates where to show areas' names on map
berlin_vbb_A_names <- berlin_vbb_A_sf %>%
  sf::st_centroid() %>%
  sf::st_coordinates() %>%
  base::as.data.frame() %>%
  dplyr::rename(long = X,
                lat  = Y) %>%
  dplyr::mutate(id = "A")
berlin_vbb_B_names <- berlin_sf %>%
  sf::st_bbox() %>%
  base::as.matrix() %>%
  base::t() %>%
  as.data.frame() %>%
  dplyr::transmute(long = (xmax + xmin)/2,
                   lat  = (3*ymax + ymin)/4) %>%
  dplyr::mutate(id = "B")
berlin_vbb_AB_names <- berlin_vbb_A_names %>%
  rbind(berlin_vbb_B_names)

# Remove not needed data
rm("berlin", "berlin_sf", "ringbahn_names_df", "stations", "berlin_vbb_A_sf", 
   "points_midpoint", "berlin_vbb_A_names", "berlin_vbb_B_names")
