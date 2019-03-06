# Load packages
needed_packages = c("dplyr", "data.table", "sf", "tibble")
for (package in needed_packages) {
    if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
    library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located
setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

# Load shapefiles
berlin = sf::st_read(file.path(getwd(), "spatial_data", "Berlin-Ortsteile-polygon.shp", fsep="/"))

# Object with the neightbourhoods (and respective district)
berlin_neighbourhood_sf = berlin %>%
    dplyr::rename(id    = Name,
                  group = BEZNAME) %>%
    dplyr::select(id, group, geometry) %>%
    dplyr::arrange(group)

# Buckow is composed of two separate parts, so we need to join them
berlin_neighbourhood_singlebuckow_sf = berlin_neighbourhood_sf %>%
    dplyr::group_by(id, group) %>%
    dplyr::summarize(do_union = TRUE)

# Object with the districts
berlin_district_sf = berlin_neighbourhood_sf %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(do_union = TRUE) %>%
    dplyr::mutate(id = group)

# Create dataframes with the names for plotting
# Neighbourhoods
berlin_neighbourhoods_names = berlin_neighbourhood_sf %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    base::as.data.frame() %>%
    dplyr::rename(long = X,
                  lat  = Y) %>%
    dplyr::mutate(id    = berlin_neighbourhood_sf$id,
                  group = berlin_neighbourhood_sf$group,
                  name  = gsub("-", "-/n", berlin_neighbourhood_sf$id))

# Districts
berlin_districts_names = berlin_district_sf %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    base::as.data.frame() %>%
    dplyr::rename(long = X,
                  lat  = Y) %>%
    dplyr::mutate(id    = berlin_district_sf$id,
                  group = berlin_district_sf$group,
                  name  = gsub("-", "-/n", berlin_district_sf$id))

# Remove not needed data
rm("berlin")
