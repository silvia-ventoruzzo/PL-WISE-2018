# Load packages
needed_packages = c("leaflet", "ggplot2", "htmltools", "mapview")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE))
      {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages")

# Set working directory to the one where the file is located

  # This works when run directly
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  # setwd(dirname(sys.frame(1)$ofile))

# Load file where the sf object is created
source("berlin_districts_neighbourhoods.R", chdir = TRUE)

# Colors for polygons
district_colors = colorspace::rainbow_hcl(n = 12)

# Leaflet map of the districts
berlin_district_leaflet = leaflet() %>%
    addTiles() %>%
    addPolygons(data = berlin_district_sf,
                weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                fillColor = district_colors, color = "grey") %>%
    addLabelOnlyMarkers(data = berlin_districts_names,
                      lng = ~long, lat = ~lat, 
                      label = ~lapply(name, htmltools::HTML),
                      labelOptions = labelOptions(noHide = TRUE, 
                                                  direction = 'center',
                                                  textOnly = TRUE, 
                                                  textsize = "20px")) %>%
    setView(lng  = berlin_neighbourhoods_names$long
                              [berlin_neighbourhoods_names$id == "Mitte"],
            lat  = berlin_neighbourhoods_names$lat
                              [berlin_neighbourhoods_names$id == "Mitte"],
            zoom = 11)
berlin_district_leaflet

# mapview::mapshot(berlin_district_leaflet, 
#                  file = "berlin_district_leaflet.pdf",
#                  remove_controls = c("zoomControl", "layersControl", 
#                                      "homeButton","scaleBar"))


# ggplot map
ggplot() +
    geom_sf(data = berlin_district_sf, 
            show.legend = FALSE, color = "grey") +
    coord_sf(datum = NA) +
    aes(fill = district_colors) +
    geom_text(data = berlin_districts_names, 
              aes(x = long, y = lat, 
                  label = gsub("<br>", "\n", name),
                  hjust = "center")) +
    theme_classic() +
    theme(plot.title   = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

# dev.copy2pdf(file = "berlin_district_ggplot.pdf")
# dev.copy2pdf(file = "../SeminarPaper/berlin_district_ggplot.pdf")
