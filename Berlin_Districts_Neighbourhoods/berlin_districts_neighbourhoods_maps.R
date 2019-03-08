# Load packages
needed_packages = c("leaflet", "ggplot2", "htmltools", "mapview")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE))
      {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages")

# Set working directory to the one where the file is located
if (interactive()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  setwd(dirname(sys.frame(1)$ofile))
}

# Load file where the sf object is created
source("berlin_districts_neighbourhoods.R", chdir = TRUE)

# Leaflet map of the districts
berlin_district_leaflet = leaflet() %>%
    addTiles() %>%
    addPolygons(data = berlin_district_sf,
                weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                fillColor = rainbow(12), color = "grey") %>%
    addLabelOnlyMarkers(data = berlin_districts_names,
                      lng = ~long, lat = ~lat, label = ~lapply(name, htmltools::HTML),
                      labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                  textOnly = TRUE, textsize = "11px"))
berlin_district_leaflet

mapview::mapshot(berlin_district_leaflet, file = "berlin_district_leaflet.pdf",
                 remove_controls = c("zoomControl", "layersControl", "homeButton","scaleBar"))


# ggplot map
ggplot() +
    geom_sf(data = berlin_district_sf, show.legend = FALSE, color = "grey") +
    coord_sf(datum = NA) +
    aes(fill = rainbow(12)) +
    geom_text(data = berlin_districts_names, aes(x = long, y = lat, 
                                                 label = gsub("<br>", "\n", name),
                                                 hjust = "center")) +
    theme_classic() +
    theme(plot.title   = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

dev.copy2pdf(file = "berlin_district_ggplot.pdf")
