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
source("berlin_vbb_zones.R", chdir = TRUE)

# Leaflet map
berlin_vbb_zones_leaflet = leaflet() %>%
    addTiles() %>%
    addPolygons(data = berlin_vbb_AB_sf,
                weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                fillColor = c("cadetblue", "darkorange"), 
                color = "black") %>%
    addLabelOnlyMarkers(data = berlin_vbb_AB_names,
                      lng = ~long, lat = ~lat, 
                      label = ~lapply(name, htmltools::HTML),
                      labelOptions = labelOptions(noHide = TRUE, 
                                                  direction = 'center',
                                                  textOnly = TRUE, 
                                                  textsize = "20px")) %>%
    setView(lng  = berlin_vbb_AB_names$long
                      [berlin_vbb_AB_names$id == "A"],
            lat  = berlin_vbb_AB_names$lat
                      [berlin_vbb_AB_names$id == "A"],
            zoom = 11)
berlin_vbb_zones_leaflet

# mapview::mapshot(berlin_vbb_zones_leaflet, 
#                  file = "berlin_vbb_zones_leaflet.pdf",
#                  remove_controls = c("zoomControl", "layersControl", 
#                                      "homeButton","scaleBar"))
# mapview::mapshot(berlin_vbb_zones_leaflet, 
#                  file = "../SeminarPaper/berlin_vbb_zones_leaflet.pdf",
#                  remove_controls = c("zoomControl", "layersControl", 
#                                      "homeButton","scaleBar"))


# ggplot map
ggplot() +
    geom_sf(data = berlin_vbb_AB_sf, show.legend = FALSE, color = "black") +
    coord_sf(datum = NA) +
    aes(fill = c("darkorange", "cadetblue")) +
    geom_text(data = berlin_vbb_AB_names, 
              aes(x = long, y = lat, label = id, hjust = "center"), 
              size = 5) +
    theme_classic() +
    theme(plot.title   = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

# dev.copy2pdf(file = "berlin_vbb_zones_ggplot.pdf")
# dev.copy2pdf(file = "../SeminarPaper/berlin_vbb_zones_ggplot.pdf")
