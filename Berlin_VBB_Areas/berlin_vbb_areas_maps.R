# Load packages
needed_packages = c("leaflet", "ggplot2", "htmltools", "mapview")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE))
      {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages")

# Set working directory to the one where the file is located
# setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

# Load file where the sf object is created
source("berlin_vbb_areas.R", chdir = TRUE)

# Leaflet map
berlin_vbb_areas_leaflet = leaflet() %>%
    addTiles() %>%
    addPolygons(data = berlin_vbb_AB_sf,
                weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                fillColor = c("cadetblue", "darkorange"), color = "black") %>%
    addLabelOnlyMarkers(data = berlin_vbb_AB_names,
                      lng = ~long, lat = ~lat, label = ~lapply(name, htmltools::HTML),
                      labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                  textOnly = TRUE, textsize = "20px"))
berlin_vbb_areas_leaflet
mapview::mapshot(berlin_vbb_areas_leaflet, file = "berlin_vbb_areas_leaflet.png")

# ggplot map
ggplot() +
    geom_sf(data = berlin_vbb_AB_sf, show.legend = FALSE, color = "black") +
    coord_sf(datum = NA) +
    aes(fill = c("cadetblue", "darkorange")) +
    geom_text(data = berlin_vbb_AB_names, aes(x = long, y = lat, label = id, hjust = "center"), size = 5) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle("ggplot map of Berlin's VBB Areas") +
    xlab("longitude") +
    ylab("latitude") +
    theme_classic()
dev.copy2pdf(file = "berlin_vbb_areas_ggplot.pdf")
