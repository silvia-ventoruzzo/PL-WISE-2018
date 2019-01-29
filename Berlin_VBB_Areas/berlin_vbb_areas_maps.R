# Load packages
needed_packages <- c("leaflet", "ggplot2")
for (package in needed_packages) {
  if (package %in% rownames(installed.packages()))
    install.packages(package)
  do.call("library", list(package))
}
rm("needed_packages")

# Load file where the sf object is created
source(file.path(getwd(), "Berlin_VBB_Areas", "berlin_vbb_areas.R", fsep="/"))

# Leaflet map
berlin_vbb_areas_leaflet <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = berlin_vbb_AB_sf,
              weight = 1, smoothFactor = 1, fillOpacity = 0.8,
              fillColor = c("cadetblue", "darkorange"), color = "black") %>%
  addLabelOnlyMarkers(data = berlin_vbb_AB_names,
                      lng = ~long, lat = ~lat, label = ~id,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                  textOnly = TRUE, textsize = "20px"))
berlin_vbb_areas_leaflet
library(mapview)
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
