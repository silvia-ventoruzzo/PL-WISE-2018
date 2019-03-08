# Install and load needed packages
needed_packages = c("tidyverse",
                     "rstudioapi",
                    "leaflet")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located

  # This works when run directly
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  setwd(dirname(sys.frame(1)$ofile))

# Load helper functions and other scripts
source("airbnb_listings.R", chdir = TRUE)
source(file.path(getwd(), "Helpers", "summarize_df.R", fsep="/"))
source(file.path(getwd(), "Helpers", "recode_all.R", fsep="/"))

## Summary dataframes

# For all Berlin
listings_summary = summarize_df(df = listings,
                                 vars_mean = c("price", "review_scores_rating"),
                                 vars_count = c("property_type", "room_type",
                                                "cancellation_policy"))

# For each VBB Zone
listings_vbb_summary = summarize_df(df = listings,
                                     wrt = "vbb_zone",
                                     vars_mean = c("price", "review_scores_rating"),
                                     vars_count = c("property_type", "room_type",
                                                    "cancellation_policy"))
listings_vbb_summary = listings_vbb_summary %>%
  left_join(listings_vbb_season_availability, by = "vbb_zone") %>%
  rename(id    = vbb_zone) %>%
  mutate(view  = "VBB Zones",
         group = id)

# For each district
listings_district_summary = summarize_df(df = listings,
                                          wrt = "district",
                                          vars_mean = c("price", "review_scores_rating"),
                                          vars_count = c("property_type", "room_type",
                                                         "cancellation_policy"))
listings_district_summary = listings_district_summary %>%
  left_join(listings_district_season_availability, by = "district") %>%
  rename(id    = district) %>%
  mutate(view  = "Districts",
         group = id)

# For each neighbourhood
listings_neighbourhood_summary = summarize_df(df = listings,
                                               wrt = "neighbourhood",
                                               vars_mean = c("price", "review_scores_rating"),
                                               vars_count = c("property_type", "room_type",
                                                              "cancellation_policy"))
listings_neighbourhood_summary = berlin_neighbourhoods_names %>%
  dplyr::select(id, group) %>%
  dplyr::rename(neighbourhood = id) %>%
  dplyr::left_join(listings_neighbourhood_summary, by = "neighbourhood") %>%
  dplyr::left_join(listings_neighbourhood_season_availability, by = "neighbourhood") %>%
  dplyr::rename(id = neighbourhood) %>%
  dplyr::mutate(view = "Neighbourhoods") %>%
  replace(is.na(.), 0)

# Plot 
if (interactive()) {
  
  colors = colorBin(topo.colors(5), listings_district_summary$price, bins = 5, pretty = FALSE)
  
  colors = colorQuantile(topo.colors(5), listings_district_summary$price, n = 4)
  
  leaflet() %>% 
    addTiles() %>%
    addPolygons(data = berlin_district_sf, weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                color = "black", 
                fillColor = ~colors(listings_district_summary$price)) %>%
    addLabelOnlyMarkers(data = berlin_districts_names,
                        lng = ~long, lat = ~lat, label = ~lapply(name, htmltools::HTML),
                        labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                    textOnly = TRUE, textsize = 12)) %>%
    addLegend(position = "bottomleft", pal = colors, 
              values = listings_district_summary$price,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
              title = "Price in districts")
  
  dev.copy2pdf(file = "./SeminarPaper/map_price_averages.pdf")
  
}

print("Finished producing summaries of airbnb properties.")

# Remove objects not further needed
rm("listings_vbb_season_availability", "listings_district_season_availability",
   "listings_neighbourhood_season_availability", "colors")
rm(list=lsf.str()) # All functions