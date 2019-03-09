# Install and load needed packages
needed_packages = c("tidyverse",
                    "rstudioapi",
                    "Jmisc",
                    "reshape2",
                    "leaflet",
                    "mapview")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located

  # This works when run directly
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  # setwd(dirname(sys.frame(1)$ofile))

# Load helper functions and other scripts
source("airbnb_listings.R", chdir = TRUE)
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

## SUMMARY STATISTICS

# Numeric variables
statistics_num = listings %>%
  dplyr::select(-id, -listing_url, -long, -lat) %>%
  dplyr::select_if(is.numeric) %>%
  descriptive_statistics()

# Categorical variables
statistics_cat = listings %>%
  dplyr::select(-id, -listing_url, -long, -lat) %>%
  dplyr::select_if(function(x) is.character(x) | is.factor(x) | is.logical(x)) %>%
  descriptive_statistics()

# Latex tables
statistics_num %>%
  filter(variable %in% c("price", "station_count")) %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)
statistics_cat %>%
  filter(variable == "room_type") %>%
  xtable::xtable() %>%
  print(include.rownames = FALSE)

## SUMMARY DATAFRAMES

# Select numerical and categorical variables
num_var = listings %>%
  dplyr::select(-id, -listing_url, -long, -lat, -vbb_zone, -district, -neighbourhood) %>%
  dplyr::select_if(is.numeric) %>%
  names()

fact_var = listings %>%
  dplyr::select(-id, -listing_url, -long, -lat, -vbb_zone, -district, -neighbourhood) %>%
  dplyr::select_if(function(x) is.character(x) | is.factor(x) | is.logical(x)) %>%
  names()

# For all Berlin
listings_summary = summarize_df(df = listings,
                                vars_mean = num_var,
                                vars_count = fact_var)

# For each VBB Zone
listings_vbb_summary = summarize_df(df = listings,
                                    wrt = "vbb_zone",
                                    vars_mean = num_var,
                                    vars_count = fact_var)

listings_vbb_summary = listings_vbb_summary %>%
  rename(id    = vbb_zone) %>%
  mutate(view  = "VBB Zones",
         group = id)

# For each district
listings_district_summary = summarize_df(df = listings,
                                         wrt = "district",
                                         vars_mean = num_var,
                                         vars_count = fact_var)
listings_district_summary = listings_district_summary %>%
  rename(id    = district) %>%
  mutate(view  = "Districts",
         group = id)

# For each neighbourhood
listings_neighbourhood_summary = summarize_df(df = listings,
                                              wrt = "neighbourhood",
                                              vars_mean = num_var,
                                              vars_count = fact_var)
listings_neighbourhood_summary = berlin_neighbourhoods_names %>%
  dplyr::select(id, group) %>%
  dplyr::left_join(listings_neighbourhood_summary, by = c("id" = "neighbourhood")) %>%
  dplyr::mutate(view = "Neighbourhoods") %>%
  replace(is.na(.), 0)

## PLOTS

# Numeric variables
numvar_plot = listings %>%
  transmute(var     = price,
            median  = median(var),
            mean    = mean(var),
            `1Q`    = quantile(var, probs = 0.25),
            `3Q`    = quantile(var, probs = 0.75),
            IQR     = IQR(var),
            upper   = (IQR*1.5) + `3Q`,
            lower   = `1Q` - (IQR*1.5),
            count   = sum(var > unique(upper) | var < unique(lower)),
            outlier = ifelse(var > unique(upper) | var < unique(lower), TRUE, FALSE))

# Complete distribution
numvar_plot %>%
  ggplot() +
  geom_density(aes(x = var), fill = "green") +
  geom_vline(aes(xintercept = unique(median)),
             color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = unique(mean)),
             color = "black", linetype = "dashed") +
  geom_vline(aes(xintercept = `1Q`),
             color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = `3Q`),
             color = "red", linetype = "dashed") +
  labs(x = "price") +
  theme_classic() +
  theme(axis.text.x = element_text(size = rel(1.5)))

dev.copy2pdf(file = "./SeminarPaper/price_distribution_complete.pdf")
dev.off()

# Complete distribution
numvar_plot %>%
  dplyr::filter(outlier == FALSE) %>%
  ggplot() +
  geom_density(aes(x = var), fill = "green") +
  geom_vline(aes(xintercept = unique(median)),
             color = "blue", linetype = "dashed") +
  annotate(geom = "text", x = (unique(numvar_plot$median) + 2.6), y = 0,
           label = "Median",
           colour = "blue",
           angle = 90, hjust = 0) +
  geom_vline(aes(xintercept = unique(mean)),
             color = "black", linetype = "dashed") +
  annotate(geom = "text", x = (unique(numvar_plot$mean) + 2.6), y = 0,
           label = "Mean",
           colour = "black",
           angle = 90, hjust = 0) +
  geom_vline(aes(xintercept = `1Q`),
             color = "red", linetype = "dashed") +
  annotate(geom = "text", x = (unique(numvar_plot$`1Q`) + 2.6), y = 0,
           label = "1st Quantile",
           colour = "red",
           angle = 90, hjust = 0) +
  geom_vline(aes(xintercept = `3Q`),
             color = "red", linetype = "dashed") +
  annotate(geom = "text", x = (unique(numvar_plot$`3Q`) + 2.6), y = 0,
           label = "3rd Quantile",
           colour = "red",
           angle = 90, hjust = 0) +
  labs(caption = paste(unique(numvar_plot$count), "outlier values not displayed", sep = " "),
       x = "price") +
  theme_classic() +
  theme(axis.text.x = element_text(size = rel(1.5)))

dev.copy2pdf(file = "./SeminarPaper/price_distribution_nooutliers.pdf")
dev.off()

# Plot factor variables
if (interactive()) {
  
  factvar_plot = listings %>%
    dplyr::rename(var = room_type) %>%
    dplyr::group_by(var) %>%
    dplyr::summarize(count = n()) 
  
  ggplot(factvar_plot) +
    geom_bar(aes(x = var, y = count), fill = "green", stat = "identity") +
    labs(x = "room_type",
         title = "Histogram of the variable room_type") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)))
  
  dev.copy2pdf(file = "./SeminarPaper/room_type_distribution.pdf")
  dev.off()
  
}

# Maps of variable distribution
if (interactive()) {
  
  colors = colorBin(topo.colors(5), listings_district_summary$price, bins = 5, pretty = FALSE)
  
  price_map_distr = leaflet() %>% 
    addTiles() %>%
    addPolygons(data = berlin_district_sf, weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                color = "black", 
                fillColor = ~colors(listings_district_summary$price)) %>%
    addLabelOnlyMarkers(data = berlin_districts_names,
                        lng = ~long, lat = ~lat, label = ~lapply(name, htmltools::HTML),
                        labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                    textOnly = TRUE, textsize = 12,
                                                    style = list("color" = "black"))) %>%
    addLegend(position = "bottomleft", pal = colors, 
              values = listings_district_summary$price,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
              title = "Price in districts")
  
  price_map_distr
  mapview::mapshot(price_map_distr, file = "./SeminarPaper/price_map_distr.pdf",
                   remove_controls = c("zoomControl", "layersControl", "homeButton","scaleBar"))
  
}

print("Exploratory Data Analysis done.")


# Remove unnecessary objects
rm("listings_calendar", "listings_season_availability", "stations", "berlin_neighbourhood_sf",
   "numvar_plot", "factvar_plot", "listings_detailed", "listings_summarized",
   "colors", "price_map_distr")
rm(list=lsf.str()) # All functions