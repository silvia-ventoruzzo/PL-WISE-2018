rm(list=ls(all = TRUE))

# Install and load needed packages
needed_packages = c("tidyverse",
                    "rstudioapi",
                    "Jmisc",
                    "reshape2",
                    "leaflet",
                    "mapview",
                    "xtable")
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
source("data_preparation.R", chdir = TRUE)
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
    dplyr::select_if(function(x) is.character(x) | is.factor(x) 
                                                 | is.logical(x)) %>%
    descriptive_statistics()

# Latex tables
# statistics_num %>%
#     filter(variable %in% c("price", "review_scores_rating")) %>%
#     xtable::xtable() %>%
#     print(include.rownames = FALSE)
# statistics_cat %>%
#     filter(variable == "room_type") %>%
#     xtable::xtable() %>%
#     print(include.rownames = FALSE)

## SUMMARY DATAFRAMES

# Select numerical and categorical variables
num_var = listings %>%
    dplyr::select(-id, -listing_url, -long, -lat, 
                  -vbb_zone, -district, -neighbourhood) %>%
    dplyr::select_if(is.numeric) %>%
    names()

fact_var = listings %>%
    dplyr::select(-id, -listing_url, -long, -lat, 
                  -vbb_zone, -district, -neighbourhood) %>%
    dplyr::select_if(function(x) is.character(x) | is.factor(x) 
                                                 | is.logical(x)) %>%
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
    dplyr::left_join(listings_neighbourhood_summary, 
                     by = c("id" = "neighbourhood")) %>%
    dplyr::mutate(view = "Neighbourhoods") %>%
    replace(is.na(.), 0)

## PLOTS

# Numeric variables

# Complete distribution
distribution_plot(df = listings, var_name = "price")

# dev.copy2pdf(file = "./SeminarPaper/price_distribution_complete.pdf")
# dev.off()

# Without outliers
distribution_plot(df = listings, var_name = "price", hide_outliers = TRUE)

# dev.copy2pdf(file = "./SeminarPaper/price_distribution_nooutliers.pdf")
# dev.off()

# Plot factor variables
distribution_plot(df = listings, var_name = "station_count")
  
# dev.copy2pdf(file = "./SeminarPaper/room_type_distribution.pdf")
# dev.off()

# Maps of variable distribution
price_map_distr = var_avg_map(summary_df        = listings_district_summary,
                              var_name          = "price",
                              polygon_sf        = berlin_district_sf, 
                              polygon_names_df  = berlin_districts_names, 
                              polygon_names_var = "name")
price_map_distr

# mapview::mapshot(price_map_distr, file = "./SeminarPaper/price_map_distr.pdf",
#                 remove_controls = c("zoomControl", "layersControl", "homeButton","scaleBar"))

print("Exploratory Data Analysis done.")

# Remove unnecessary objects
rm("price_map_distr", "fact_var", "num_var")
rm(list=lsf.str()) # All functions