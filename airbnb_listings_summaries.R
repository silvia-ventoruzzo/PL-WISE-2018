# Install and load needed packages
needed_packages <- c("tidyverse",
                     "rstudioapi")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located
setwd(dirname(sys.frame(1)$ofile)) # This works when sourcing
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # This works when running the code directly

# Load helper functions and other scripts
source("airbnb_listings.R", chdir = TRUE)
source(file.path(getwd(), "Helpers", "summarize_df.R", fsep="/"))
source(file.path(getwd(), "Helpers", "recode_all.R", fsep="/"))

## Summary dataframes

# For all Berlin
listings_summary <- summarize_df(df = listings,
                                 vars_mean = c("price", "review_scores_rating"),
                                 vars_count = c("property_type", "room_type",
                                                "cancellation_policy"))

# For each VBB Area
listings_vbb_summary <- summarize_df(df = listings,
                                     wrt = "vbb_area",
                                     vars_mean = c("price", "review_scores_rating"),
                                     vars_count = c("property_type", "room_type",
                                                    "cancellation_policy"))
listings_vbb_summary <- listings_vbb_summary %>%
  left_join(listings_vbb_season_availability, by = "vbb_area") %>%
  rename(id    = vbb_area) %>%
  mutate(view  = "VBB Areas",
         group = id)

# For each district
listings_district_summary <- summarize_df(df = listings,
                                          wrt = "district",
                                          vars_mean = c("price", "review_scores_rating"),
                                          vars_count = c("property_type", "room_type",
                                                         "cancellation_policy"))
listings_district_summary <- listings_district_summary %>%
  left_join(listings_district_season_availability, by = "district") %>%
  rename(id    = district) %>%
  mutate(view  = "Districts",
         group = id)

# For each neighbourhood
listings_neighbourhood_summary <- summarize_df(df = listings,
                                               wrt = "neighbourhood",
                                               vars_mean = c("price", "review_scores_rating"),
                                               vars_count = c("property_type", "room_type",
                                                              "cancellation_policy"))
listings_neighbourhood_summary <- berlin_neighbourhoods_names %>%
  dplyr::select(id, group) %>%
  dplyr::rename(neighbourhood = id) %>%
  dplyr::left_join(listings_neighbourhood_summary, by = "neighbourhood") %>%
  dplyr::left_join(listings_neighbourhood_season_availability, by = "neighbourhood") %>%
  dplyr::rename(id = neighbourhood) %>%
  dplyr::mutate(view = "Neighbourhoods") %>%
  replace(is.na(.), 0)

print("Finished producing summaries of airbnb properties.")

# Remove objects not further needed
rm("listings_vbb_season_availability", "listings_district_season_availability", "listings_neighbourhood_season_availability")
rm(list=lsf.str()) # All functions