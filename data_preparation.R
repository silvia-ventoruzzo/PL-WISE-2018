# Install and load needed packages
needed_packages = c("tidyverse",
                     "geosphere",
                     "readr",
                     "rstudioapi",
                     "Jmisc",
                     "sp",
                     "sf")
for (package in needed_packages) {
  if (!require(package, character.only=TRUE)) {
    install.packages(package, character.only=TRUE)}
  library(package, character.only=TRUE)
}
rm("needed_packages", "package")

# Set working directory to the one where the file is located

  # This works when run directly
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  
  # This works when sourced
  setwd(dirname(sys.frame(1)$ofile))

# Load helper functions and other scripts
source(file.path(getwd(), "SPL_Berlin_VBB_Zones", 
                 "berlin_vbb_zones.R", fsep="/"), chdir = TRUE)
source(file.path(getwd(), "SPL_Berlin_Districts_Neighbourhoods", 
                 "berlin_districts_neighbourhoods.R", fsep="/"), chdir = TRUE)
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# Load shapefile
stations = sf::st_read(file.path(getwd(), "Data", "spatial_data", 
                                 "gis_osm_transport_free_1.shp", fsep="/"))

# Print code development
print("Spatial data loaded.")


# Load dataframes
listings_summarized = read_csv(file.path(getwd(), "Data", "airbnb_data", 
                                         "listings.csv", fsep="/"),
                                na = c("NA", ""), 
                               locale = locale(encoding = "UTF-8"))
listings_detailed = read_csv(file.path(getwd(), "Data", "airbnb_data", 
                                       "listings.csv.gz", fsep="/"),
                              na = c("NA", ""), 
                             locale = locale(encoding = "UTF-8"))
listings_calendar = read_csv(file.path(getwd(), "Data", "airbnb_data", 
                                       "calendar.csv.gz", fsep="/"),
                              na = c("NA", ""), 
                             locale = locale(encoding = "UTF-8"))

# Print code development
print("Airbnb datasets uploaded.")

# Join dataframes, first clean and keep only variables of interest
listings = df_join_clean(df1 = listings_detailed, df2 = listings_summarized)

# Check if there are missing values
apply(listings, 2, function(x) any(is.na(x)))

# host_is_superhost, # host_has_profile_pic, host_identity_verified
listings = listings %>%
    dplyr::mutate(host_is_superhost      = ifelse(is.na(host_is_superhost), 
                                                  FALSE, host_is_superhost),
                  host_has_profile_pic   = ifelse(is.na(host_has_profile_pic), 
                                                  FALSE, host_has_profile_pic),
                  host_identity_verified = ifelse(is.na(host_identity_verified), 
                                                  FALSE, host_identity_verified))

# review_scores_rating
# We insert the average of the review_scores_rating
listings %>% 
    dplyr::summarize(mean = mean(review_scores_rating, na.rm = TRUE), # 77
                    median = median(review_scores_rating, na.rm = TRUE), # 95
                    mode = getmode(review_scores_rating)) %>% # 100
    round(0)
# mean might be influenced by outliers, being it so different from median and mode, and mode is too high, so I will substitute the missing values with the median
listings = listings %>%
    dplyr::mutate(review_scores_rating = ifelse(is.na(review_scores_rating), 
                                                median(review_scores_rating, na.rm = TRUE),
                                                review_scores_rating))
# bedrooms and beds
# Being the amount of missing values in these features relative small, we can derive their value from the other variables:
listings = listings %>%
  # If beds is NA, but bedrooms has a valid value, we set beds with the number of bedrooms
  dplyr::mutate(beds     = ifelse(is.na(beds) & !is.na(bedrooms), 
                                  bedrooms, beds),
                # If bedrooms is NA, but beds has a valid value, we set bedrooms with the number of beds
                bedrooms = ifelse(is.na(bedrooms) & !is.na(beds), beds, bedrooms),
                # If both beds and bedrooms are NA: 1
                # Since in this case the property can accomodate only one person, we assume that it has 1 bed and 1                             bedroom (even if it might not be separate)
                beds     = ifelse(is.na(beds) & is.na(bedrooms), 1, beds),
                bedrooms = ifelse(beds == 1 & is.na(bedrooms), 1, bedrooms))

# Check if there are missing values
apply(listings, 2, function(x) any(is.na(x)))

# Listings selection
# Keep only listings with at least one customer review, as per Wang and Nicolau (2017)
listings %>%
  dplyr::filter(number_of_reviews == 0) %>%
  dplyr::summarize(count = n())
listings = listings %>%
  dplyr::filter(number_of_reviews > 0)

# Print code development
print("Missing values cleaned.")

## New variables

# Areas: Not doing this with berlin_sf because the polys_sf need to have geometry sfc_POLYGON
# district
listings = point_in_polygons(points_df = listings,
                              polys_sf  = berlin_district_sf,
                              var_name  = "district")
# vbb_zone
listings = point_in_polygons(points_df = listings,
                              polys_sf  = berlin_vbb_AB_sf,
                              var_name  = "vbb_zone")
# neighbourhood
listings = point_in_polygons(points_df = listings,
                              polys_sf = berlin_neighbourhood_sf,
                              var_name = "neighbourhood")
# For the sake of consistency, we will delete these listings
listings = listings %>%
  filter(!is.na(district) & !is.na(vbb_zone))

# Stations
railway_stations_df = stations %>%
  dplyr::filter(fclass %like% "railway") %>%
  dplyr::rename(id     = name) %>%
  dplyr::mutate(id     = gsub("Berlin ", "", id),
                id     = gsub("Berlin-", "", id),
                id     = gsub(" *\\(.*?\\) *", "", id),
                s_bahn = ifelse(startsWith(id, "S "), TRUE, FALSE),
                u_bahn = ifelse(startsWith(id, "U "), TRUE, FALSE),
                id     = gsub("S ", "", id),
                id     = gsub("U ", "", id)) %>%
  points_midpoint()
railway_stations_df = point_in_polygons(points_df = railway_stations_df,
                                         polys_sf  = berlin_district_sf,
                                         var_name  = "district")
railway_stations_df = railway_stations_df %>%
  filter(!is.na(district))
listings = distance_count(main = listings, reference = railway_stations_df,
                           var_name = "station", distance = 1000)

# (Top 10) attractions
attractions_df = data.frame(
  id   = c("Reichstag", "Brandenburger Tor", "Fernsehturm", 
           "Gendarmenmarkt", "Berliner Dom", "Kurfürstendamm", 
           "Schloss Charlottenburg", "Museuminsel",
           "Gedenkstätte Berliner Mauer", "Potsdamer Platz"),
  lat  = c(52.518611, 52.516389, 52.520803, 52.513333, 52.519167, 
           52.500833, 52.521111, 52.520556, 52.535, 52.509444),
  long = c(13.376111, 13.377778, 13.40945, 13.393056, 13.401111,
           13.312778, 13.295833, 13.397222, 13.389722, 13.375833))
attractions_df = point_in_polygons(points_df = attractions_df,
                                    polys_sf  = berlin_district_sf,
                                    var_name  = "district")
listings = distance_count(main = listings, reference = attractions_df,
                           var_name = "attraction", distance = 2000)

# season availability
listings_calendar = listings_calendar %>%
  dplyr::rename(id = listing_id) %>%
  dplyr::mutate(date        = as.Date(date),
                year        = lubridate::year(date),
                month       = lubridate::month(date, label = TRUE),
                day         = lubridate::day(date),
                season      = ifelse((month == "Mar" & day >= 21) | 
                                      (month == "Apr") | 
                                      (month == "May") |
                                      (month == "Jun" & day < 21), "Spring",
                              ifelse((month == "Jun" & day >= 21) | 
                                     (month == "Jul") | 
                                    (month == "Aug") | 
                                      (month == "Sep" & day < 21), "Summer",
                              ifelse((month == "Sep" & day >= 21) | 
                                     (month == "Oct") | 
                                     (month == "Nov") | 
                                     (month == "Dec" & day < 21), "Fall",
                                                            "Winter"))) %>%
                                  factor(levels = c("Spring", "Summer", 
                                                    "Fall", "Winter")),
                year_season = paste(tolower(season), year, sep = "_")) %>%
  dplyr::ungroup() %>%
  dplyr::select(-price)

# Availability per year_season
listings_season_availability = listings_calendar %>% 
  dplyr::group_by(id, year_season) %>% 
  dplyr::summarize(count       = sum(available == TRUE),
                   season_days = n(),
                   proportion  = round(count/season_days*100, 4)) %>%   
  dplyr::arrange(id, year_season) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(season_av =  paste(year_season, "availability", 
                                   sep = "_") %>%
                             tolower() %>%
                             factor(levels = paste(year_season, 
                                                   "availability", 
                                                   sep = "_") %>%
                                             tolower() %>%
                                             unique())) %>%
  dplyr::select(-count, -year_season, -season_days) %>%
  tidyr::spread(season_av, proportion)

# Print code development
print("New variables created.")

# Join to the listings
listings = listings %>%
  left_join(listings_season_availability, by = "id") %>%
  replace(is.na(.), 0)

# Remove unnecessary objects
rm("listings_calendar", "listings_season_availability", "stations", 
   "berlin_neighbourhood_sf", "listings_detailed", "listings_summarized")
rm(list=lsf.str()) # All functions
