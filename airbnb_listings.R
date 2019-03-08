# Install and load needed packages
needed_packages = c("tidyverse",
                     "geosphere",
                     "readr",
                     "rstudioapi",
                     "Jmisc")
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
source(file.path(getwd(), "Berlin_VBB_Zones", "berlin_vbb_zones.R", fsep="/"), chdir = TRUE)
source(file.path(getwd(), "Berlin_Districts_Neighbourhoods", "berlin_districts_neighbourhoods.R", fsep="/"), chdir = TRUE)
Jmisc::sourceAll(file.path(getwd(), "Helpers", fsep="/"))

# Load shapefile
stations = sf::st_read(file.path(getwd(), "Data", "spatial_data", "gis_osm_transport_free_1.shp", fsep="/"))

# Print code development
print("Spatial data loaded.")


# Load dataframes
# If files are zipped
# This is the case, because the files are too big to be uploaded to GitHub otherwise
listings_summarized = read_csv(file.path(getwd(), "Data", "airbnb_data", "07.11_listings_summary.csv.zip", fsep="/"),
                                na = c("NA", ""), locale = locale(encoding = "UTF-8"))
listings_detailed = read_csv(file.path(getwd(), "Data", "airbnb_data", "07.11_listings_detailed.csv.zip", fsep="/"),
                              na = c("NA", ""), locale = locale(encoding = "UTF-8"))
listings_calendar = read_csv(file.path(getwd(), "Data", "airbnb_data", "07.11_listings_calendar.csv.zip", fsep="/"),
                              na = c("NA", ""), locale = locale(encoding = "UTF-8"))

# Print code development
print("Airbnb datasets uploaded.")

# Join dataframes, first clean and keep only variables of interest
listings = df_join_clean(df1 = listings_detailed, df2 = listings_summarized)

# Check if there are missing values
if (!interactive()) {
  apply(listings, 2, function(x) any(is.na(x)))
}

# host_is_superhost
# According to airbnb: https://www.airbnb.com/help/article/828
if (!interactive()) {
  listings %>%
    dplyr::filter(is.na(host_is_superhost)) # 26
}

listings = listings %>%
  dplyr::mutate(host_is_superhost = ifelse(is.na(host_is_superhost), 0, host_is_superhost))

# review_scores_rating
if (!interactive()) {
  listings %>%
    dplyr::filter(is.na(review_scores_rating)) %>%
    dplyr::select(id, review_scores_rating, number_of_reviews) # 4,389
}
# If the number_of_reviews is zero, we set the review_scores_value to 0 meanwhile creating a variable to keep track that the listing has not been reviewed yet
listings = listings %>%
  dplyr::mutate(review_scores_rating = ifelse(number_of_reviews == 0, 0, review_scores_rating),
                reviewed_yn = ifelse(number_of_reviews == 0, 0, 1))
# For the rest, we insert the average of the review_scores_rating
if (!interactive()) {
  listings %>%
    dplyr::filter(is.na(review_scores_rating)) %>%
    dplyr::select(id, review_scores_accuracy, review_scores_value,
                  review_scores_cleanliness, review_scores_checkin, review_scores_communication,
                  review_scores_location, review_scores_rating,
                  number_of_reviews) # 499
  listings %>% 
    dplyr::summarize(mean = mean(review_scores_rating, na.rm = TRUE),
                    median = median(review_scores_rating, na.rm = TRUE),
                    mode = getmode(review_scores_rating)) %>%
    round(0)
}
# mean might be influenced by outliers, being it so different from median and mode, and mode is too high, so I will substitute the missing values with the median
listings = listings %>%
  mutate(review_scores_rating = ifelse(is.na(review_scores_rating), 
                                       median(review_scores_rating, na.rm = TRUE),
                                       review_scores_rating))

# bedrooms and beds
if (!interactive()) {
  listings %>%
  dplyr::filter(is.na(bedrooms) | 
                is.na(beds)) %>%
  dplyr::select(id, accommodates, bedrooms, beds) # 57
}
# Being the amount of missing values in these features relative small, we can derive their value from the other variables:
listings = listings %>%
  # If beds is NA, but bedrooms has a valid value, we set beds with the number of bedrooms
  dplyr::mutate(beds = ifelse(is.na(beds) & !is.na(bedrooms), bedrooms, beds),
                # If bedrooms is NA, but beds has a valid value, we set bedrooms with the number of beds
                bedrooms = ifelse(is.na(bedrooms) & !is.na(beds), beds, bedrooms),
                # If both beds and bedrooms are NA: 1
                # Since in this case the property can accomodate only one person, we assume that it has 1 bed and 1                             bedroom (even if it might not be separate)
                beds = ifelse(is.na(beds) & is.na(bedrooms), 1, beds),
                bedrooms = ifelse(beds == 1 & is.na(bedrooms), 1, bedrooms))

listings = listings %>%
  dplyr::rename(price = price.x) %>%
  # Keep only columns of interest
  dplyr::select(id, long, lat, price, property_type, room_type,
                security_deposit_yn, cleaning_fee_yn,
                host_is_superhost, accommodates, bedrooms, beds, minimum_nights, 
                review_scores_rating, number_of_reviews, reviewed_yn, cancellation_policy,
                availability_30, availability_60, availability_90, availability_365,
                listing_url)

# Check if there are missing values
if (!interactive()) {
  apply(listings, 2, function(x) any(is.na(x)))
}

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
  id   = c("Reichstag", "Brandenburger Tor", "Fernsehturm", "Gendarmenmarkt",
           "Berliner Dom", "Kurfürstendamm", "Schloss Charlottenburg", "Museuminsel",
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
  rename(id = listing_id) %>%
  mutate(date        = as.Date(date),
         # if loading the csv use recode, if loading the zip there is no need
         # available   = recode(available,
         #                    "f" = 0,
         #                    "t" = 1),
         year        = lubridate::year(date),
         month       = lubridate::month(date, label = TRUE),
         day         = lubridate::day(date),
         season      = ifelse((month == "Mar" & day >= 21) | (month == "Apr") | 
                              (month == "May") | (month == "Jun" & day < 21), "Spring",
                       ifelse((month == "Jun" & day >= 21) | (month == "Jul") | 
                              (month == "Aug") | (month == "Sep" & day < 21), "Summer",
                       ifelse((month == "Sep" & day >= 21) | (month == "Oct") | 
                              (month == "Nov") | (month == "Dec" & day < 21), "Fall",
                                                                              "Winter"))) %>%
           factor(levels = c("Spring", "Summer", "Fall", "Winter")),
         year_season = paste(year, tolower(season), sep = "_") %>%
           factor(levels = c("2018_fall", "2018_winter", "2019_spring",
                             "2019_summer", "2019_fall", "2019_winter"))) %>%
  dplyr::select(-price)

# Availability per year_season
listings_season_availability = listings_calendar %>% 
  dplyr::group_by(id, year_season) %>% 
  # if loading the csv use sum(available)
  # dplyr::summarize(count = sum(available)) %>% 
  # if loading the zip use sum(available == TRUE)
  dplyr::summarize(count = sum(available == TRUE)) %>%   
  arrange(id, year_season) %>%
  ungroup() %>%
  mutate(season_av =  paste(year_season, "availability", sep = "_") %>%
           tolower() %>%
           factor(levels = unique(tolower(paste(year_season, "availability", sep = "_"))))) %>%
  dplyr::select(-year_season) 

listings_district_season_availability = listings %>%
  select(id, district) %>%
  inner_join(listings_season_availability, by = "id") %>%
  group_by(district, season_av) %>%
  dplyr::summarize(count = mean(count) %>% round(0)) %>%
  spread(season_av, count)

listings_vbb_season_availability = listings %>%
  select(id, vbb_zone) %>%
  inner_join(listings_season_availability, by = "id") %>%
  group_by(vbb_zone, season_av) %>%
  dplyr::summarize(count = mean(count) %>% round(0)) %>%
  spread(season_av, count)

listings_neighbourhood_season_availability = listings %>%
  select(id, neighbourhood) %>%
  inner_join(listings_season_availability, by = "id") %>%
  group_by(neighbourhood, season_av) %>%
  dplyr::summarize(count = mean(count) %>% round(0)) %>%
  spread(season_av, count)

listings_season_availability = listings_season_availability %>%
  spread(season_av, count)

# Print code development
print("New variables created.")

# Join to the listings
listings = listings %>%
  left_join(listings_season_availability, by = "id") %>%
  replace(is.na(.), 0)

# Plot numeric variables
if (interactive()) {
  
  numvar_plot = listings %>%
    transmute(var    = price,
              median = median(price),
              mean   = mean(price),
              `1Q`   = quantile(price, probs = 0.25),
              `3Q`   = quantile(price, probs = 0.75),
              `95P`  = quantile(price, probs = 0.95),
              count  = sum(price > unique(`95P`)))
  
  numvar_plot %>%
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
    scale_x_continuous(limits = c(min(numvar_plot$var), unique(numvar_plot$`95P`))) +
    labs(caption = paste("Left limit set to 95% percentile:", unique(numvar_plot$count), "values not displayed", sep = " "),
         title = "Distribution of the variable price",
         subtitle = "1st, 2nd, 3rd Quantiles and Mean also displayed",
         x = "price") +
    theme_bw() +
    theme(axis.text.x = element_text(size = rel(1.5)))
  
  dev.copy2pdf(file = "./SeminarPaper/price_distribution.pdf")
  
}

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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)))
  
  dev.copy2pdf(file = "./SeminarPaper/room_type_distribution.pdf")
  
}

# Remove unnecessary objects
rm("listings_calendar", "listings_season_availability", "stations", "berlin_neighbourhood_sf",
   "numvar_plot", "factvar_plot", "listings_detailed", "listings_summarized")
rm(list=lsf.str()) # All functions
