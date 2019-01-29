# Calculate midpoint from a sf point object
points_midpoint <- function(points_sf, point_name = id) {
  
  # Get coordinates from sf point objects
  coordinates <- st_coordinates(points_sf) %>%
    base::as.data.frame() %>%
    dplyr::rename(lat  = Y,
                  long = X)
  
  # Bind coordinates with sf point object and calculate mean of lat and long
  midpoints_df <- points_sf %>%
    base::as.data.frame() %>%
    dplyr::select(-geometry) %>%
    base::cbind(coordinates) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(lat  = mean(lat),
              long = mean(long))
  
  return(midpoints_df)
  
}