# Tells in which polygon a point is
point_in_polygons <- function(points_df, polys_sf, var_name)  {
  # Create empty dataframe
  is_in <- data.frame(matrix(ncol = nrow(polys_sf), nrow = nrow(points_df)))
  is_in[,var_name] <- NA
  # Extract coordinates of the polygons
  coordinates <- as.data.frame(st_coordinates(polys_sf))
  # Extract names of the polygons
  name <- polys_sf$id
  # For all polygons check if the points are inside of them
  for (k in 1:nrow(polys_sf)) {
    is_in[,k] <- sp::point.in.polygon(point.x = points_df$long,
                                      point.y = points_df$lat,
                                      pol.x   = coordinates$X[coordinates$L2 == k],
                                      pol.y   = coordinates$Y[coordinates$L2 == k])
    # Get the names of the polygons where the points are in one column
    is_in[,var_name][is_in[,k] == 1] <- name[k]
  }
  # Keep only summary column and add points' names
  is_in <- is_in %>%
    dplyr::select(var_name) %>%
    dplyr::mutate(id = points_df$id)
  # Add the summary column to the points dataframe
  points_df <- dplyr::full_join(points_df, is_in, by = "id")
  return(points_df)
} 