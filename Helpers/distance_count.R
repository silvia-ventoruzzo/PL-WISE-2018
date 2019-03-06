# Calculate the distance from the nearest reference point (from reference df)
# and the number of reference points within a certain distance
distance_count = function(main, reference, var_name, distance) {
    # Create variable names
    var_name_count = paste(var_name, "count", sep = "_")
    var_name_dist = paste(var_name, "dist", sep = "_")
    # Calculate distance for each listing to each station
    point_distance = geosphere::distm(x = main %>% 
                                            dplyr::select(long, lat),
                                      y = reference %>%
                                            dplyr::select(long, lat),
                                      fun = distHaversine) %>%
        as.data.frame() %>%
        data.table::setnames(as.character(reference$id))
    # Calculate how many "reference" are within "distance"
    point_distance[,var_name_count] = rowSums(point_distance <= distance)
    # Calculate the distance to the nearest "reference"
    point_distance[,var_name_dist] = apply(point_distance
                                          [,-ncol(point_distance)],
                                          MARGIN = 1,
                                          FUN = min) %>%
                                      round(0)
    # Insert this information into the main DF
    main = point_distance %>%
        dplyr::mutate(id = main$id) %>%
        dplyr::select(id, var_name_count, var_name_dist) %>%
        dplyr::right_join(main, by = "id")
    
    return(main)
}