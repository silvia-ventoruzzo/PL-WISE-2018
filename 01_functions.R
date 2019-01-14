## Function to load files
load_listings <- function(file) {
  wd <- getwd()
  setwd(file.path(getwd(), "insideairbnb", fsep="/"))
  df <- utils::read.csv(file, sep = ",", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8",
                        na.strings = c("NA", ""))
  df <- df %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(df)), funs(gsub("\\$", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(df)), funs(gsub("\\,", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(df)), funs(as.numeric(.)))
  setwd(wd)
  return(df)
}



## Join dataframes and cleaning
df_join_clean <- function(df1, df2) {
  df_joined <- full_join(df1, df2, by = c("id", "host_id", "room_type", "price",
                                          "minimum_nights",
                                          "number_of_reviews", 
                                          "latitude", "longitude",
                                          "availability_365", "neighbourhood_cleansed" = "neighbourhood")) %>% 
    rename(lat = latitude,
           long = longitude) %>%
    # neighbourhood_group
    mutate(neighbourhood_group = gsub("\\ ", "", neighbourhood_group),
           neighbourhood_group = as.factor(ifelse(neighbourhood_group ==
                                                                     "Charlottenburg-Wilm.",
                                                                   "Charlottenburg-Wilmersdorf",
                                                                   neighbourhood_group))) %>%
    # property_type
    group_by(property_type) %>% 
    mutate(property_type_count=n()) %>% 
    ungroup() %>%
    mutate(property_type = as.factor(ifelse(property_type_count <= 10,
                                                             "Other", property_type))) %>%
    # security_deposit
    mutate(security_deposit_yn = ifelse(security_deposit == 0 |
                                                    is.na(security_deposit), 0, 1),
    # cleaning_fee
      cleaning_fee_yn = ifelse(cleaning_fee == 0 |
                                                is.na(cleaning_fee), 0, 1),
    # host_is_superhost
      host_is_superhost = ifelse(host_is_superhost == "t", 0, 1))
  # %>%
  #   # cancellation_policy
  #   mutate(cancellation_policy = 
  #            recode(cancellation_policy,
  #                     "strict_14_with_grace_period" = "strict",
  #                     "super_strict_30" = "very_strict",
  #                     "super_strict_60" = "super_strict"))

  return(df_joined)
}


## Tells in which polygon (area) a point (listing) is
point_in_polygons <- function(points_df, polys_sf, var_name)  {
  is_in <- data.frame(matrix(ncol = nrow(polys_sf), nrow = nrow(points_df)))
  is_in[,var_name] <- NA
  coordinates <- as.data.frame(st_coordinates(polys_sf))
  name <- unique(polys_sf$id)
  for (k in 1:nrow(polys_sf)) {
    is_in[,k] <- point.in.polygon(point.x = points_df$long,
                                  point.y = points_df$lat,
                                  pol.x = coordinates$X[coordinates$L2 == k],
                                  pol.y = coordinates$Y[coordinates$L2 == k])
    is_in[,var_name][is_in[,k] == 1] <- name[k]
  }
  is_in <- is_in %>%
    dplyr::select(var_name) %>%
    mutate(id = points_df$id)
  points_df <- full_join(points_df, is_in, by = "id")
  return(points_df)
} 


## Calculate the distance from the nearest reference point (from reference df)
## and the number of reference points within a certain distance
distance_count <- function(main, reference, var_name, distance) {
  # Create variable names
  var_name_count <- paste(var_name, "count", sep = "_")
  var_name_dist <- paste(var_name, "dist", sep = "_")
  # Calculate distance for each listing to each station
  point_distance <- geosphere::distm(main %>% dplyr::select(long, lat),
                                reference %>% dplyr::select(long, lat),
                                fun = distHaversine) %>%
    as.data.frame() %>%
    data.table::setnames(as.character(reference$id))
  # Calculate how many "reference" are within "distance"
  point_distance[,var_name_count] <- rowSums(point_distance <= distance)
  # Calculate the distance to the nearest "reference"
  point_distance[,var_name_dist] <- apply(point_distance[,-ncol(point_distance)],
                                          MARGIN = 1, FUN = min) %>%
    round(0)
  # Insert this information into the main DF
  main <- point_distance %>%
    mutate(id = main$id) %>%
    dplyr::select(id, var_name_count, var_name_dist) %>%
    right_join(main, by = "id")
  return(main)
}

## Summary information for numeric and factor variables
# summarize_df <- function(df, wrt = NULL, vars_mean, vars_count) {
#   # Create empty vector
#   summary_df <- c()
#   # Variables of which we calculate the mean
#   for (var in vars_mean) {
#     var_name <- sym(var)
#     if(is.null(wrt)) {
#       mean_df <- df %>% summarize(!!var_name := mean(!!var_name))
#       summary_df <- c(summary_df,mean_df)
#     } else {
#       wrt_name <- sym(wrt)
#       mean_df <- df %>%
#         group_by(!!wrt_name) %>% 
#         summarize(!!var_name := mean(!!var_name))
#       summary_df <- c(summary_df,mean_df)
#       summary_df[duplicated(summary_df)] <- NULL
#     }
#   }
#   summary_df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
#   # Variables of which we count the unique values of the factors
#   for (var in vars_count) {
#     var_name <- sym(var)
#     if(is.null(wrt)) {
#       count_df <- df %>%
#         recode_all(var) %>%
#         group_by(!!var_name) %>% 
#         dplyr::summarize(count = n()) %>%
#         spread(key = !!var_name, value = count)    
#       summary_df <- summary_df %>% cbind(count_df)
#     } else {
#       wrt_name <- sym(wrt)
#       count_df <- df %>%
#         recode_all(var) %>%
#         group_by(!!var_name, !!wrt_name) %>%
#         dplyr::summarize(count = n()) %>%
#         spread(key = !!var_name, value = count)  
#       summary_df <- summary_df %>% full_join(count_df, by = wrt) %>%
#         replace(is.na(.), 0)
#     }
#   }
#   return(summary_df)
# }
summarize_df <- function(df, wrt = NULL, vars_mean, vars_count) {
  # Create empty vector
  summary_df <- c()
  # For the case in which the summary is general
  if(is.null(wrt)) {
    # Variables of which we calculate the mean
    for (var in vars_mean) {
      var_name <- sym(var)
      mean_df <- df %>% summarize(!!var_name := mean(!!var_name))
      summary_df <- c(summary_df,mean_df)
    }
    # Variables of which we count the unique values of the factors
    for (var in vars_count) {
      var_name <- sym(var)
      count_df <- df %>%
      recode_all(var) %>%
      group_by(!!var_name) %>% 
      dplyr::summarize(count = n()) %>%
      spread(key = !!var_name, value = count)    
      summary_df <- summary_df %>% cbind(count_df)
    }
  # For the case in which the summary is done wrt a variable
  } else {
    wrt_name <- sym(wrt)
    # Variables of which we calculate the mean
    for (var in vars_mean) {
      var_name <- sym(var)
      mean_df <- df %>%
        group_by(!!wrt_name) %>% 
        summarize(!!var_name := mean(!!var_name))
      summary_df <- c(summary_df,mean_df)
      summary_df[duplicated(summary_df)] <- NULL
    }
    # Variables of which we count the unique values of the factors
    for (var in vars_count) {
      var_name <- sym(var)
      count_df <- df %>%
        recode_all(var) %>%
        group_by(!!var_name, !!wrt_name) %>%
        dplyr::summarize(count = n()) %>%
        spread(key = !!var_name, value = count)  
      summary_df <- summary_df %>% as.data.frame() %>%
        dplyr::mutate(!!wrt_name := as.character(!!wrt_name)) %>%
        full_join(count_df, by = wrt) %>%
        replace(is.na(.), 0)
    }
    # Percentage of the rows
    percentage_df <- df %>%
      group_by(!!wrt_name) %>%
      summarize(percentage = round(n()/nrow(.)*100, 2))
    summary_df <- summary_df %>% full_join(percentage_df, by = wrt)
  }
  return(summary_df)
}
  

  
  



## Recode variable: rename all factor levels of that variable
recode_all <- function(df, variable) {
  variable_name <- sym(variable)
  df <- df %>%
    mutate(!!variable_name := as.factor(!!variable_name))
  levels(df[,variable]) <- gsub(" ", "_", levels(df[,variable])) %>%
    tolower() %>%
    paste(variable, ., sep = "_")
  return(df)
}


## Spread rows into columns using just one variable
from_row_to_col <- function(df, variable) {
  df <- df %>%
    mutate(yesno = 1) %>%
    distinct() %>%
    spread(variable, yesno, fill = 0)
  return(df)
}

# Capitalize the first letter of every word
# from the help for toupper()
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# ## Transform points into lines (found online)
# points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL, id = "Ringbahn") {
#   
#   # Convert to SpatialPointsDataFrame
#   coordinates(data) <- c(long, lat)
#   
#   # If there is a sort field...
#   if (!is.null(sort_field)) {
#     if (!is.null(id_field)) {
#       data <- data[order(data[[id_field]], data[[sort_field]]), ]
#     } else {
#       data <- data[order(data[[sort_field]]), ]
#     }
#   }
#   
#   # If there is only one path...
#   if (is.null(id_field)) {
#     
#     lines <- SpatialLines(list(Lines(list(Line(data)), id)))
#     
#     return(lines)
#     
#     # Now, if we have multiple lines...
#   } else if (!is.null(id_field)) {  
#     
#     # Split into a list by ID field
#     paths <- sp::split(data, data[[id_field]])
#     
#     sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), paste(id, "line1"))))
#     
#     # I like for loops, what can I say...
#     for (p in 2:length(paths)) {
#       id2 <- paste0(id, "line", as.character(p))
#       l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id2)))
#       sp_lines <- spRbind(sp_lines, l)
#     }
#     
#     return(sp_lines)
#   }
# }
# 
# ## Punch a hole in a Polygon inside a PolygonDataFrame
# # From here: https://stackoverflow.com/questions/29624895/how-to-add-a-hole-to-a-polygon-within-a-spatialpolygonsdataframe
# AddHoleToPolygon <-function(poly,hole){
#   # invert the coordinates for Polygons to flag it as a hole
#   coordsHole <-  hole@polygons[[1]]@Polygons[[1]]@coords
#   newHole <- Polygon(coordsHole,hole=TRUE)
#   
#   # punch the hole in the main poly
#   listPol <- poly@polygons[[1]]@Polygons
#   listPol[[length(listPol)+1]] <- newHole
#   punch <- Polygons(listPol,poly@polygons[[1]]@ID)
#   
#   # make the polygon a SpatialPolygonsDataFrame as the entry
#   new <- SpatialPolygons(list(punch),proj4string=poly@proj4string)
#   new <- SpatialPolygonsDataFrame(new,data=as(poly,"data.frame"))
#   
#   return(new)
# }
