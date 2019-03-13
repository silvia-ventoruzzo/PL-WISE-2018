# Join dataframes and cleaning
df_join_clean = function(df1, df2) {
  
  df1 = df1 %>%
    # price and fee columns
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(gsub("\\$", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(gsub("\\,", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(as.numeric(.)))
  
  df2 = df2 %>%
    # price and fee columns
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(gsub("\\$", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(gsub("\\,", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(as.numeric(.)))
  
  df_joined = full_join(df1, df2, by = c("id", "host_id", "room_type", "price",
                                          "minimum_nights",
                                          "number_of_reviews", 
                                          "latitude", "longitude",
                                          "availability_365", "calculated_host_listings_count")) %>% 
    #First selection
    dplyr::select(id, longitude, latitude, price, property_type, room_type,
                  instant_bookable, listing_url, host_is_superhost, 
                  security_deposit, cleaning_fee,
                  calculated_host_listings_count, host_has_profile_pic, host_identity_verified,
                  accommodates, bedrooms, beds, minimum_nights, 
                  review_scores_rating, number_of_reviews, cancellation_policy,
                  availability_30, availability_60, availability_90, availability_365) %>%
    dplyr::rename(lat                 = latitude,
                  long                = longitude,
                  host_listings_count = calculated_host_listings_count) %>%
    # property_type
    dplyr::group_by(property_type) %>% 
    dplyr::mutate(property_type_count = n()) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(property_type     = as.factor(ifelse(property_type_count <= 10,
                                            "Other", property_type)),
    # security_deposit
                  security_deposit  = ifelse(security_deposit == 0 |
                                             is.na(security_deposit), FALSE, TRUE),
    # cleaning_fee
                  cleaning_fee      = ifelse(cleaning_fee == 0 |
                                             is.na(cleaning_fee), FALSE, TRUE),
    # availabilities
                  availability_30   = round(availability_30/30*100, 4),
                  availability_60   = round(availability_60/60*100, 4),
                  availability_90   = round(availability_90/90*100, 4),
                  availability_365  = round(availability_365/365*100, 4)) %>%
    dplyr::select(-property_type_count)

  return(df_joined)
}