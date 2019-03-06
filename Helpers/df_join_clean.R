# Join dataframes and cleaning
df_join_clean <- function(df1, df2) {
  
  df_joined <- full_join(df1, df2, by = c("id", "host_id", "room_type",
                                          "minimum_nights",
                                          "number_of_reviews", 
                                          "latitude", "longitude",
                                          "availability_365", "neighbourhood_cleansed" = "neighbourhood")) %>% 
    rename(lat  = latitude,
           long = longitude) %>%
    
    # price and fee columns
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(gsub("\\$", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(gsub("\\,", "", .))) %>%
    dplyr::mutate_if(grepl("price|cleaning|deposit|people" , 
                           names(.)), funs(as.numeric(.))) %>%
    
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
  
  return(df_joined)
}