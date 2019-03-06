server = function(input, output, session) {
  
  # Tab 1
  introduction_server(input, output, session)
  
  # Tab 2
  
    # Panel 1
    maps_berlin_server(input, output, session)
    
    # Panel 2
    maps_district_server(input, output, session)
    
  # Tab 3
    
    # Panel 1
    price_corr_server(input, output, session)
    
    # Panel 2
    price_lm_server(input, output, session)
  
  # Tab 4
  clustering_server(input, output, session)

}
