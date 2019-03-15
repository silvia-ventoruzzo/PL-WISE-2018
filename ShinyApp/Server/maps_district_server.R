#########################################################################
# Maps District Server-file #############################################
# Author: Silvia Ventoruzzo #############################################
#########################################################################

maps_district_server = function(input, output, session) {

  output$district_text <- renderText({
    "This tab is similar to the previous one. You can still choose which variable to show the average 
    in the different areas and display various properties, railway stations and tourist attractions.
    But here you see the city at the district's level, showing therefore the neighbourhoods in the
    different districts."
  })
  
  ### LEAFLET MAP
  
  ## TITLE
  
  title_secondary_map_change = reactive({
    
    if (variable2() == "none") {
      
      paste("Map of", input$district, sep = " ") %>%
        paste("'s", sep = "") %>%
        paste("Neighbourhoods", sep = " ")
      
    } else if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {
      
      variable = stringr::str_match(variable2(), paste(factor_vars(), collapse = "|")) %>% 
        c() %>% gsub("_", " ", .) %>% capwords()
      
      variable %>%
        paste("Count of the variable", ., "in the district", input$district, sep = " ")
      
    } else {
      
      variable = capwords(input$variable2)
      
      variable %>%
        paste("Average of the variable", ., "in the district", input$district, sep = " ")
      
    }
  })
  
  observeEvent({c(input$district, input$variable2)}, 
               {output$title_secondary_map = renderText({title_secondary_map_change()})}, ignoreNULL = FALSE)
  
  
  ## REACTIVE ELEMENTS
  
  # SF Dataframe to plot polygons
  neighbourhoods = reactive({berlin_sf %>%
      filter(view  == "Neighbourhoods",
             group == input$district)})
  
  # Dataframe with coordinates of the areas' names
  neighbourhood_names = reactive({berlin_names %>% 
      filter(view  == "Neighbourhoods",
             group == input$district)})
  
  # Number of colors depend on how many areas to display
  neighbourhood_colors = reactive({
    colorspace::rainbow_hcl(n = berlin_names %>% 
                              filter(view  == "Neighbourhoods",
                                     group == input$district) %>% 
                              dplyr::summarize(count = n()) %>% 
                              as.numeric())
  })
  
  # Rename variable2
  variable2 = reactive({
    
    gsub(" ", "_", input$variable2)
    
  })
  
  # Number of areas in selected view
  neighbourhood_area_num = reactive({
    
    berlin_names %>%
      dplyr::filter(view  == "Neighbourhoods",
                    group == input$district) %>%
      dplyr::summarize(count = n()) %>%
      as.numeric()
    
  })
  
  # Colors for average maps
  neighbourhood_colors_var = reactive({
    leaflet::colorNumeric(
      palette = topo.colors(area_num()),
      domain  = listings_area_summary %>%
        dplyr::filter(view == input$view) %>%
        dplyr::select(variable1()),
      n       = area_num(),
      reverse = TRUE)})
  
  
  # When choosing a variable (input$variable2), the color should represent the variable's distribution
  neighbourhood_colors_var = reactive({
    leaflet::colorNumeric(
      palette = topo.colors(neighbourhood_area_num()),
      domain  = listings_area_summary %>%
                  dplyr::filter(view == "Neighbourhoods",
                                group == input$district) %>%
                  dplyr::select(variable2()),
      n       = neighbourhood_area_num(),
      reverse = TRUE)
      
  })
  
  
  ## MAP
  
  output$secondary_map = renderLeaflet({
    
    district_icons = awesomeIconList(
      "district_properties" = makeAwesomeIcon(icon = "home", markerColor = "red"),
      "district_stations"   = makeAwesomeIcon(icon = "subway", library = "fa", markerColor = "green", iconColor = "#FFFFFF"),
      "district_sights"     = makeAwesomeIcon(icon = "eye-open", markerColor = "blue")
    )
    
    secondary_map = leaflet() %>%
      addTiles() %>%
      addLabelOnlyMarkers(data = neighbourhood_names(),
                          lng = ~long, lat = ~lat, label = ~lapply(name, htmltools::HTML),
                          labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                      textOnly = TRUE, textsize = "11px")) %>%
      addAwesomeMarkers(data = listings %>% filter(district == input$district),
                        lng = ~long, lat = ~lat,
                        icon = district_icons[["district_properties"]],
                        group = "district_properties",
                        popup = ~lapply(paste("<b><a href=", listing_url, "> ID: ", id, "</a></b>", sep = "") %>%
                                          paste("Price: ", sep = "<br/>") %>%
                                          paste(price, "$", sep = "") %>%
                                          paste(property_type, sep = "<br/>") %>%
                                          paste(room_type, sep = " - ") %>%     
                                          paste("Accomodates", sep = "<br/>") %>%
                                          paste(accommodates, ifelse(accommodates == 1, "person", "people"), sep = " "),
                                        htmltools::HTML),
                        clusterId = "1",
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction=JS("function (cluster) {    
                                                var childCount = cluster.getChildCount();  
                                                if (childCount > 1) {  
                                                c = 'rgba(235, 0, 0, 1);'}    
                                                return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),
                          singleMarkerMode = FALSE)) %>%
      addAwesomeMarkers(data = railway_stations_df %>% filter(district == input$district),
                        lng = ~long, lat = ~lat,
                        icon = district_icons[["district_stations"]],
                        popup = ~htmltools::htmlEscape(
                          paste(ifelse(s_bahn == TRUE, "S", ""),
                                ifelse(s_bahn == TRUE & u_bahn == TRUE, "+", ""),
                                ifelse(u_bahn == TRUE, "U", ""),
                                sep = "") %>%
                            paste(id, sep = " ")),
                        group = "district_stations",
                        clusterId = "2",
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction = JS("function (cluster) {    
                                                  var childCount = cluster.getChildCount();  
                                                  if (childCount > 1) {  
                                                  c = 'rgba(7, 187, 31, 1);'}    
                                                  return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),
                          singleMarkerMode = FALSE)) %>%
      addAwesomeMarkers(data = attractions_df %>% filter(district == input$district),
                        lng = ~long, lat = ~lat,
                        icon = district_icons[["district_sights"]],
                        popup = ~htmltools::htmlEscape(id),
                        group = "district_sights",
                        clusterId = "3",
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction = JS("function (cluster) {    
                                                  var childCount = cluster.getChildCount();  
                                                  if (childCount > 1) {  
                                                  c = 'rgba(11, 200, 213, 1);'}    
                                                  return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),
                          singleMarkerMode = FALSE)) %>%
      hideGroup(group = c("district_properties", "district_stations", "district_sights")) %>%
      addMiniMap(centerFixed    = berlin_center %>% t() %>% c(),
                 zoomLevelFixed = 8,
                 zoomAnimation  = FALSE)
    
    # if no variable is selected
    if (variable2() == "none") {
      
      secondary_map %>%
        addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                    color = "grey", fillColor = neighbourhood_colors())
     
    # if variable is selected   
    } else {
      
      neighbourhood_colors_vars = neighbourhood_colors_var()
      
      secondary_map %>%
        addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                    color = "grey",
                    fillColor = ~neighbourhood_colors_vars(
                                    listings_area_summary
                                          [listings_area_summary$view  == "Neighbourhoods" &
                                           listings_area_summary$group == input$district,
                                           variable2()])) %>%
        addLegend(position = "bottomleft", pal = neighbourhood_colors_var(),
                  values = listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
                                                   listings_area_summary$group == input$district,
                                                 variable2()],
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                  title = variable2())
      
        }
    })
  
  # checkboxInputs
  observeEvent({c(input$district_properties, input$district_stations, input$district_sights, input$district)}, {
    
    proxy = leafletProxy("secondary_map")
    
    if (input$district_properties) {proxy %>% showGroup("district_properties")
    } else {proxy %>% hideGroup("district_properties")}
    
    if (input$district_stations) {proxy %>% showGroup("district_stations")
    } else {proxy %>% hideGroup("district_stations")}
    
    if (input$district_sights) {proxy %>% showGroup("district_sights")
    } else {proxy %>% hideGroup("district_sights")}
    
  })
  
  ### SUMMARY TABLE
  
  ## TITLE

  title_secondary_table_change = reactive({
    
    if (variable2() == "none") { return() }
    
    if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {
      
      variable = stringr::str_match(variable2(), paste(factor_vars(), collapse = "|")) %>% 
        c() %>% gsub("_", " ", .) %>% capwords()
      
    } else {
      
      variable = capwords(input$variable2)
      
    }
    
    variable %>%
      paste("Summary statistics of the variable", ., "in the district", input$district, sep = " ")
    
  })
  
  observeEvent({c(input$district, input$variable2)}, 
               {output$title_secondary_table = renderText({title_secondary_table_change()})}, ignoreNULL = FALSE)
  
  ## REACTIVE ELEMENTS
  
  # Categorical variables
  factor_vars = reactive({
    
    listings %>%
      dplyr::select(-id, -listing_url, -long, -lat, -vbb_zone, -district, -neighbourhood) %>%
      dplyr::select_if(function(x) is.character(x) | is.factor(x) | is.logical(x)) %>%
      names()
    
  })
  
  var_table_plot2 = reactive({

    if (variable2() == "none") { return() }

    if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {

      variable = stringr::str_match(variable2(), paste(factor_vars(), collapse = "|")) %>% c()

    } else {

      variable = variable2()

    }

  })

  
  table_df = reactive({
    
    if (variable2() == "none") { return() }
    
    df = listings %>%
      dplyr::filter(district == input$district) %>%
      dplyr::select(var_table_plot2())
    
    return(df)
    
  })
  
  ## TABLE
  
  # Descriptive table
  output$secondary_table1 = renderTable({
    
    if (variable2() == "none") {
      
      return() 
      
    } else if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {
      
      secondary_df = descriptive_statistics(table_df())
        
    } else {
      
      secondary_df = descriptive_statistics(table_df()) %>%
        dplyr::select(min, `1Q`, median, `3Q`, max)
      
    }
    
    return(secondary_df)
    
  })
  
  output$secondary_table2 = renderTable({
    
    if (variable2() == "none") {
      
      return() 
      
    } else if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {
      
      return()
      
    } else {
      
      secondary_df = descriptive_statistics(table_df()) %>%
        dplyr::select(mean, sd)
      
    }
    
    return(secondary_df)
    
  })

  ### DISTRIBUTION PLOT
  
  ## TITLE
  title_secondary_plot_change = reactive({
    
    # factor_vars() <- c("room_type", "property_type", "room_type")
    
    if (variable2() == "none") { return() }
    
    if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {
      
      variable = stringr::str_match(variable2(), paste(factor_vars(), collapse = "|")) %>% 
        c() %>% gsub("_", " ", .) %>% capwords()
      
    } else {
      
      variable = capwords(input$variable2)
      
    }
    
    variable %>%
      paste("Distribution of the variable", ., "in the district", input$district, sep = " ")
  })
  
  observeEvent({c(input$view, input$variable2)},
               {output$title_secondary_plot = renderText({title_secondary_plot_change()})},
               ignoreNULL = FALSE)
  
  
  ## REACTIVE ELEMENTS
  
  plot_df2 = reactive({

    if (variable2() == "none") {

      return()

    } else if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {

      df = listings %>%
        filter(district == input$district) 

    } else {

      variable = var_table_plot2() %>% sym()

      df = listings %>%
        filter(district == input$district) 
      
    }

    return(df)
  })
  
  ## PLOT
  
  output$secondary_plot = renderPlot({
    
    if (variable2() == "none") { 
      
      return() 
      
    } else if (grepl(paste(factor_vars(), collapse = "|"), variable2())) {
      
      plot2 = distribution_plot(plot_df2(), var_table_plot2(), tilt_text_x = TRUE)
      
    } else {
      
      plot2 = distribution_plot(plot_df2(), var_table_plot2(), hide_outliers = TRUE)
      
    }
    
    return(plot2)
    
  })
  
}
