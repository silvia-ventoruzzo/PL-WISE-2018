#########################################################################
# Maps Berlin Server-file ###############################################
# Author: Silvia Ventoruzzo #############################################
#########################################################################

maps_berlin_server = function(input, output, session) {
    
    output$berlin_text <- renderText({
      "In this tab you can view a map of entire Berlin divided in its different areas. You can choose which view
      to show and also which variable to show the average in the different areas. Moreover you can also display
      the properties themselves, the railway stations and the top 10 tourist attractions."
    })
    
    ### LEAFLET MAP
    
    ## TITLE
    title_main_map_change = reactive({
      
      if (variable1() == "none") {
        
        paste("Map of Berlin's", input$view, sep = " ")
        
      } else if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        variable = stringr::str_match(variable1(), paste(factor_vars(), collapse = "|")) %>% 
          c() %>% gsub("_", " ", .) %>% capwords()
        
        variable %>%
          paste("Count of the variable", ., "in Berlin's", input$view, sep = " ")
        
      } else {
        
        variable = capwords(input$variable1)
        
        variable %>%
          paste("Average of the variable", ., "in Berlin's", input$view, sep = " ")
        
      }
      
    })
    
    observeEvent({c(input$view, input$variable1)}, 
                 {output$title_main_map = renderText({title_main_map_change()})},
                 ignoreNULL = FALSE)
    
    ## REACTIVE ELEMENTS
    
    # SF Dataframe to plot polygons
    polygons = reactive({berlin_sf %>% filter(view == input$view)})
    
    # Dataframe with coordinates of the areas' names
    area_names = reactive({berlin_names %>% filter(view == input$view)})
    
    # Number of colors depend on how many areas to display
    colors = reactive({
      colorspace::rainbow_hcl(n = berlin_names %>% 
                                filter(view == input$view) %>% 
                                dplyr::summarize(count = n()) %>% 
                                as.numeric())
    })
    
    # Rename variable1
    variable1 = reactive({
      
      gsub(" ", "_", input$variable1)
      
    })
    
    var_table_plot = reactive({
      
      if (variable1() == "none") { return() }
      
      if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        variable = stringr::str_match(variable1(), paste(factor_vars(), collapse = "|")) %>% c()
        
      } else {
        
        variable = variable1()
        
      }
      
    })
    
    # Number of areas in selected view
    area_num = reactive({
      
      berlin_names %>%
        dplyr::filter(view == input$view) %>%
        dplyr::summarize(count = n()) %>%
        as.numeric()
      
    })
    
    # Colors for average maps
    colors_var = reactive({
      leaflet::colorNumeric(
        palette = topo.colors(area_num()),
        domain  = listings_area_summary %>%
                    dplyr::filter(view == input$view) %>%
                    dplyr::select(variable1()),
        n       = area_num(),
        reverse = TRUE)})
    
    # Text size depend on how many areas to display
    text_size = reactive({ifelse(input$view == "Districts", "11px", "20px")})
    
    ## MAP
    
    output$main_map = renderLeaflet({
      
      main_icons = awesomeIconList(
        "main_properties" = makeAwesomeIcon(icon = "home", markerColor = "red"),
        "main_stations"   = makeAwesomeIcon(icon = "subway", library = "fa", markerColor = "green", iconColor = "#FFFFFF"),
        "main_sights"     = makeAwesomeIcon(icon = "eye-open", markerColor = "blue")
      )
      
      main_map = leaflet() %>%
        addTiles() %>%
        addLabelOnlyMarkers(data = area_names(),
                            lng = ~long, lat = ~lat, label = ~lapply(name, htmltools::HTML),
                            labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                        textOnly = TRUE, textsize = text_size()))  %>%
        addAwesomeMarkers(data = listings,
                          lng = ~long, lat = ~lat,
                          icon = main_icons[["main_properties"]],
                          group = "main_properties",
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
        addAwesomeMarkers(data = railway_stations_df,
                          lng = ~long, lat = ~lat,
                          icon = main_icons[["main_stations"]],
                          popup = ~htmltools::htmlEscape(
                            paste(ifelse(s_bahn == TRUE, "S", ""),
                                  ifelse(s_bahn == TRUE & u_bahn == TRUE, "+", ""),
                                  ifelse(u_bahn == TRUE, "U", ""),
                                  sep = "") %>%
                              paste(id, sep = " ")),
                          group = "main_stations",
                          clusterId = "2",
                          clusterOptions = markerClusterOptions(
                            iconCreateFunction = JS("function (cluster) {    
                                                    var childCount = cluster.getChildCount();  
                                                    if (childCount > 1) {  
                                                    c = 'rgba(7, 187, 31, 1);'}    
                                                    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),
                            singleMarkerMode = FALSE)) %>%
        addAwesomeMarkers(data = attractions_df,
                          lng = ~long, lat = ~lat,
                          icon = main_icons[["main_sights"]],
                          popup = ~htmltools::htmlEscape(id),
                          group = "main_sights",
                          clusterId = "3",
                          clusterOptions = markerClusterOptions(
                            iconCreateFunction = JS("function (cluster) {    
                                                    var childCount = cluster.getChildCount();  
                                                    if (childCount > 1) {  
                                                    c = 'rgba(11, 200, 213, 1);'}    
                                                    return new L.DivIcon({ html: '<div style=\"background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}"),
                            singleMarkerMode = FALSE)) %>%
        hideGroup(group = c("main_properties", "main_stations", "main_sights"))  %>%
        setView(lng = berlin_center$long, lat = berlin_center$lat, zoom = 10)
      
      if (variable1() == "none") {
        
        main_map %>%
          addPolygons(data = polygons(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                      color = "grey", fillColor = colors())
        
      } else {
        
        colors_vars = colors_var()
        
        main_map %>%
          addPolygons(data = polygons(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                      color = "grey", 
                      fillColor = ~colors_vars(listings_area_summary[listings_area_summary$view == input$view, variable1()])) %>%
          addLegend(position = "bottomleft", pal = colors_var(), 
                    values = listings_area_summary[listings_area_summary$view == input$view, variable1()],
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                    title = variable1())
        
      }
      })
    
    # checkboxInputs
    observeEvent({c(input$main_properties, input$main_stations, input$main_sights)}, {
      
      proxy = leafletProxy("main_map")
      
      if (input$main_properties) {proxy %>% showGroup("main_properties")
      } else {proxy %>% hideGroup("main_properties")}
      
      if (input$main_stations) {proxy %>% showGroup("main_stations")
      } else {proxy %>% hideGroup("main_stations")}
      
      if (input$main_sights) {proxy %>% showGroup("main_sights")
      } else {proxy %>% hideGroup("main_sights")}
      
    })
    
    
    ### DESCRIPTIVE TABLE
    
    ## TITLE
    title_main_table_change = reactive({
    
      
      if (variable1() == "none") { return() }
      
      if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        variable = stringr::str_match(variable1(), paste(factor_vars(), collapse = "|")) %>% 
          c() %>% gsub("_", " ", .) %>% capwords()
        
      } else {
        
        variable = capwords(input$variable1)
        
      }
      
      variable %>%
        paste("Descriptive statistics of the variable", ., sep = " ")
      
    })
    
    observeEvent({c(input$view, input$variable1)}, 
                 {output$title_main_table = renderText({title_main_table_change()})}, 
                 ignoreNULL = FALSE)
    
    ## REACTIVE ELEMENTS
    
    # Categorical variables
    factor_vars = reactive({
      
      listings %>%
        dplyr::select(-id, -listing_url, -long, -lat, -vbb_zone, -district, -neighbourhood) %>%
        dplyr::select_if(function(x) is.character(x) | is.factor(x) | is.logical(x)) %>%
        names()
      
    })
    
    
    var_table_plot = reactive({
      
      if (variable1() == "none") { return() }
      
      if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        variable = stringr::str_match(variable1(), paste(factor_vars(), collapse = "|")) %>% c()
        
      } else {
        
        variable = variable1()
        
      }
      
    })
    
    ## TABLES
    
    output$main_table1 = renderTable({
      
      if (variable1() == "none") {
        
        return() 
        
      } else if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        main_df  = statistics_cat %>%
          dplyr::filter(variable == var_table_plot()) %>%
          dplyr::select(-variable)
        
      } else {
        
        main_df  = statistics_num %>%
          dplyr::filter(variable == var_table_plot()) %>%
          dplyr::select(min, `1Q`, median, `3Q`, max)
      }
      
      return(main_df)
      
    })
    
    output$main_table2 = renderTable({
      
      if (variable1() == "none") { 
        
        return() 
        
      } else if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        return()
        
      } else {
        
        main_df  = statistics_num %>%
          dplyr::filter(variable == var_table_plot()) %>%
          dplyr::select(mean, sd)
      }
      
      return(main_df)
      
    })
    
    
    ### DISTRIBUTION PLOT
    
    ## TITLE
    title_main_plot_change = reactive({
      
      if (variable1() == "none") { return() }
      
      if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        variable = stringr::str_match(variable1(), paste(factor_vars(), collapse = "|")) %>% 
          c() %>% gsub("_", " ", .) %>% capwords()
        
      } else {
        
        variable = capwords(variable1())
        
      }
      
      variable %>%
        paste("Distribution of the variable", ., sep = " ")
    })

    observeEvent({c(input$view, input$variable1)},
                 {output$title_main_plot = renderText({title_main_plot_change()})},
                 ignoreNULL = FALSE)
    

    ## REACTIVE ELEMENTS
    
    ## PLOT
    
    output$main_plot = renderPlot({
      
      if (variable1() == "none") { 
        
        return() 
        
      } else if (grepl(paste(factor_vars(), collapse = "|"), variable1())) {
        
        variable = stringr::str_match(variable1(), paste(factor_vars(), collapse = "|")) %>% 
          c() %>% gsub("_", " ", .) %>% capwords()
        
        plot = distribution_plot(listings, var_table_plot(), tilt_text_x = TRUE)
        
      } else {
        
        plot = distribution_plot(listings, var_table_plot(), hide_outliers = TRUE)
        
      }
      
      return(plot)
      
    })

}