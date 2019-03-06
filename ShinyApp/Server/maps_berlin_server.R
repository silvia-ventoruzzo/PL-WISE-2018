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
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") {
        
        paste("Map of Berlin's", input$view, sep = " ")
        
      } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        variable = stringr::str_match(gsub(" ", "_", input$variable1), paste(factor_vars, collapse = "|")) %>% 
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
    names = reactive({berlin_names %>% filter(view == input$view)})
    
    # Number of colors depend on how many areas to display
    colors = reactive({
      colorspace::rainbow_hcl(n = berlin_names %>% 
                                filter(view == input$view) %>% 
                                dplyr::summarize(count = n()) %>% 
                                as.numeric())
      
      
    })
    
    # When choosing a variable (input$variable1), the color should represent the variable'distribution
    colors_var = reactive({leaflet::colorNumeric(palette = "Dark2",
                                                 domain = listings_area_summary %>%
                                                   filter(view == input$view) %>%
                                                   select(gsub(" ", "_", input$variable1)),
                                                 n = berlin_names %>%
                                                   filter(view == input$view) %>%
                                                   dplyr::summarize(count = n()) %>%
                                                   as.numeric(),
                                                 reverse = TRUE)}) # find better color palette!!!
    
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
        addLabelOnlyMarkers(data = names(),
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
      
      if (gsub(" ", "_", input$variable1) == "none") {
        
        main_map %>%
          addPolygons(data = polygons(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                      color = "grey", fillColor = colors())
        
      } else {
        
        colors_vars = colors_var()
        
        main_map %>%
          addPolygons(data = polygons(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                      color = "grey", 
                      fillColor = ~colors_vars(listings_area_summary[listings_area_summary$view == input$view, gsub(" ", "_", input$variable1)])) %>%
          addLegend(position = "bottomleft", pal = colors_var(), 
                    values = listings_area_summary[listings_area_summary$view == input$view, gsub(" ", "_", input$variable1)],
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                    title = gsub(" ", "_", input$variable1))
        
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
    
    
    ### SUMMARY TABLE
    
    ## TITLE
    title_main_table_change = reactive({
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { return() }
      
      if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        variable = stringr::str_match(gsub(" ", "_", input$variable1), paste(factor_vars, collapse = "|")) %>% 
          c() %>% gsub("_", " ", .) %>% capwords()
        
      } else {
        
        variable = capwords(input$variable1)
        
      }
      
      variable %>%
        paste("Summary statistics of the variable", ., sep = " ")
      
    })
    
    observeEvent({c(input$view, input$variable1)}, 
                 {output$title_main_table = renderText({title_main_table_change()})}, 
                 ignoreNULL = FALSE)
    
    ## REACTIVE ELEMENTS
    
    var_table_plot = reactive({
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { return() }
      
      if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        variable = stringr::str_match(gsub(" ", "_", input$variable1), paste(factor_vars, collapse = "|")) %>% c()
        
      } else {
        
        variable = gsub(" ", "_", input$variable1)
        
      }
      
    })
    
    ## TABLEs
    
    output$main_table1 = renderTable({
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { return() 
        
        } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        main_df  = summary_stat_fact(df  = listings,
                                     var = var_table_plot())
        
      } else {
        
        variable = gsub(" ", "_", input$variable1)
        
        main_df  = summary_stat_num(df  = listings,
                                    var = var_table_plot()) %>%
          dplyr::select(min, `1Q`, median, `3Q`, max)
      }
      
      return(main_df)
      
    })
    
    output$main_table2 = renderTable({
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { return() 
        
        } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        return()
        
      } else {
        
        variable = gsub(" ", "_", input$variable1)
        
        main_df  = summary_stat_num(df  = listings,
                                    var = var_table_plot()) %>%
          dplyr::select(mean, sd)
      }
      
      return(main_df)
      
    })
    
    
    ### DISTRIBUTION PLOT
    
    ## TITLE
    title_main_plot_change = reactive({

      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { return() }
      
      if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        variable = stringr::str_match(gsub(" ", "_", input$variable1), paste(factor_vars, collapse = "|")) %>% 
          c() %>% gsub("_", " ", .) %>% capwords()
        
      } else {
        
        variable = capwords(input$variable1)
        
      }
      
      variable %>%
        paste("Distribution of the variable", ., sep = " ")
    })

    observeEvent({c(input$view, input$variable1)},
                 {output$title_main_plot = renderText({title_main_plot_change()})},
                 ignoreNULL = FALSE)
    

    ## REACTIVE ELEMENTS
    
    plot_df = reactive({
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { 
        
        return() 
        
      } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        variable = var_table_plot() %>% sym()
        
        df = listings %>%
          group_by(!!variable) %>%
          summarize(count = n()) %>%
          rename(var = !!variable)
        
      } else {
        
        variable = var_table_plot() %>% sym()
        
        df = listings %>%
          transmute(var    = !!variable,
                    median = median(!!variable),
                    mean   = mean(!!variable),
                    `1Q`   = quantile(!!variable, probs = 0.25),
                    `3Q`   = quantile(!!variable, probs = 0.75),
                    `95P`  = quantile(!!variable, probs = 0.95),
                    count  = sum(!!variable > unique(`95P`)))
        
      }
      
      return(df)
    })
    
    ## PLOT
    
    output$main_plot = renderPlot({
      
      factor_vars <- c("room_type", "property_type", "room_type")
      
      if (gsub(" ", "_", input$variable1) == "none") { 
        
        return() 
        
      } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable1))) {
        
        ggplot(plot_df()) +
          geom_bar(aes(x = var, y = count), fill = "green", stat = "identity") +
          labs(x = var_table_plot()) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)))
        
      } else {
        
        ggplot(plot_df()) +
          geom_density(aes(x = var), fill = "green") +
          geom_vline(aes(xintercept = unique(median)),
                     color = "blue", linetype = "dashed") +
          annotate(geom = "text", x = (unique(plot_df()$median) + 2.6), y = 0,
                   label = "Median",
                   colour = "blue", 
                   angle = 90, hjust = 0) +
          geom_vline(aes(xintercept = unique(mean)),
                     color = "black", linetype = "dashed") +
          annotate(geom = "text", x = (unique(plot_df()$mean) + 2.6), y = 0,
                   label = "Mean",
                   colour = "black", 
                   angle = 90, hjust = 0) +
          geom_vline(aes(xintercept = `1Q`),
                     color = "red", linetype = "dashed") +
          annotate(geom = "text", x = (unique(plot_df()$`1Q`) + 2.6), y = 0,
                   label = "1st Quantile",
                   colour = "red", 
                   angle = 90, hjust = 0) +
          geom_vline(aes(xintercept = `3Q`),
                     color = "red", linetype = "dashed") +
          annotate(geom = "text", x = (unique(plot_df()$`3Q`) + 2.6), y = 0,
                   label = "3rd Quantile",
                   colour = "red", 
                   angle = 90, hjust = 0) +
          scale_x_continuous(limits = c(min(plot_df()$var), unique(plot_df()$`95P`))) +
          labs(caption = paste("Left limit set to 95% percentile:", unique(plot_df()$count), "values not displayed", sep = " "),
               x = var_table_plot()) +
          theme_bw() +
          theme(axis.text.x = element_text(size = rel(1.5)))
        
      }
      
    })

}