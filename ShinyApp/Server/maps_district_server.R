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
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") {
      
      paste("Map of", input$district, sep = " ") %>%
        paste("'s", sep = "") %>%
        paste("Neighbourhoods", sep = " ")
      
    } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      variable = stringr::str_match(gsub(" ", "_", input$variable2), paste(factor_vars, collapse = "|")) %>% 
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
  
  # When choosing a variable (input$variable2), the color should represent the variable'distribution
  neighbourhood_colors_var = reactive({leaflet::colorNumeric(palette = "Dark2",
                                                             domain = listings_area_summary %>%
                                                               filter(view == "Neighbourhoods",
                                                                      group == input$district) %>%
                                                               select(gsub(" ", "_", input$variable2)),
                                                             n = berlin_names %>%
                                                               filter(view  == "Neighbourhoods",
                                                                      group == input$district) %>%
                                                               dplyr::summarize(count = n()) %>%
                                                               as.numeric(),
                                                             reverse = TRUE)}) # find better color palette!!!
  
  
  ## MAP
  
  output$secondary_map = renderLeaflet({
    
    district_icons = awesomeIconList(
      "district_properties" = makeAwesomeIcon(icon = "home", markerColor = "red"),
      "district_stations"   = makeAwesomeIcon(icon = "subway", library = "fa", markerColor = "green", iconColor = "#FFFFFF"),
      "district_sights"     = makeAwesomeIcon(icon = "eye-open", markerColor = "blue")
    )
    
    secondary_map = leaflet() %>%
      addTiles() %>%
      # addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
      #             color = "grey", fillColor = neighbourhood_colors(),
      #             group = "start") %>%
      # addPolygons(data = neighbourhoods(),
      #             fillColor = ~neighbourhood_colors_vars(listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
      #                                                                            listings_area_summary$group == input$district, 
      #                                                                          gsub(" ", "_", input$variable2)]),
      #             group = "variables") %>%
      # addLegend(position = "bottomleft", pal = neighbourhood_colors_var(), 
      #           values = listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
      #                                            listings_area_summary$group == input$district, 
    #                                          gsub(" ", "_", input$variable2)],
    #           labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
    #           title = gsub(" ", "_", input$variable2),
    #           group = "variables") %>%
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
    # %>%
    #   setView(lng = berlin_names[berlin_names$view == "Districts" & berlin_names$group == input$district, "long"],
    #           lat = berlin_names[berlin_names$view == "Districts" & berlin_names$group == input$district, "lat"], 
    #           zoom = 11.5)
    
    if (gsub(" ", "_", input$variable2) == "none") {
      
      secondary_map %>%
        addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                    color = "grey", fillColor = neighbourhood_colors())
      
    } else {
      
      neighbourhood_colors_vars = neighbourhood_colors_var()
      
      secondary_map %>%
        addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                    color = "grey",
                    fillColor = ~neighbourhood_colors_vars(listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
                                                                                   listings_area_summary$group == input$district,
                                                                                 gsub(" ", "_", input$variable2)])) %>%
        addLegend(position = "bottomleft", pal = neighbourhood_colors_var(),
                  values = listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
                                                   listings_area_summary$group == input$district,
                                                 gsub(" ", "_", input$variable2)],
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
                  title = gsub(" ", "_", input$variable2))
      
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
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") { return() }
    
    if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      variable = stringr::str_match(gsub(" ", "_", input$variable2), paste(factor_vars, collapse = "|")) %>% 
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
  
  var_table_plot2 = reactive({
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") { return() }
    
    if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      variable = stringr::str_match(gsub(" ", "_", input$variable2), paste(factor_vars, collapse = "|")) %>% c()
      
    } else {
      
      variable = gsub(" ", "_", input$variable2)
      
    }
    
  })
  
  ## TABLE
  
  # Table of average or count values of the selected variable (input$variable2) for each neighbourhood
  # in the selected district (input$district)
  output$secondary_table = renderTable({
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") { return() }
    
    if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      variable = stringr::str_match(gsub(" ", "_", input$variable2), paste(factor_vars, collapse = "|")) %>% c()
      
      secondary_df  = summary_stat_fact(df               = listings,
                                        var              = variable,
                                        constraint_var   = "district",
                                        constraint_value = input$district)
      
    } else {
      
      variable = gsub(" ", "_", input$variable2)
      
      secondary_df  = summary_stat_num(df               = listings,
                                       var              = variable,
                                       constraint_var   = "district",
                                       constraint_value = input$district)
    }
    
    return(secondary_df)
    
  })

  ### DISTRIBUTION PLOT
  
  ## TITLE
  title_secondary_plot_change = reactive({
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") { return() }
    
    if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      variable = stringr::str_match(gsub(" ", "_", input$variable2), paste(factor_vars, collapse = "|")) %>% 
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
  
  plot_df = reactive({
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") { 
      
      return() 
      
    } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      variable = var_table_plot2() %>% sym()
      
      df = listings %>%
        filter(district == input$district) %>%
        group_by(!!variable) %>%
        summarize(count = n()) %>%
        rename(var = !!variable)
      
    } else {
      
      variable = var_table_plot2() %>% sym()
      
      df = listings %>%
        filter(district == input$district) %>%
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
  
  output$secondary_plot = renderPlot({
    
    factor_vars <- c("room_type", "property_type", "room_type")
    
    if (gsub(" ", "_", input$variable2) == "none") { 
      
      return() 
      
    } else if (grepl(paste(factor_vars, collapse = "|"), gsub(" ", "_", input$variable2))) {
      
      ggplot(plot_df()) +
        geom_bar(aes(x = var, y = count), fill = "green", stat = "identity") +
        labs(x = var_table_plot2()) +
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
             x = var_table_plot2()) +
        theme_bw() +
        theme(axis.text.x = element_text(size = rel(1.5)))
      
    }
    
  })
  
}
