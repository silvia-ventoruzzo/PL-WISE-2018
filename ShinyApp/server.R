p_load(colorspace)
p_load(htmltools)

server <- function(input, output) {
  
  ### TAB1
  
  # SF Dataframe to plot polygons
  polygons <- reactive({berlin_sf %>% filter(view == input$view)})
  
  # Dataframe with coordinates of the areas' names
  names <- reactive({berlin_names %>% filter(view == input$view)})
  
  # Number of colors depend on how many areas to display
  colors <- reactive({
    colorspace::rainbow_hcl(n = berlin_names %>% 
                              filter(view == input$view) %>% 
                              dplyr::summarize(count = n()) %>% 
                              as.numeric())
    
    
  })
  
  # When choosing a variable (input$variable1), the color should represent the variable'distribution
  colors_var <- reactive({leaflet::colorNumeric(palette = "Dark2",
                                                domain = listings_area_summary %>%
                                                  filter(view == input$view) %>%
                                                  select(gsub(" ", "_", input$variable1)),
                                                n = berlin_names %>%
                                                  filter(view == input$view) %>%
                                                  dplyr::summarize(count = n()) %>%
                                                  as.numeric(),
                                                reverse = TRUE)}) # find better color palette!!!
  
  # Text size depend on how many areas to display
  text_size <- reactive({ifelse(input$view == "Districts", "11px", "20px")})
  
  # Title changes depending on the choice of view (input$view) and variable(input$variable1)
  title_main_map_change <- reactive({
    
    if (gsub(" ", "_", input$variable1) == "none") {
      paste("Map of Berlin's", input$view, sep = " ")
    } else {
    capwords(input$variable1) %>%
      paste("Distribution of", ., sep = " ") %>%
      paste("in Berlin's", sep = " ") %>% 
      paste(input$view, sep = " ")
    }
  })
  
  observeEvent({c(input$view, input$variable1)}, {output$title_main_map <- renderText({title_main_map_change()})}, ignoreNULL = FALSE)
  
  title_main_table_change <- reactive({
    
    if (gsub(" ", "_", input$variable1) == "none") {
      paste("Areas in Berlin's", input$view, sep = " ")
    } else {
      capwords(input$variable1) %>%
        paste("Values of", ., sep = " ") %>%
        paste("in Berlin's", sep = " ") %>% 
        paste(input$view, sep = " ")
    }
  })
  
  observeEvent({c(input$view, input$variable1)}, {output$title_main_table <- renderText({title_main_table_change()})}, ignoreNULL = FALSE)
  
  
  # Berlin map for tab1
  output$main_map <- renderLeaflet({
    
    main_icons <- awesomeIconList(
      "main_properties" = makeAwesomeIcon(icon = "home", markerColor = "red"),
      "main_stations"   = makeAwesomeIcon(icon = "subway", library = "fa", markerColor = "green", iconColor = "#FFFFFF"),
      "main_sights"     = makeAwesomeIcon(icon = "eye-open", markerColor = "blue")
    )
    
    main_map <- leaflet() %>%
      addTiles() %>%
      addLabelOnlyMarkers(data = names(),
                          lng = ~long, lat = ~lat, label = ~lapply(leaflet, htmltools::HTML),
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
      addAwesomeMarkers(data = bahn_stations_df,
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
      
      colors_vars <- colors_var()
      
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
        
        proxy <- leafletProxy("main_map")
        
        if (input$main_properties) {proxy %>% showGroup("main_properties")
        } else {proxy %>% hideGroup("main_properties")}
        
        if (input$main_stations) {proxy %>% showGroup("main_stations")
        } else {proxy %>% hideGroup("main_stations")}
        
        if (input$main_sights) {proxy %>% showGroup("main_sights")
        } else {proxy %>% hideGroup("main_sights")}
        
      })
  

      # Table of average or count values of the selected variable (input$variable1) for each area (input$view)
      output$main_table <- renderTable({

        if (gsub(" ", "_", input$variable1) == "none") {

          listings_area_summary %>%
            dplyr::filter(view  == input$view) %>%
            dplyr::select(id) %>%
            dplyr::rename(areas = id)

        }  else {

            listings_area_summary %>%
              dplyr::filter(view == input$view) %>%
              tibble::column_to_rownames(var = "id") %>%
              dplyr::select(gsub(" ", "_", input$variable1)) %>%
              t()
          }
      })
      
      ### TAB2
      
      # Title changes depending on the choice of district (input$district) and variable (input$variable2)
      title_secondary_map_change <- reactive({
        
        if (gsub(" ", "_", input$variable2) == "none") {
          paste("Map of", input$district, sep = " ") %>%
            paste("'s", sep = "") %>%
            paste("Neighbourhoods", sep = " ")
        } else {
          capwords(input$variable2) %>%
            paste("Distribution of", ., sep = " ") %>%
            paste("in", sep = " ") %>% 
            paste(input$district, sep = " ")
        }
      })
      
      observeEvent({c(input$district, input$variable2)}, {output$title_secondary_map <- renderText({title_secondary_map_change()})}, ignoreNULL = FALSE)
      
      title_secondary_table_change <- reactive({

        if (gsub(" ", "_", input$variable2) == "none") {
          paste("Neighbourhoods in", input$district, sep = " ")
        } else {
          capwords(input$variable2) %>%
            paste("Values of", ., sep = " ") %>%
            paste("in", sep = " ") %>%
            paste(input$district, sep = " ") %>%
            paste("'s", sep = "") %>%
            paste("Neighbourhoods", sep = " ")
        }
      })

      observeEvent({c(input$district, input$variable2)}, {output$title_secondary_table <- renderText({title_secondary_table_change()})}, ignoreNULL = FALSE)
      
      # SF Dataframe to plot polygons
      neighbourhoods <- reactive({berlin_sf %>%
          filter(view  == "Neighbourhoods",
                 group == input$district)})
      
      # Dataframe with coordinates of the areas' names
      neighbourhood_names <- reactive({berlin_names %>% 
          filter(view  == "Neighbourhoods",
                 group == input$district)})
      
      # Number of colors depend on how many areas to display
      neighbourhood_colors <- reactive({
        colorspace::rainbow_hcl(n = berlin_names %>% 
                                  filter(view  == "Neighbourhoods",
                                         group == input$district) %>% 
                                  dplyr::summarize(count = n()) %>% 
                                  as.numeric())
      })
      
      # When choosing a variable (input$variable2), the color should represent the variable'distribution
      neighbourhood_colors_var <- reactive({leaflet::colorNumeric(palette = "Dark2",
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
      
      
      # District map for tab2
      output$secondary_map <- renderLeaflet({
        
        district_icons <- awesomeIconList(
          "district_properties" = makeAwesomeIcon(icon = "home", markerColor = "red"),
          "district_stations"   = makeAwesomeIcon(icon = "subway", library = "fa", markerColor = "green", iconColor = "#FFFFFF"),
          "district_sights"     = makeAwesomeIcon(icon = "eye-open", markerColor = "blue")
        )
        
        secondary_map <- leaflet() %>%
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
                              lng = ~long, lat = ~lat, label = ~lapply(leaflet, htmltools::HTML),
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
          addAwesomeMarkers(data = bahn_stations_df %>% filter(district == input$district),
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
          addMiniMap(centerFixed = berlin_center %>% t() %>% c(),
                     zoomLevelFixed = 8,
                     zoomAnimation = FALSE)
        # %>%
        #   setView(lng = berlin_names[berlin_names$view == "Districts" & berlin_names$group == input$district, "long"],
        #           lat = berlin_names[berlin_names$view == "Districts" & berlin_names$group == input$district, "lat"], 
        #           zoom = 11.5)
        
        if (gsub(" ", "_", input$variable2) == "none") {

          secondary_map %>%
            addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                        color = "grey", fillColor = neighbourhood_colors())

        } else {

          neighbourhood_colors_vars <- neighbourhood_colors_var()

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
      
      # observeEvent(input$variable2, {
      #   
      #   proxy <- leafletProxy("secondary_map") 
      #   
      #   if (input$variable2 == "none") {
      #     proxy %>%
      #       hideGroup(group = "variables") %>%
      #       addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8,
      #                   color = "grey", fillColor = neighbourhood_colors(),
      #                   group = "none")
      #   } else {
      #     
      #     neighbourhood_colors_vars <- neighbourhood_colors_var()
      #     
      #     proxy %>%
      #       hideGroup(group = c("start", "none")) %>%
      #       addPolygons(data = neighbourhoods(), weight = 1, smoothFactor = 1, fillOpacity = 0.8, color = "grey", 
      #                   fillColor = ~neighbourhood_colors_vars(listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
      #                                                                                  listings_area_summary$group == input$district, 
      #                                                                                gsub(" ", "_", input$variable2)]),
      #                   group = "variables") %>%
      #       addLegend(position = "bottomleft", pal = neighbourhood_colors_var(), 
      #                 values = listings_area_summary[listings_area_summary$view  == "Neighbourhoods" &
      #                                                  listings_area_summary$group == input$district, 
      #                                                gsub(" ", "_", input$variable2)],
      #                 labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
      #                 title = gsub(" ", "_", input$variable2),
      #                 group = "variables")
      #   }
      # 
      # })
      
      # checkboxInputs
      observeEvent({c(input$district_properties, input$district_stations, input$district_sights, input$district)}, {
        
        proxy <- leafletProxy("secondary_map")
        
        if (input$district_properties) {proxy %>% showGroup("district_properties")
        } else {proxy %>% hideGroup("district_properties")}
        
        if (input$district_stations) {proxy %>% showGroup("district_stations")
        } else {proxy %>% hideGroup("district_stations")}
        
        if (input$district_sights) {proxy %>% showGroup("district_sights")
        } else {proxy %>% hideGroup("district_sights")}
        
      })
      
      
      # Table of average or count values of the selected variable (input$variable2) for each neighbourhood
      # in the selected district (input$district)
      
      output$secondary_table <- renderTable({

        if (gsub(" ", "_", input$variable2) == "none") {

          listings_area_summary %>%
            dplyr::filter(view  == "Neighbourhoods",
                          group == input$district) %>%
            dplyr::select(id) %>%
            dplyr::rename(neighbourhoods = id)

        }  else {

          listings_area_summary %>%
            dplyr::filter(view  == "Neighbourhoods",
                          group == input$district) %>%
            tibble::column_to_rownames(var = "id") %>%
            dplyr::select(gsub(" ", "_", input$variable2)) %>%
            t()
        }
      })
      
      

      ### TAB3
      
      # For plot in tab3, check that at least one variable (input$variable3) is selected
      # If not show an error message
      price_correlation <- reactive({ 
        validate(
          need(!is.null(input$variable3), "Please select variable(s)")
        )
        listings_price_correlation %>%
          filter(variable %in% gsub(" ", "_", input$variable3)) %>%
          arrange(variable)
      })
      
      corr_color <- reactive({
        validate(
          need(!is.null(input$variable3), "Please select variable(s)")
        )
        ifelse(price_correlation()$corr > 0, "green4",
        ifelse(price_correlation()$corr < 0, "red1",
                                             "blue3"))
      })
      
      corr_margin <- reactive({
        validate(
          need(!is.null(input$variable3), "Please select variable(s)")
        )
        rep(c(seq(from = 4.5, to = 0, by = -0.1), rep(0.0, 128))[length(input$variable3)], 4)
      })
      
      corr_text_size <- reactive({
        validate(
          need(!is.null(input$variable3), "Please select variable(s)")
        )
        seq(from = 13.65, to = 5, by = -0.05)[length(input$variable3)]
      })
      
      
      # If it is, show plot
      output$corr_plot <- renderPlot({
        
        # color <-  ifelse(price_correlation()$corr > 0, "green4",
        #                  ifelse(price_correlation()$corr < 0, "red1",
        #                         "blue3"))
        
        # margin <- rep(c(seq(from = 4.5, to = 0, by = -0.1), rep(0.0, 128))[length(input$variable3)], 4)
        
        # text_size <- c(seq(from = 10, to = 5, by = -0.03), rep(5, 7))[length(input$variable3)]

        price_correlation() %>%
          ggplot(aes(x = variable, y = corr, fill = sign)) +
          geom_bar(stat="identity") +
          scale_fill_manual("sign", values = c("positive" = "green4", "negative" = "red1", "null" = "blue3"),
                            guide=FALSE) +
          coord_flip() +
          labs(x = "Variable(s)",
               y = "Correlation") +
          theme(axis.text.y = element_text(colour = corr_color(), size = corr_text_size()), 
                plot.title = element_text(hjust = 0.5, size = 17),
                plot.margin = unit(corr_margin(), "cm"))
        
      })
      
        
}

shinyApp(ui, server)
