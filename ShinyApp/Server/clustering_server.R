#########################################################################
# Clustering Server-file ################################################
# Author: Silvia Ventoruzzo #############################################
#########################################################################

clustering_server = function(input, output, session) {
  
  output$cluster_ui <- renderUI({
    
    text1  = "In this tab you can cluster Airbnb properties in Berlin. The process is the following:"
    text2  = "Choose the number of the clusters according to the elbow-method"
    text2a = "Choose the number of clusters to test"
    text2b = "Calculate for each number the percentage of the total variance explained"
    text2c = "Plot the total variance explained against the number of clusters"
    text2d = "Choose the number of clusters where adding another does not add sufficient informatoin"
    text3  = "Cluster the properties with the selected number of clusters"
    text4  = "Plot the clusters"
  
    intern = paste(text2, "<ul><li>", sep = "<br/>") %>%
      paste(text2a, sep = "") %>%
      paste(text2b, sep = "</li><li>") %>%
      paste(text2c, sep = "</li><li>") %>%
      paste(text2d, sep = "</li><li>") %>%
      paste("",   sep = "</li></ul>")
    
    extern = paste(text1, "<ul><li>", sep = "<br/>") %>%
      paste(intern, sep = "") %>%
      paste(text3, sep = "</li><li>") %>%
      paste(text4, sep = "</li><li>") %>%
      paste("",   sep = "</li></ul>")
    
    html = HTML(extern)
    
  })
  

    ## Panel 1 (Find number of clusters)
    
    output$ctest_text <- renderText({
      "Before clustering the airbnb properties, we need to find out what is the optimal number of clusters.
      We use the elbow method, which means that from plot showing the percentage of the total variance
      explained for that number of clusters we choose the number before the increase becomes too flat."
    })
    
    tot_var_explained <- eventReactive(input$ctest_start, {
      
      clusters = seq(input$ctest[1], input$ctest[2], 1)
      
      cluster_count = input$ctest[2] - input$ctest[1] + 1
      
      tve = data.frame(clusters = clusters,
                       tve      = rep(NA, cluster_count))
      
      clk = list()
      
      n = 1
      
      for (k in seq(input$ctest[1], input$ctest[2], 1)) {
        
        clk[[k-1]] = kmeans(listings_scaled, centers = k, iter.max = 20)
        tve$tve[k-1] = 1-clk[[k-1]]$tot.withinss/clk[[k-1]]$totss
        
        updateProgressBar(session = session, id = "ctest_progress",
                          value = (n/cluster_count*100), total = 100,
                          title = paste(n, "of", cluster_count))
        n = n + 1
      }
      
      return(tve)
    })
    
    
    output$tve = renderPlot({
      
      # Dependency on input$ctest_start
      if (input$ctest_start == 0) return()
      
      tot_var_explained() %>%
        ggplot(aes(x = clusters, y = tve)) +
        geom_line(color = "grey") +
        geom_point(color = "red") +
        scale_x_continuous(breaks = seq(input$ctest[1], input$ctest[2], 2)) +
        theme_bw()
      
    })
    
    ## Panel 2 (Actual clustering)
    
    output$cnum_ui <- renderUI({
      text <- "Select the optimal number of clusters you found in the previous tab and run the clustering 
      function. In this case we use k-means, which splits the data into the given number of clusters
      in such a way as to minimize the within-cluster sum of squares. For detailed information on
      this and other clustering algorithms you can read here:"
      url <- a("Data Clustering: A Review", 
               href="http://delivery.acm.org/10.1145/340000/331504/p264-jain.pdf?ip=141.20.217.40&id=331504&acc=PUBLIC&key=2BA2C432AB83DA15%2E04C410D892D772C0%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35&__acm__=1551719946_9d82c9a89fd8cd69cfa3d310ea27bc7f")
      tagList(text, url)
    })
    
    # REACTIVE ELEMENTS
    
    # k-means
    k_means = eventReactive(input$cnum_start, {
      
      kmeans(listings_scaled, centers = input$cnum, iter.max = 20)
      
    })
    
    # clusters
    clusters <- eventReactive(input$cnum_start, {
      
      k_means()$cluster %>% as.factor()
      
    })
    
    # Plot of cluster on coordinates
    output$clustercoord = renderPlot({
      
      # Dependency on input$ctest_start
      if (input$cnum_start == 0) { return() }
      
      ggplot() +
        geom_sf(data = berlin_sf %>% dplyr::filter(view == "Districts"),
                show.legend = FALSE, color = "black") +
        coord_sf(datum = NA) +
        geom_point(data = listings, aes(x = long, y = lat, color = clusters()), alpha = 0.5) +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(color = "clusters") +
        theme_void()
      
    })
    
    # Plot of clusters on Principal Components
    output$clusterpc = renderPlot({
      
      # Dependency on input$ctest_start
      if (input$cnum_start == 0) { return() }
      
      ggplot2::autoplot(listings_pc, data = k_means(), colour = "cluster") +
        labs(color = "clusters") +
        theme_bw()
      
    })

}