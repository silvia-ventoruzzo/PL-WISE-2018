var_avg_map = function(summary_df, var_name,
                       polygon_sf, polygon_names_df, polygon_names_var,
                       bins = TRUE, nsplits = 4, pretty = FALSE, zoom_level = 10,
                       title = paste("Average of", var_name, sep = " ")) {
  
  
  if (bins) {
    
    colors = colorBin(topo.colors(nsplits), summary_df[, var_name], bins = nsplits, pretty = pretty)
    
  } else {
    
    if (pretty) {
      
      stop("Parameter pretty not used in colorQuantile.")
      
    }
    
    colors = colorQuantile(topo.colors(nsplits), summary_df[, var_name], bins = nsplits)
    
  }
  
  map = leaflet() %>% 
    addTiles() %>%
    addPolygons(data = polygon_sf, weight = 1, smoothFactor = 1, fillOpacity = 0.8,
                color = "grey", 
                fillColor = ~colors(summary_df[, var_name])) %>%
    addLabelOnlyMarkers(data = polygon_names_df,
                        lng = ~long, lat = ~lat, label = ~lapply(polygon_names_df[,polygon_names_var], htmltools::HTML),
                        labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                    textOnly = TRUE, textsize = 20,
                                                    style = list("color" = "black"))) %>%
    addLegend(position = "bottomleft", pal = colors, 
              values = summary_df[, var_name],
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
              title = title)
  
  return(map)
  
}