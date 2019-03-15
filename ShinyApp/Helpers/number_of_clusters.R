# Calculate plot of tve to choose number of clusters for k-means clustering
number_of_clusters = function(scaled_df, min = 2, max,
                              iter_max = 10, plot_breaks) {
  # Set seed for reproducibility
  set.seed(900114)
  # Values to test
  k_values = min:max
  # Empty dataframe for the total variance explained
  tve = data.frame(clusters = k_values,
                   tve      = rep(NA, length(k_values)))
  # Empty list for the kmeans objects
  clk = list()
  # Loop through the possible values of k
  for (k in k_values) {
    # Calculate k-means for each k
    clk[[k-1]] = kmeans(scaled_df, centers = k, iter.max = iter_max)
    # Save the number of clusters
    names(clk)[k-1] = paste(k, "clusters", sep = " ")
    # Calculate percentage of total variance explained
    tve$tve[k-1] = 1-clk[[k-1]]$tot.withinss/clk[[k-1]]$totss
    # Print process
    print(paste("k-means with", k, "clusters done", sep = " "))
  }
  
  # Plot tve against k values
  plot = ggplot(data = tve, aes(x = clusters, y = tve)) +
    geom_line(color = "grey") +
    geom_point(color = "red") +
    scale_x_continuous(breaks = plot_breaks) +
    labs(x     = "number of clusters",
         y     = "% of tve",
         title = paste("Plot of total variance explained for k from", min, "to", max, sep = " ")) +
    theme_bw() +
    theme(axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          axis.title.x = element_text(size = rel(1.2)),
          axis.title.y = element_text(size = rel(1.2)))
  
  return(plot)
}