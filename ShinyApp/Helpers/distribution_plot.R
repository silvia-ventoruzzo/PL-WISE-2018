distribution_plot = function(df, var_name, hide_outliers = FALSE, tilt_text_x = FALSE) {
  
  variable = sym(var_name)
  
  # Plot for numeric variables
  if (is.numeric(df[, var_name])) {
    
    numvar_plot = df %>%
      dplyr::transmute(
                var            = !!variable,
                median         = median(var),
                mean           = mean(var),
                `1Q`           = quantile(var, probs = 0.25),
                `3Q`           = quantile(var, probs = 0.75),
                IQR            = IQR(var),
                upper          = (IQR*3) + `3Q`,
                lower          = `1Q` - (IQR*3),
                outlier_upper  = ifelse(var > unique(upper), 1, 0),
                outlier_lower  = ifelse(var < unique(lower), 1, 0),
                outlier        = ifelse(outlier_upper == 1 | outlier_lower == 1, 1, 0),
                count_upper    = sum(outlier_upper),
                count_lower    = sum(outlier_lower)
                )
    
    if (hide_outliers) {
      
      plot = numvar_plot %>%
        dplyr::filter(outlier == 0) %>%
        ggplot() +
        geom_density(aes(x = var), fill = "green4", color = "green4") +
        geom_vline(aes(xintercept = unique(median)),
                   color = "blue", linetype = "dashed") +
        annotate(geom = "text", x = (unique(numvar_plot$median) + 2.6), y = 0,
                 label = "Median",
                 colour = "blue",
                 angle = 90, hjust = 0) +
        geom_vline(aes(xintercept = unique(mean)),
                   color = "black", linetype = "dashed") +
        annotate(geom = "text", x = (unique(numvar_plot$mean) + 2.6), y = 0,
                 label = "Mean",
                 colour = "black",
                 angle = 90, hjust = 0) +
        geom_vline(aes(xintercept = `1Q`),
                   color = "red", linetype = "dashed") +
        annotate(geom = "text", x = (unique(numvar_plot$`1Q`) + 2.6), y = 0,
                 label = "1st Quantile",
                 colour = "red",
                 angle = 90, hjust = 0) +
        geom_vline(aes(xintercept = `3Q`),
                   color = "red", linetype = "dashed") +
        annotate(geom = "text", x = (unique(numvar_plot$`3Q`) + 2.6), y = 0,
                 label = "3rd Quantile",
                 colour = "red",
                 angle = 90, hjust = 0) +
        labs(caption = paste("Outliers not displayed:",
                             numvar_plot$count_lower, "values under lower outer fence and",
                             numvar_plot$count_upper, "values over upper outer fence",
                             sep = " "),
             title = paste("Density of the variable", variable, sep = " "),
             x = var_name) +
        theme_bw() +
        theme(axis.text.x = element_text(size = rel(1.2)),
              axis.text.y = element_text(size = rel(1.2)),
              axis.title.x = element_text(size = rel(1.2)),
              axis.title.y = element_text(size = rel(1.2)))
    } else {
      
      plot = numvar_plot %>%
        ggplot() +
        geom_density(aes(x = var), fill = "green4", color = "green4") +
        geom_vline(aes(xintercept = unique(median)),
                   color = "blue", linetype = "dashed") +
        geom_vline(aes(xintercept = unique(mean)),
                   color = "black", linetype = "dashed") +
        geom_vline(aes(xintercept = `1Q`),
                   color = "red", linetype = "dashed") +
        geom_vline(aes(xintercept = `3Q`),
                   color = "red", linetype = "dashed") +
        labs(x     = var_name,
             title = paste("Density of the variable", variable, sep = " ")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = rel(1.2)),
              axis.text.y = element_text(size = rel(1.2)),
              axis.title.x = element_text(size = rel(1.2)),
              axis.title.y = element_text(size = rel(1.2)))
      
    }
    
    if (tilt_text_x) {
      
      plot = plot +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
    }
    
  # Plot for categorical variables  
  } else if (is.character(df[, var_name]) | 
             is.factor(df[, var_name]) | 
             is.logical(df[, var_name])) {
    
    factvar_plot = df %>%
      dplyr::rename(var = !!variable) %>%
      dplyr::group_by(var) %>%
      dplyr::summarize(count = n()) 
    
    plot = ggplot(factvar_plot) +
      geom_bar(aes(x = var, y = count), fill = "green4", stat = "identity") +
      labs(x     = variable,
           title = paste("Histogram of the variable", variable, sep = " ")) +
      theme_bw() +
      theme(axis.text.x = element_text(size = rel(1.2)),
            axis.text.y = element_text(size = rel(1.2)),
            axis.title.x = element_text(size = rel(1.2)),
            axis.title.y = element_text(size = rel(1.2)))
    
    if (tilt_text_x) {
      
      plot = plot +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      
    }
    

    if (hide_outliers) {
      
      stop("hide_outliers cannot be TRUE for categorical variables.")
      
    }
    
  } else {
    
    stop("Check what type of variable you area dealing with.")
    
  }
  
  plot
  
}