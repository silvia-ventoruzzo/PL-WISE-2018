correlation_plot = function(corr_df, variables = corr_df$variable, variables_original = TRUE) {
  
    correlation = corr_df %>%
      dplyr::mutate(sign        = ifelse(correlation > 0, "positive",
                                  ifelse(correlation < 0, "negative",
                                                          "null")),
                    correlation = abs(correlation)) %>%
      dplyr::arrange(variable)
  
    if (all(correlation$variable == corr_df$variable)) {
      
      # If variables are listed with their names from the original dataframe (before dummies)
      if (variables_original) {
      
      correlation = correlation %>%
        dplyr::filter(grepl(paste(variables, collapse = "|"), corr_df$variable))
      
      # If variables are listed with their names from corr_df
      } else {
        
        correlation = correlation %>%
          dplyr::filter(variable %in% variables)
        
      }
      
    }
  
    corr_plot = correlation %>%
      ggplot(aes(x = variable, y = correlation, fill = sign)) +
      geom_bar(stat="identity") +
      scale_fill_manual("sign", values = c("positive" = "green4", "negative" = "red1", "null" = "blue3")) +
      coord_flip() +
      labs(x = "variables", y = "correlation", title = "Correlation with price") +
      theme_bw() +
      theme(axis.text.y     = element_text(color = ifelse(correlation$sign == "positive", "green4",
                                                   ifelse(correlation$sign == "negative", "red1",
                                                                                          "blue3"))),
            axis.text.x     = element_text(size = rel(1.2)),
            axis.title.x    = element_text(size = rel(1.2)),
            axis.title.y    = element_text(size = rel(1.2)),
            legend.text     = element_text(size = rel(1.2)),
            legend.position = "bottom") +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) 
    
    return(corr_plot)
    
}