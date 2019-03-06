#########################################################################
# Price Correlation Server-file #########################################
# Author: Silvia Ventoruzzo #############################################
#########################################################################

price_corr_server = function(input, output, session) {
  
  ## TEXT
  
  output$price_text = renderText({
    "In this tab you can choose one or multiple variables and check their correlation with price. Moreover,
    a linear regression on price is run with the selected variable(s) as regressors. You can also check the
    coefficients and the residuals for this mode."
  })
  
  # For plot in tab3-panel1, check that at least one variable (input$variable3) is selected
  # If not show an error message
  
  ## REACTIVE ELEMENTS
  
  price_correlation = reactive({ 
    validate(
      need(!is.null(input$variable3), "Please select variable(s)")
    )
    listings_price_correlation %>%
      filter(variable %in% gsub(" ", "_", input$variable3)) %>%
      mutate(corr = abs(corr)) %>%
      arrange(variable)
  })
  
  corr_color = reactive({
    validate(
      need(!is.null(input$variable3), "Please select variable(s)")
    )
    ifelse(price_correlation()$sign == "positive", "green4",
           ifelse(price_correlation()$sign == "negative", "red1",
                  "blue3"))
  })
  
  corr_margin = reactive({
    validate(
      need(!is.null(input$variable3), "Please select variable(s)")
    )
    c(seq(from = 4.5, to = 0, by = -0.1), rep(0.0, 128))[length(input$variable3)]
    # rep(c(rep(0.0, 128), seq(from = 4.5, to = 0, by = -0.1))[length(input$variable3)], 4)
  })
  
  corr_text_size = reactive({
    validate(
      need(!is.null(input$variable3), "Please select variable(s)")
    )
    seq(from = 13.65, to = 5, by = -0.05)[length(input$variable3)]
  })
  
  ## PLOT
  
  output$corr_plot = renderPlot({
    
    price_correlation() %>%
      ggplot(aes(x = variable, y = corr, fill = sign)) +
      geom_bar(stat="identity") +
      scale_fill_manual("sign", values = c("positive" = "green4", "negative" = "red1", "null" = "blue3")) +
      coord_flip() +
      labs(x = ifelse(length(input$variable3) == 1, "variable", "variables"),
           y = "correlation") +
      theme_bw() +
      theme(axis.text.y     = element_text(colour = corr_color(), size = corr_text_size()), 
            axis.text.x     = element_text(size = rel(1.5)),
            axis.title.x    = element_text(size = rel(1.5)),
            axis.title.y    = element_text(size = rel(1.5)),
            legend.text     = element_text(size = rel(1.2)),
            legend.title    = element_text(size = rel(1.5)),
            plot.title      = element_text(hjust = 0.5, size = 17),
            plot.margin     = unit(c(0, corr_margin(), 0, corr_margin()), "cm"),
            legend.position = "bottom") +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) 
    
  })
  
}
