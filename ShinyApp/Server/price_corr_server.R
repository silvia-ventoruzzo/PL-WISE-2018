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
      filter(variable %in% gsub(" ", "_", input$variable3))
  })
  
  ## PLOT
  
  output$corr_plot = renderPlot({
    
    correlation_plot(price_correlation()) 
    
  })
  
}
