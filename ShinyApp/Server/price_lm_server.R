#########################################################################
# Price Server-file #####################################################
# Author: Silvia Ventoruzzo #############################################
#########################################################################

price_lm_server = function(input, output, session) {

    ## REACTIVE ELEMENTS

    # For plot in tab3-panel1, check that at least one variable (input$variable3) is selected
    # If not show an error message
    price_lm_df = reactive({ 
      validate(
        need(!is.null(input$variable3), "Please select variable(s)")
      )
      
      listings_price %>%
        dplyr::select(price, gsub(" ", "_", input$variable3))
      
    })
    
    price_lm = reactive({
      
      lm(price ~ ., data = price_lm_df())
      
    })
    
    ## TEXT WITH R-SQUARED
    
    output$price_lm_r2 = renderText({
      
      summary(price_lm())$r.squared %>% round(4)
      
    })
    
    ## TABLE WITH COEFFICIENTS
    
    output$price_lm_coefficients = renderTable({
      
      coefficients = data.frame(
        variable = c("(Coefficient)", gsub(" ", "_", input$variable3)),
        coefficient = price_lm()$coefficients) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(num = 1:nrow(.)) %>%
        dplyr::mutate_if(is.numeric, function(x) round(x, 4))
      
      significance = data.frame(
        significant = ifelse(summary(price_lm())$coefficients[,4] < 0.5, 
                             TRUE, FALSE)) %>%
        dplyr::mutate(num = 1:nrow(.))
      
      lm = coefficients %>%
        dplyr::inner_join(significance, by = "num") %>%
        dplyr::select(-num)
      
      return(lm)
      
    })
    
    ## SUMMARY TABLE OF RESIDUALS
    
    output$price_lm_residuals = renderTable({
      
      lm = data.frame(
        residuals = price_lm()$residuals
      )
      
      summary_stat_num(df  = lm,
                       var = "residuals")
      
    })
    
    ## FITTED VS ACTUAL PLOT
    
    output$price_lm_resplot = renderPlot({
      
      ggplot(price_lm(), aes(x = .fitted, y = .resid)) + 
        geom_point() +
        geom_smooth(method = lm) +
        geom_hline(yintercept=0, col="red", linetype="dashed") +
        xlab("Fitted values") +
        ylab("Residuals") +
        ggtitle("Residual vs Fitted Plot") +
        theme_bw()
      
    })
    
    ## QQ PLOT RESIDUALS
    
    output$price_lm_qqplot = renderPlot({
      
      gg_qqplot(price_lm()) +
        theme_bw()
      
    })

}