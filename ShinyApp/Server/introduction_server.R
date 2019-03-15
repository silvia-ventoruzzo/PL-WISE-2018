#########################################################################
# Introduction Server-file ##############################################
# Author: Silvia Ventoruzzo #############################################
#########################################################################

introduction_server = function(input, output, session) {
  
    output$intro_ui = renderUI({
      
      text1 = "This ShinyApp was developed by "
      url1  = a("Silvia Ventoruzzo", 
                href="https://www.linkedin.com/in/silvia-ventoruzzo-1a042110a/")
      text2 = " for the university course 'Statististical Programming Languages' at the "
      url2  = a("Humboldt-University of Berlin", 
                href="https://www.hu-berlin.de/")
      text3 = " in the Winter Termin 2018/19. It shows some Exploratory Data Analysis of the Airbnb properties
      in Berlin updated at the 14. January 2019."
      
      part1 = paste(text1, url1, text2, url2, text3, sep = "") %>%
        paste("<br/>", sep = "<br/>")
      html1 = HTML(part1)
      
      text4 = "The data for this project has been downloaded from the following sources:"
      url4  = a("Inside Airbnb", 
                href="http://insideairbnb.com/")
      url5  = a("Statistics Office of Berlin-Brandenburg", 
                href="https://www.statistik-berlin-brandenburg.de/produkte/opendata/geometrienOD.asp?Kat=6301")
      url6  = a("Geofabrik", 
                href="http://download.geofabrik.de/europe/germany/berlin.html")
      
      part2 = paste(text4, "<ul><li>", sep = "<br/>") %>%
        paste(url4, sep = "") %>%
        paste(url5, sep = "</li><li>") %>%
        paste(url6, sep = "</li><li>") %>%
        paste("",   sep = "</li></ul><br/>")
      html2 = HTML(part2)
      
      text5 = "The complete code for this project can be found on"
      url7  = a("GitHub", 
                href="https://github.com/silvia-ventoruzzo/SPL-WISE-2018")
      text6 = "."
      
      part3 = paste(text5, url7, sep = " ") %>%
        paste(text6, sep = "")
      html3  = HTML(part3)
      
      tagList(html1, html2, html3)
      
      
    })
    
    output$intro_maps_text = renderText({
      "Map of Berlin or one of its district and variable distribution."
    })
    
    output$intro_price_text = renderText({
      "Variable correlation with price and linear regression on price."
    })
    
    output$intro_cluster_text = renderText({
      "Clustering of Airbnb properties."
    })
    
    # Connections to other tabs
    observeEvent(input$switch_maps, {
      updateTabItems(session, inputId = "tabs", selected = "maps")
    })
    
    observeEvent(input$switch_price, {
      updateTabItems(session, inputId = "tabs", selected = "price")
    })
    
    observeEvent(input$switch_cluster, {
      updateTabItems(session, inputId = "tabs", selected = "cluster")
    })

}