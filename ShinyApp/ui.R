ui <- dashboardPage(
  skin = "red",
dashboardHeader(title = "Airbnb listings in Berlin"
                # titleWidth = 350,
                # Set height of dashboardHeader
                # tags$li(class = "dropdown",
                #         tags$style(".main-header {max-height: 100px}"),
                #         tags$style(".main-header .logo {height: 100px;}"),
                #         tags$style(".sidebar-toggle {height: 100px; padding-top: 1px !important;}"),
                #         tags$style(".navbar {min-height:100px !important}"))
                ),
dashboardSidebar(
  # width = 350,
  # Adjust the sidebar
  # tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
  
  sidebarMenu(
    id = "tabs",
    menuItem("Berlin map", tabName = "berlin", icon = icon("globe")),
    menuItem("District map", tabName = "district", icon = icon("map")),
    menuItem("Price", tabName = "price_corr", icon = icon("dollar"))
  )
),
dashboardBody(
  # Also add some custom CSS to make the title background area the same
  # color as the rest of the header.
  # tags$head(tags$style(HTML('
  #       .skin-red .main-header .logo {
  #         background-color: #d7395;
  #       }
  #       .skin-red .main-header .logo:hover {
  #         background-color: #d73925;
  #       }
  #     '))),
  
  tabItems(
    
    ### TAB1
    tabItem(tabName = "berlin",
            fluidRow(
              
              box(
                title = "Display choice",
                status = "danger",
                selectInput(inputId = "view", label = NULL,
                            choices = berlin_sf %>%
                                      as.data.frame() %>% 
                                      select(view) %>% 
                                      unique() %>% 
                                      t() %>% c() %>%
                                      setdiff("Neighbourhoods")),
                width = 6),
              box(
                title = "Variable choice",
                status = "danger",
                # selectizeInput(inputId = "variable1", label = NULL,
                #             choices = listings_district_summary %>%
                #               colnames() %>%
                #               setdiff("district"),
                #             options = list(
                #               placeholder = 'Please select an option below',
                #               onInitialize = I('function() { this.setValue(""); }')
                #             )),
                            
                  selectInput(inputId = "variable1", label = NULL,
                              choices = c("none", listings_area_summary %>%
                                                  colnames() %>%
                                                  setdiff(c("id", "view", "group")) %>%
                                                  gsub("_", " ", .))),
                width = 6)
              ),
            fluidRow(  
              box(
                status = "danger",
                column(4, checkboxInput(inputId = "main_properties",
                                        label   = "Display properties",
                                        value   = FALSE)),
                column(4, checkboxInput(inputId = "main_stations",
                                        label   = "Display train/subway stations",
                                        value   = FALSE)),
                column(4, checkboxInput(inputId = "main_sights",
                                        label   = "Display top 10 attractions",
                                        value   = FALSE)),
                # sliderInput(inputId = "price",
                #             label   = "Choose price range",
                #             min = min(listings$price),
                #             max = max(listings$price),
                #             value = c(min(listings$price), max(listings$price))),
                width = 12)
              ),
            fluidRow(  
              box(title = textOutput("title_main_map"),
                  status = "success",
                  leafletOutput("main_map"),
                  width = 12,
                  align = "center")
            ),
            fluidRow(
              box(title = textOutput("title_main_table"),
                  status = "info",
                  tableOutput("main_table"),
                  width = 12,
                  align = "center")
            )
    
  ),
  
  ### TAB2
  tabItem(tabName = "district",
          fluidRow(
            box(
              title = "District choice",
              status = "danger",
              selectInput(inputId = "district", label = NULL,
                          choices = berlin_names %>%
                            filter(view == "Districts") %>%
                            select(id) %>% unique() %>% t() %>% c(),
                          selected = "Mitte"),
              width = 6),
            
            box(
              title = "Variable choice",
              status = "danger",
              selectInput(inputId = "variable2", label = NULL,
                          choices = c("none", listings_area_summary %>%
                                        colnames() %>%
                                        setdiff(c("id", "view", "group")) %>%
                                        gsub("_", " ", .))),
              width = 6)
          ),
          fluidRow(  
            box(
              status = "danger",
              column(4, checkboxInput(inputId = "district_properties",
                                      label   = "Display properties",
                                      value   = FALSE)),
              column(4, checkboxInput(inputId = "district_stations",
                                      label   = "Display train/subway stations",
                                      value   = FALSE)),
              column(4, checkboxInput(inputId = "district_sights",
                                      label   = "Display top 10 attractions",
                                      value   = FALSE)),
              # sliderInput(inputId = "price",
              #             label   = "Choose price range",
              #             min = min(listings$price),
              #             max = max(listings$price),
              #             value = c(min(listings$price), max(listings$price))),
              width = 12
            )),
          fluidRow(  
            box(title = textOutput("title_secondary_map"),
                status = "success",
                leafletOutput("secondary_map"),
                width = 12,
                align = "center")
          ),
          fluidRow(
            box(title = textOutput("title_secondary_table"),
                status = "info",
                tableOutput("secondary_table"),
                width = 12,
                align = "center")
          )
          
  ),
  
  ### TAB3
  tabItem(tabName = "price_corr",
          fluidRow(
            box(title = "Variable(s)",
                status = "danger",
                pickerInput(inputId = "variable3", label = NULL,
                            choices = listings_price_correlation %>% 
                              select(variable) %>% unique() %>% t() %>% c() %>% gsub("_", " ", .),
                            multiple = TRUE,
                            selected = listings_price_correlation %>% 
                              select(variable) %>% unique() %>% t() %>% c() %>% gsub("_", " ", .),
                            options = list(`actions-box` = TRUE)),
                width = 6,
                align = "center"
            )
          ),
          fluidRow(
            box(title = "Price correlation(s)",
                status = "warning",
                plotOutput(outputId = "corr_plot"),
                           width = 12,
                           align = "center")
          )
      )
  )
)
)


