ui = dashboardPage(
    skin = "red",
    dashboardHeader(title = "Airbnb properties in Berlin"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Intro",      tabName = "intro",      icon = icon("play-circle")),
        menuItem("Maps",       tabName = "maps",       icon = icon("map")),
        menuItem("Price",      tabName = "price",      icon = icon("dollar")),
        menuItem("Clustering", tabName = "cluster",    icon = icon("object-group"))
        )
      ),
    dashboardBody(
      tabItems(
        
      tabItem(tabName = "intro",
          fluidRow(
            box(
              title  = "Welcome to the ShinyApp on Airbnb properties in Berlin!",
              width  = 12,
              status = "danger", 
              uiOutput("intro_ui"),
              br(),
              column(width = 4,
                     h4("Maps Tab"),
                     textOutput("intro_maps_text"),
                     br(),
                     actionButton("switch_maps", "Go to Maps Tab")),
              column(width = 4,
                     h4("Price Tab"),
                     textOutput("intro_price_text"),
                     br(),
                     actionButton("switch_price", "Go to Price Tab")),
              column(width = 4, 
                     h4("Clustering Tab"),
                     textOutput("intro_cluster_text"),
                     br(), br(),
                     actionButton("switch_cluster", "Go to Clustering Tab")))
          )
      ), 
    
      ### TAB2 (Maps)
      tabItem(tabName = "maps",
          fluidRow(
            tabBox(id = "berlin",
                   width  = 12,
                   tabPanel(title = "Berlin map",
                            textOutput("berlin_text"),
                            br(),
                            box(width = 12,
                                solidHeader=TRUE,
                                column(width = 6,
                                       selectInput(inputId = "view",
                                                   label   = "Choose view",
                                                   choices = berlin_sf %>%
                                                     as.data.frame() %>%
                                                     select(view) %>%
                                                     unique() %>%
                                                     t() %>%
                                                     c() %>%
                                                     setdiff("Neighbourhoods"))),
                                column(width = 6,
                                       selectInput(inputId = "variable1",
                                                   label   = "Choose variable",
                                                   choices = c("none", listings_area_summary %>%
                                                                 colnames() %>%
                                                                 setdiff(c("id", "view", "group")) %>%
                                                                 gsub("_", " ", .))))
                                ),
                            box(width = 12,
                                solidHeader=TRUE,
                                column(width = 4,
                                       checkboxInput(inputId = "main_properties",
                                                     label   = "Display properties",
                                                     value   = FALSE)),
                                column(width = 4,
                                       checkboxInput(inputId = "main_stations",
                                                     label   = "Display train/subway stations",
                                                     value   = FALSE)),
                                column(width = 4,
                                       checkboxInput(inputId = "main_sights",
                                                     label   = "Display top 10 attractions",
                                                     value   = FALSE))
                                ),
                            box(width = 12,
                                solidHeader = TRUE,
                                h4(textOutput("title_main_map")),
                                leafletOutput("main_map") %>% withSpinner()
                                ),
                            box(width = 12,
                                solidHeader = TRUE,
                                column(
                                  width = 4,
                                  h4(textOutput("title_main_table")),
                                  tableOutput("main_table1"),
                                  tableOutput("main_table2")
                                ),
                                column(
                                  width = 8,
                                  h4(textOutput("title_main_plot")),
                                  plotOutput("main_plot")
                                  )
                                )
                            ),
                   
                   tabPanel(title = "District map",
                            textOutput("district_text"),
                            br(),
                            box(
                              width = 12,
                              solidHeader=TRUE,
                              column(
                                width = 6,
                                selectInput(inputId  = "district",
                                            label    = "Choose district",
                                            choices  = berlin_names %>%
                                                          filter(view == "Districts") %>%
                                                          select(id) %>% unique() %>% t() %>% c(),
                                            selected = "Mitte")),
                                column(
                                  width = 6,
                                  selectInput(inputId = "variable2",
                                              label   = "Choose variable",
                                              choices = c("none", listings_area_summary %>%
                                                           colnames() %>%
                                                           setdiff(c("id", "view", "group")) %>%
                                                           gsub("_", " ", .))))
                            ),
                            box(width = 12,
                                solidHeader=TRUE,
                                column(width = 4,
                                       checkboxInput(inputId = "district_properties",
                                                     label   = "Display properties",
                                                     value   = FALSE)),
                                column(width = 4,
                                       checkboxInput(inputId = "district_stations",
                                                     label   = "Display train/subway stations",
                                                     value   = FALSE)),
                                column(width = 4, 
                                       checkboxInput(inputId = "district_sights",
                                                     label   = "Display top 10 attractions",
                                                     value   = FALSE))

                                ),
                            box(width       = 12,
                                align       = "center",
                                solidHeader = TRUE,
                                h4(textOutput("title_secondary_map")),
                                leafletOutput("secondary_map") %>% withSpinner()
                                ),
                            box(width       = 12,
                                align       = "center",
                                solidHeader = TRUE,
                                column(
                                  width = 4,
                                  h4(textOutput("title_secondary_table")),
                                  tableOutput("secondary_table")
                                ),
                                column(
                                  width = 8,
                                  h4(textOutput("title_secondary_plot")),
                                  plotOutput("secondary_plot")
                                )
                            )
                            )
    
              )
          )    
  ),
  
  ### TAB3 (Price)
  tabItem(tabName = "price",
          fluidRow(
            box(
              status = "danger",
              width  = 12,
              column(
                width = 6,
                textOutput(outputId = "price_text")
                ),
              column(
                width = 6,
                pickerInput(inputId  = "variable3", label = "Choose variable(s)",
                            choices  = listings_price_correlation %>% 
                              select(variable) %>% 
                              unique() %>% 
                              t() %>% 
                              c() %>% 
                              gsub("_", " ", .),
                            multiple = TRUE,
                            selected = listings_price_correlation %>% 
                              select(variable) %>%
                              unique() %>% 
                              t() %>% 
                              c() %>% 
                              gsub("_", " ", .),
                            options  = list(`actions-box` = TRUE))
                )
                

                )
            ),
          fluidRow(
            tabBox(id = "price_box",
                   width  = 12,
                   tabPanel(
                     title = "Correlation with price",
                     plotOutput(outputId = "corr_plot")
                      ),
                   tabPanel(
                     title = "Linear regression on price",
                     tabBox(id = "lm_model",
                            width = 12,
                            tabPanel(
                              title = "Model",
                              column(
                                width = 6,
                                h4("R-Squared of the model"),
                                textOutput(outputId = "price_lm_r2")
                              ),
                              column(
                                width = 6,
                                h4("Choose variable to plot against price."),
                                selectInput(inputId = "variable3_2",
                                            label   = NULL,
                                            choices = NULL)
                              ),
                              plotOutput(outputId = "price_lm_scatterplot")
                            ),
                            tabPanel(
                              title = "Coefficients",
                              box(width = 12,
                                  solidHeader = TRUE,
                                  align = "center",
                                  h4("Coefficients of the linear regression on price"),
                                  tableOutput(outputId = "price_lm_coefficients")
                                  )
                            ),
                            tabPanel(
                              title = "Residuals",
                              box(width = 12,
                                  solidHeader = TRUE,
                                  align = "center",
                                  h4("Summary statistics of the residuals"),
                                  tableOutput(outputId = "price_lm_residuals")
                                 ),
                              box(width = 12,
                                  solidHeader = TRUE,
                                  align = "center",
                                  h4("Residuals plots"),
                                  column(
                                    width = 6,
                                    plotOutput(outputId = "price_lm_resplot") %>% withSpinner()
                                    ),
                                  column(
                                    width = 6,
                                    plotOutput(outputId = "price_lm_qqplot") %>% withSpinner()
                                    )
                                  )
                            )
                         )
                   )
                )
          )
          ),
  ### TAB4 (Clustering)
  tabItem(tabName = "cluster",
          fluidRow(
            box(status = "danger",
                width  = 12,
                # align  = "center",
                textOutput("cluster_text")),
            tabBox(id = "cluster_test",
                width  = 12,
                tabPanel(title = "Find number of clusters",
                         textOutput("ctest_text"),
                         br(),
                         sliderInput(inputId  = "ctest",
                                     label = "Choose which number of clusters to test",
                                     min = 2, max = 96, value = c(2, 40), step = 1,
                                     width = "100%"),
                         actionButton("ctest_start", "Calculate"),
                         progressBar(id = "ctest_progress",
                                     status = "danger",
                                     value = 0, total = 100,
                                     display_pct = TRUE),
                         plotOutput(outputId = "tve")
                         ),
                
                tabPanel(title = "Cluster Airbnb properties",
                         uiOutput("cnum_ui"),
                         br(),
                         numericInput(inputId  = "cnum",
                                      label = "Choose number of clusters for clustering",
                                      min = 2, max = 96, value = 12, step = 1,
                                      width = "30%"),
                         actionButton("cnum_start", "Calculate"),
                         plotOutput(outputId = "clustering")
                         )
                )
        )
        
        
      )
    )
  )
)