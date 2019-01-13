p_load(shiny)
p_load(shinydashboard)
library(leaflet)
p_load(shinyWidgets)

ui <- dashboardPage(
dashboardHeader(title = "Explorative Data Analysis of Airbnb listings in Berlin",
                titleWidth = 450),
dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Berlin map", tabName = "berlin", icon = icon("map")),
    menuItem("District map", tabName = "district", icon = icon("map")),
    menuItem("Price", tabName = "price", icon = icon("dollar"))
  )
),
dashboardBody(
  
  tabItems(
    
    ### TAB1
    tabItem(tabName = "berlin",
            fluidRow(
              
              box(
                title = "Display choice",
                selectInput(inputId = "view", label = NULL,
                            choices = berlin_sf %>% as.data.frame() %>% select(view) %>% unique() %>% t() %>% c()),
                width = 6),
              box(
                title = "Variable choice",
                # selectizeInput(inputId = "variable1", label = NULL,
                #             choices = listings_district_summary %>%
                #               colnames() %>%
                #               setdiff("district"),
                #             options = list(
                #               placeholder = 'Please select an option below',
                #               onInitialize = I('function() { this.setValue(""); }')
                #             )),
                            
                  selectInput(inputId = "variable1", label = NULL,
                              choices = c("None", listings_area_summary %>%
                                                  colnames() %>%
                                                  setdiff(c("id", "view", "group")) %>%
                                                  gsub("_", " ", .))),
                width = 6)
              ),
            
            fluidRow(  
              box(title = textOutput("title_main_map"),
                leafletOutput("main_map"),
                  width = 12,
                  align = "center")
            ),
            fluidRow(
              box(title = textOutput("title_main_table"),
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
              selectInput(inputId = "district", label = NULL,
                          choices = berlin_names %>%
                            filter(view == "Districts") %>%
                            select(id) %>% unique() %>% t() %>% c()),
              width = 6),
            
            box(
              title = "Variable choice",
              selectInput(inputId = "variable2", label = NULL,
                          choices = c("None", listings_area_summary %>%
                                        colnames() %>%
                                        setdiff(c("id", "view", "group")) %>%
                                        gsub("_", " ", .))),
              width = 6)
          ),
          fluidRow(  
            box(checkboxInput(inputId = "properties",
                              label = "Display properties",
                              value = FALSE),
                width = 4),
            box(checkboxInput(inputId = "stations",
                              label = "Display train/subway stations",
                              value = FALSE),
                width = 4),
            box(checkboxInput(inputId = "sights",
                              label = "Display top 10 attractions",
                              value = FALSE),
                width = 4)
          ),
          fluidRow(  
            box(title = textOutput("title_secondary_map"),
                leafletOutput("secondary_map"),
                width = 12,
                align = "center")
          ),
          fluidRow(
            box(title = textOutput("title_secondary_table"),
                tableOutput("secondary_table"),
                width = 12,
                align = "center")
          )
          
  ),
  
  ### TAB3
  tabItem(tabName = "price",
          fluidRow(
            box(title = "Variable(s)",
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
                plotOutput(outputId = "corr_plot"),
                           width = 12,
                           align = "center")
          ))


    )

)
)


