library(tidyverse)
library(shiny)
library(here)
library(shinythemes)
library(leaflet)
library(janitor)
library(lubridate)
library(sf)
library(tmap)
library(shinyWidgets)

foraging <- read.csv(here("data","2018_foraging.csv")) %>% 
  clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  rename(aerial_survey_yr = year)

prey <- foraging %>% 
  select(suc, prey_item, date, age, prey_qty) %>% 
  group_by(prey_item) %>% 
  count(suc, wt = prey_qty) %>% 
  rename(prey_caught = n) %>% 
  filter(suc == "Y")

age_sex <- foraging %>% 
  filter(sex == "F" | sex == "M") %>% 
  filter(age == "J" | age == "A")

dive_data <- foraging %>% 
  select(otter_lat_deg, otter_long_deg, suc)


ui <- fluidPage(
    theme = shinytheme("cerulean"),
                
                navbarPage("Sea otter foraging",
                           tabPanel("About this site",
                                    leaflet() %>% 
                                      addTiles() %>% 
                                      setView(lng = -134.105784, lat = 56.336226, zoom = 3.25) %>% 
                                      addMarkers(lng = -134.105784, lat = 56.336226, popup = "Prince of Wales, AK"),
                                    tags$div(
                                      tags$br(), tags$br(), tags$h4("Background"),
                                      "The sea otters of Prince Wales, Alaska, U.S., were relocated as a result of conservation efforts in 1968.                                             Ecologist of the University of Alaska and neighboring areas studied these otters as a part of the Apex Predators,                                        Ecosystems, and Community Sustainability (APECS) project in order to study the impacts of the reintroduction on                                          coastal communities. We will further manipulate the data provided to create an interactive interface to identify                                         trends or significant effects.", 
                                      tags$br(),tags$br(),tags$h4("Code"),
                                      "Code and input data used to generate this Shiny mapping tool are available as ",tags$a(href="https://knb       .ecoinformatics.org/view/urn%3Auuid%3Abbf026b7-ca66-412e-9243-33532506c4e0", "sea otter foraging data."),
                                      tags$br(),tags$br(),tags$h4("Sources"),
                                      "Nicole LaRoche, Sydney King, and Heidi Pearson. 2020. Sea otter foraging data, visual observations from Prince of Wales, Alaska. Knowledge Network for Biocomplexity.",
                                      tags$br(),tags$br(),tags$h4("Authors"),
                                      "Lory Salazar, Master's Candidate at The Bren School of Environmental Science & Management",tags$br(),
                                      "Catherine Takata, Master's Candidate at The Bren School of Environmental Science & Management ",tags$br(),
                                    )
                                    ),
                           tabPanel("Dive Type",
                                    sidebarLayout(
                                        sidebarPanel(selectInput("select_dive", label = "Select dive type:",
                                                                 choices = list("All dives", 
                                                                                "Successful Dives" = "Y", 
                                                                                "Unsuccessful dives" = "N", 
                                                                                "Travel dive" = "T", 
                                                                                "Previous dive" = "C", 
                                                                                "Interactive otter dive" = "I", "Unknown" = "U"
                                                                 ),
                                                     )
                                       
                                    ),
                                    mainPanel(leafletOutput("map"))
                                    )),
                           
                           tabPanel("Prey",
                                    sidebarLayout(
                                      sidebarPanel(
                                                   checkboxGroupInput(inputId = "pick_prey",
                                                                      label = "Choose species:",
                                                                      choices = unique(prey$prey_item))
                                      ),
                                      mainPanel("plot of prey successfully obtained", 
                                                plotOutput("prey_plot"))
                                    )
                           ),
                           
                           
                           tabPanel("Dive time and prey quantity",
                                    sidebarLayout(
                                      sidebarPanel(selectInput("select", label = h3("Select dive type:"), 
                                                               inputId = "pick_regress",
                                                               choices = c("Adult" = "A", "Juvenile" = "J"))
                                                   ),
                                      mainPanel("Predicting number of prey caught by dive time",
                                                plotOutput("regression_plot"))
                                    )
                           ),
                           
                           tabPanel("Otter Age and Sex",
                                    sidebarLayout(
                                      sidebarPanel(checkboxGroupInput(inputId = "characteristics",
                                                                      label = "Select sex AND age:",
                                                                      choices = c("Male" = "M", "Female" = "F",
                                                                                  "Adult" = "A", "Juvenile" = "J"))
                                      ),
                                      mainPanel("Summary statistics of dive time and number of prey caught for age and sex selected",
                                                tableOutput("summary_table"))
                                    )
                           )
                           
                           
                           
                )
                
                
)

server <- function(input, output, session) {
  
  prey_reactive <- reactive({
    
    prey %>%
      filter(prey_item %in% input$pick_prey)
  })
  
  output$prey_plot <- renderPlot(
    ggplot(data = prey_reactive(), aes(x = prey_item,
                                       y = prey_caught)) +
      geom_col()
    
  )
  
  char_reactive <- reactive ({
    age_sex %>% 
      filter(age %in% input$characteristics) %>% 
      filter(sex %in% input$characteristics) %>% 
      group_by(sex, age) %>% 
      summarise(dive_time = round(mean(dt, na.rm = TRUE),2),
                prey_quantity = round(mean(prey_qty, na.rm = TRUE), 2) 
      )
  })
  
  output$summary_table <- renderTable(char_reactive())
  

  dive_react <- reactive({
    if (input$select_dive == "All dives") {
      dive_data
    } else {
      filter(dive_data, suc == input$select_dive)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(dive_react()) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addMarkers(~otter_long_deg, ~otter_lat_deg, 
                 label = ~suc, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~suc)
  })
  
  observe({
    leafletProxy("map", data = dive_react()) %>%
      clearShapes() %>%
      addMarkers(~otter_long_deg, ~otter_lat_deg, 
                 label = ~suc, 
                 labelOptions = labelOptions(textsize = "12px"),
                 popup = ~suc)
  })

  lm_reactive <- reactive({
    otters %>% 
      filter(age %in% input$pick_regress)
  })
  
  output$regression_plot <- renderPlot(
    ggplot(data = lm_reactive(), aes(x = prey_qty, y = dt)) +
      geom_jitter(size = 2) +
      geom_smooth(method = "lm",
                  color = "red",
                  size = 0.5,
                  fill = "gray10",
                  alpha = 0.5) + # geom_smooth is to add a linear model to a scatterplot
      labs(x = "Prey quantity", y = "Dive time (seconds)") +
      theme_light() +
      ggpubr::stat_regline_equation(label.x = 17, label.y = 300)
  )
  

}

shinyApp(ui = ui, server = server)