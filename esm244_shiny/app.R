library(tidyverse)
library(shiny)
library(here)
library(shinythemes)
library(leaflet)
library(janitor)
library(lubridate)

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

dive_locations <- st_as_sf(x = dive_data, 
                           coords = c("otter_long_deg", "otter_lat_deg"),
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

alaska <- read_sf(here("data", "alaska_shape", "tl_2017_02_place.shp")) %>% 
  select(NAME, ALAND) %>% 
  rename(county_name = NAME, land_area = ALAND)



ui <- fluidPage(
    theme = shinytheme("cerulean"),
                
                navbarPage("Sea otter foraging",
                           tabPanel("About this site",
                                    leaflet() %>% 
                                      addTiles() %>% 
                                      addMarkers(lng = -134.105784, lat = 56.336226, popup = "Prince of Wales, AK"),
                                    tags$div(
                                      tags$br(), tags$br(), tags$h4("Background"),
                                      "The sea otters of Prince Wales, Alaska, U.S., were relocated as a result of conservation efforts in 1968.                                             Ecologist of the University of Alaska and neighboring areas studied these otters as a part of the Apex Predators,                                        Ecosystems, and Community Sustainability (APECS) project in order to study the impacts of the reintroduction on                                          coastal communities. We will further manipulate the data provided to create an interactive interface to identify                                         trends or significant effects.", 
                                      tags$br(),tags$br(),tags$h4("Code"),
                                      "Code and input data used to generate this Shiny mapping tool are available as ",tags$a(href="https://knb       .ecoinformatics.org/view/urn%3Auuid%3Abbf026b7-ca66-412e-9243-33532506c4e0", "sea otter foraging data."),
                                      tags$br(),tags$br(),tags$h4("Sources"),
                                      "Nicole LaRoche, Sydney King, and Heidi Pearson. 2020. Sea otter foraging data, visual observations from Prince of Wales, Alaska. Knowledge Network for Biocomplexity. urn:uuid:bbf026b7-ca66-412e-9243-33532506c4e0.",
                                      tags$br(),tags$br(),tags$h4("Authors"),
                                      "Lory Salazar, Master's Candidate at The Bren School of Environmental Science & Management",tags$br(),
                                      "Catherine Takata, Master's Candidate at The Bren School of Environmental Science & Management ",tags$br(),
                                    )
                                    ),
                           tabPanel("Dive Type",
                                    sidebarLayout(
                                        sidebarPanel(selectInput("select", label = h3("Select dive type:"), 
                                                                 inputId = "select_dive",
                                                                 choices = list(
                                                                     "Successful Dives" = "Y", "Unsuccessful dives" = "N", "Travel dive" = "T", 
                                                                     "Previous dive" = "C", "Interactive otter dive" = "I", "Unknonw" = "U"),
                                                    
                                                     
                                                     ),
                                       
                                    ),
                                    mainPanel("Interactive map of sea otter foraging dives",
                                              tmapOutput("dive_map"))
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
                           
                           
                           tabPanel("Cloud Cover",
                                    sidebarLayout(
                                      sidebarPanel(
                                        fluidRow(
                                          column(12,
                                                 sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                                                             max = 100, value = c(40, 60))
                                          )
                                        ),
                                        
                                        hr(),
                                        
                                        fluidRow(
                                          column(12, verbatimTextOutput("value")),
                                          column(12, verbatimTextOutput("range"))
                                        )
                                        
                                        
                                        
                                      ),
                                      mainPanel("Linear regression describing how successful a dive is, predicted by cloud cover")
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

server <- function(input, output) {
  
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
  
  dive_reactive <- reactive({
    
    dive_locations %>%
      filter(suc %in% input$select_dive)
      
      
  })
  
  output$dive_map = renderTmap({
    tmap_mode(mode = "view") 
    
    tm_shape(alaska) +
      tm_fill("land_area") +
      tm_shape(dive_locations) + 
      tm_dots()
    
  })
}

shinyApp(ui = ui, server = server)