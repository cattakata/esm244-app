library(tidyverse)
library(shiny)
library(here)
library(shinythemes)
library(leaflet)

foraging <- read.csv(here("data","2018_foraging.csv"))

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
                                                                 choices = list(
                                                                     "All dives" = 1, "Successful dives" = 2, "Unsuccessful dives" = 3), 
                                                                 selected = 1),
                                                     
                                                     hr(),
                                                     fluidRow(column(3, verbatimTextOutput("value")))
                                                     
                                                     
                                                     
                                                     ),
                                        mainPanel("map coming soon")
                                    )
                                    ),
                           tabPanel("Prey Type",
                                    sidebarLayout(
                                        sidebarPanel(checkboxGroupInput("checkGroup", label = h3("Prey type"), 
                                                                        choices = list(
                                                                            "Sea cucumber" = 1, "Graceful crab" = 2, "Sea peach" = 3),
                                                                        selected = 1),
                                                     
                                                     
                                                     hr(),
                                                     fluidRow(column(3, verbatimTextOutput("value")))),
                                        mainPanel("Histogram of counts of selected prey obtained")
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
                                        sidebarPanel(checkboxGroupInput("checkGroup", label = h3("Select age and sex"), 
                                                                        choices = list(
                                                                            "Female" = 1, "Male" = 2, 
                                                                            "Adult" = 3, "Juvenile" = 4),
                                                                        selected = 1),
                                                     
                                                     
                                                     hr(),
                                                     fluidRow(column(3, verbatimTextOutput("value")))
                                                     ),
                                        mainPanel("Summary statistics for age and sex selected")
                                    )
                                    )
                           
                           
                )
                
                
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
