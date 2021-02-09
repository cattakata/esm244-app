library(tidyverse)
library(shiny)
library(here)
library(shinythemes)

foraging <- read.csv(here("data","2018_foraging.csv"))

ui <- fluidPage(
    theme = shinytheme("cerulean"),
                
                navbarPage("Sea otter foraging",
                           tabPanel("Thing 1",
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
                           tabPanel("Thing 2",
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
                           tabPanel("Thing 3",
                                    sidebarLayout(
                                        sidebarPanel(
                                            fluidRow(
                                                column(4,
                                                       sliderInput("slider2", label = h3("Slider Range"), min = 0, 
                                                                   max = 100, value = c(40, 60))
                                                )
                                            ),
                                            
                                            hr(),
                                            
                                            fluidRow(
                                                column(4, verbatimTextOutput("value")),
                                                column(4, verbatimTextOutput("range"))
                                            )
                                            
                                            
                                            
                                        ),
                                        mainPanel("Linear regression describing how successful a dive is, predicted by cloud cover")
                                    )
                                    ),
                           tabPanel("Thing 4",
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
