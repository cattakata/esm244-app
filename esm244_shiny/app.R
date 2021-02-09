library(tidyverse)
library(shiny)
library(here)

foraging <- read.csv(here("data","2018_foraging.csv"))

ui <- fluidPage(
                
                navbarPage("TITLE!!",
                           tabPanel("Thing 1",
                                    sidebarLayout(
                                        sidebarPanel(selectInput("select", label = h3("Select dive type:"), 
                                                                 choices = list("All dives" = 1, "Successful dives" = 2, "Unsuccessful dives" = 3), 
                                                                 selected = 1),
                                                     
                                                     hr(),
                                                     fluidRow(column(3, verbatimTextOutput("value")))
                                                     
                                                     
                                                     
                                                     ),
                                        mainPanel("map coming soon")
                                    )
                                    ),
                           tabPanel("Thing 2",
                                    sidebarLayout(
                                        sidebarPanel("widgets"),
                                        mainPanel("output")
                                    )
                                    ),
                           tabPanel("Thing 3",
                                    sidebarLayout(
                                        sidebarPanel("widgets"),
                                        mainPanel("output")
                                    )
                                    ),
                           tabPanel("Thing 4",
                                    sidebarLayout(
                                        sidebarPanel("widgets"),
                                        mainPanel("output")
                                    )
                                    )
                           
                           
                )
                
                
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
