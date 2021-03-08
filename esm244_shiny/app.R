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
library(kableExtra)
library(broom)

# Read in data set 
foraging <- read.csv(here("data","2018_foraging.csv")) %>% 
  clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  rename(aerial_survey_yr = year)

# Establish new data set to call for "prey type" tab 
prey <- foraging %>% 
  select(suc, prey_item, date, age, prey_qty) %>% 
  group_by(prey_item) %>% 
  count(suc, wt = prey_qty) %>% 
  rename(prey_caught = n) %>% 
  filter(suc == "Y") 

prey_10 <- prey [-c(1),] %>% 
  slice_max(prey_caught, n = 10) %>% 
  ungroup %>%
  top_n(10, prey_caught)


age_sex <- foraging %>% 
  filter(sex == "F" | sex == "M") %>% 
  filter(age == "J" | age == "A")

# Establish a new data set for "dive type" based on latitude and longitude data  
dive_data <- foraging %>% 
  select(otter_lat_deg, otter_long_deg, suc)

# Establish new subsets for blr regression analysis of sex and age 
blr_subset <- age_sex %>% 
  filter(suc == "Y" |
           suc == "N") %>% 
  mutate(suc = fct_drop(suc))

blr_subset$suc <- as.factor(blr_subset$suc)

success_blr <- glm(suc ~ age + sex,
                   data = blr_subset,
                   family = "binomial")

blr_tidy <- broom::tidy(success_blr)

blr_tidy$p.value <- round(blr_tidy$p.value, digit = 3)

blr_tidy %>% 
  mutate(p.value = case_when(p.value < 0.0001 ~ "< 0.0001",
                             TRUE ~ as.character(p.value)))

blr_fitted <- success_blr %>% 
  broom::augment(type.predict = "response")


sex_plot <- ggplot(data = blr_fitted, aes(x = sex, y = .fitted)) +
  geom_col() +
  labs(x = "Sex",
       y = "Probability of outcome successful")

age_plot <- ggplot(data = blr_fitted, aes(x = age, y = .fitted)) +
  geom_col() +
  labs(x = "Age",
       y = "Probability of outcome successful")

blr_fitted <- success_blr %>% 
  broom::augment(type.predict = "response")


ui <- fluidPage(theme = "style.css",
    navbarPage("Sea otter foraging",
               
                          tabPanel("Home",
                                  HTML(
                                    "<section class='banner'>
                                    <h2 class='parallax'>SEA OTTER FORAGING</h2>
                                    </section>"),
                                  tags$div(
                                    tags$br(), tags$br(), tags$h4(),
                                    "The sea otters of Prince Wales, Alaska, U.S., were relocated as a result of conservation efforts in 1968. Ecologist of the University of                                         Alaska and neighboring areas studied otters as a part of the Apex Predators, Ecosystems, and Community Sustainability (APECS) project                                       in order to study the impacts of the reintroduction on coastal communities. We will further manipulate data provided from APEC to create an                                             interactive interface to identify trends or significant effects."), 
                                  # mainPanel(
                                  #   img(src = "eating.png",
                                  #       height = 250,
                                  #       width = 300,
                                  #       style = "display: block; margin-left: auto; margin-right: auto;")
                                  # )
                          ),
               
                           tabPanel("About",
                                    titlePanel("About"),
                                      tags$div(tags$br(),tags$br(),tags$h4("Code"),
                                      "Code and input data used to generate this Shiny mapping tool are available as ",tags$a(href="https://knb.ecoinformatics.org/view                                                  /urn%3Auuid%3Abbf026b7-ca66-412e-9243-33532506c4e0", "sea otter foraging data."),
                                      tags$br(),tags$br(),tags$h4("Sources"),
                                      "Nicole LaRoche, Sydney King, and Heidi Pearson. 2020. Sea otter foraging data, visual observations from Prince of Wales, Alaska. Knowledge                                        Network for Biocomplexity.",
                                      tags$br(),tags$br(),tags$h4("Authors"),
                                      "Lory Salazar, Master's Candidate at The Bren School of Environmental Science & Management",tags$br(),
                                      "Catherine Takata, Master's Candidate at The Bren School of Environmental Science & Management ",tags$br(),
                                      tags$br(),tags$br(),tags$h4("Research Location"),
                                      
                                      leaflet() %>% 
                                        addTiles() %>% 
                                        addProviderTiles(providers$Esri.WorldStreetMap) %>% 
                                        addMiniMap(width = 200,
                                                   height = 150,
                                                   centerFixed = FALSE) %>% 
                                        setView(lng = -134.105784, 
                                                lat = 56.336226, 
                                                zoom = 4) %>% 
                                        addMarkers(lng = -134.105784, 
                                                   lat = 56.336226, 
                                                   popup = "Prince of Wales, AK"),
                                    )
                                    ),
                           
                           tabPanel("Dive Type",
                                    titlePanel("Dive Type"),
                                    sidebarLayout(
                                        sidebarPanel(selectInput("select_dive", 
                                                                 label = "Select dive type:",
                                                                 choices = list("All dives", 
                                                                                "Successful Dives" = "Y", 
                                                                                "Unsuccessful dives" = "N", 
                                                                                "Travel dive" = "T", 
                                                                                "Previous dive" = "C", 
                                                                                "Interactive otter dive" = "I", 
                                                                                "Unknown" = "U"
                                                                 ),
                                                     )
                                       
                                    ),
                                    mainPanel(leafletOutput("map"))
                                    )),
                           
                           
                           tabPanel("Prey Type",
                                    titlePanel("Prey Type"),
                                    sidebarLayout(
                                      sidebarPanel(
                                                   checkboxGroupInput(inputId = "pick_prey",
                                                                      label = "Choose species:",
                                                                      choices = unique(prey_10$prey_item))
                                      ),
                                      mainPanel(plotOutput("prey_plot"))
                                    )
                           ),
                           
                           
                           tabPanel("Dive Success",
                                    titlePanel("Predicting success of foraging dive by age and sex"),
                                    sidebarLayout(

                                      sidebarPanel(selectInput("select", label = ("Select predictor variable:"), 
                                                               inputId = "pick_regress",
                                                               choices = c("Age" = "age", "Sex" = "sex")),
                                                   tableOutput("regression_table")
                                                   ),
                                      mainPanel(plotOutput("regression_plot"))
                                      
                                    )
                           ),
                           
                           tabPanel("Otter Characteristics",
                                    titlePanel("Otter Characteristics"),
                                    sidebarLayout(
                                      sidebarPanel(checkboxGroupInput(inputId = "characteristics",
                                                                      label = "Select sex AND age:",
                                                                      choices = c("Male" = "M", 
                                                                                  "Female" = "F",
                                                                                  "Adult" = "A", 
                                                                                  "Juvenile" = "J"))
                                      ),
                                      mainPanel(tableOutput("summary_table"))
                                    )
                           )
                           
                           
                           
                )
                
                
)

server <- function(input, output, session) {
  
  prey_reactive <- reactive({
    
    prey_10 %>%
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

  blr_reactive <- reactive({
    blr_fitted %>% 
      select(input$pick_regress, .fitted) 
  })
  
  
  output$regression_plot <- 
    renderPlot({
      
      if (input$pick_regress == "age")  {print(age_plot)}   
      if (input$pick_regress == "sex")  {print(sex_plot)}  
  })
  
  output$regression_table <- renderTable(
    blr_tidy
  )
    
  

  
}

shinyApp(ui = ui, server = server)