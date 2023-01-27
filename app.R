library(shiny)
library(GPfit)
load("GP_model_all.rda")

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Intervention parameters"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      selectInput(inputId = "location",
                  label = "Location and scenario",
                  choices = c("China, baseline", "China, optimistic VE", "Shanghai, baseline", "Shanghai, optimistic VE", "Shenzhen, baseline", "Shenzhen, optimistic VE", "Shenzhen, pessimistic VE", "Shiyan, baseline", "Shiyan, optimistic VE")),
      
      sliderInput("Mask", "Mask",
                  min = 0, max = 1,
                  value = 0),
      
      sliderInput("Hospital", "Hospital beds per 1000",
                  min = 3.27, max = 14.4,
                  value = 3.27),
      
      sliderInput("ICU", "ICU beds per 100,000",
                  min = 3.42, max = 48,
                  value = 3.42),
      
      sliderInput("Vac0_19", "ΔVac. 0-19",
                  min = 0, max = 1,
                  value = 0),
      
      sliderInput("Vac20_59", "ΔVac. 20-59",
                  min = 0, max = 1,
                  value = 0),
      
      sliderInput("Vac60_69", "ΔVac. 60-69",
                  min = 0, max = 1,
                  value = 0),
      
      sliderInput("Vac_above70", "ΔVac. 70above",
                  min = 0, max = 1,
                  value = 0),
      
      sliderInput("Antiviral", "Antiviral",
                  min = 0, max = 1,
                  value = 0),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      h4("Predicted median mortality rate (1 per 100,000 persons)"),
      verbatimTextOutput("values")
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output, session) {
  
  
  observeEvent(input$location,  {
    updateSliderInput(session = session, inputId = "Hospital", min = switch(input$location, 
                                                                            "China, baseline" = 5.06,
                                                                            "China, optimistic VE" = 5.06,
                                                                            "Shanghai, baseline" = 5.78,
                                                                            "Shanghai, optimistic VE" = 5.78,
                                                                            "Shenzhen, baseline" = 3.27,
                                                                            "Shenzhen, optimistic VE" = 3.27,
                                                                            "Shenzhen, pessimistic VE" = 3.27,
                                                                            "Shiyan, baseline" = 6.82,
                                                                            "Shiyan, optimistic VE" = 6.82),
                      value = switch(input$location, 
                                     "China, baseline" = 5.06,
                                     "China, optimistic VE" = 5.06,
                                     "Shanghai, baseline" = 5.78,
                                     "Shanghai, optimistic VE" = 5.78,
                                     "Shenzhen, baseline" = 3.27,
                                     "Shenzhen, optimistic VE" = 3.27,
                                     "Shenzhen, pessimistic VE" = 3.27,
                                     "Shiyan, baseline" = 6.82,
                                     "Shiyan, optimistic VE" = 6.82))
  })
  
  
  
  
  observeEvent(input$location,  {
    updateSliderInput(session = session, inputId = "ICU", min = switch(input$location, 
                                                                       "China, baseline" = 8.33,
                                                                       "China, optimistic VE" = 4.37,
                                                                       "Shanghai, baseline" = 12.5,
                                                                       "Shanghai, optimistic VE" = 6.14,
                                                                       "Shenzhen, baseline" = 3.42,
                                                                       "Shenzhen, optimistic VE" = 3.42,
                                                                       "Shenzhen, pessimistic VE" = 3.42,
                                                                       "Shiyan, baseline" = 7.38,
                                                                       "Shiyan, optimistic VE" = 5.13),
                      value = switch(input$location, 
                                     "China, baseline" = 8.33,
                                     "China, optimistic VE" = 4.37,
                                     "Shanghai, baseline" = 12.5,
                                     "Shanghai, optimistic VE" = 6.14,
                                     "Shenzhen, baseline" = 3.42,
                                     "Shenzhen, optimistic VE" = 3.42,
                                     "Shenzhen, pessimistic VE" = 3.42,
                                     "Shiyan, baseline" = 7.38,
                                     "Shiyan, optimistic VE" = 5.13))
  })
  

  

  
  
  observeEvent(input$location,  {
    updateSliderInput(session = session, inputId = "Vac_above70", min = switch(input$location, 
                                                                               "China, baseline" = 0.76,
                                                                               "China, optimistic VE" = 0.33,
                                                                               "Shanghai, baseline" = 0.979,
                                                                               "Shanghai, optimistic VE" = 0.793,
                                                                               "Shenzhen, baseline" = 0,
                                                                               "Shenzhen, optimistic VE" = 0,
                                                                               "Shenzhen, pessimistic VE" = 0,
                                                                               "Shiyan, baseline" = 0,
                                                                               "Shiyan, optimistic VE" = 0),
                      value = switch(input$location, 
                                     "China, baseline" = 0.76,
                                     "China, optimistic VE" = 0.33,
                                     "Shanghai, baseline" = 0.979,
                                     "Shanghai, optimistic VE" = 0.793,
                                     "Shenzhen, baseline" = 0,
                                     "Shenzhen, optimistic VE" = 0,
                                     "Shenzhen, pessimistic VE" = 0,
                                     "Shiyan, baseline" = 0,
                                     "Shiyan, optimistic VE" = 0))
  })
  
  
  
  observeEvent(input$location,  {
    updateSliderInput(session = session, inputId = "Antiviral", min = switch(input$location, 
                                                                             "China, baseline" = 0.577,
                                                                             "China, optimistic VE" = 0,
                                                                             "Shanghai, baseline" = 0.777,
                                                                             "Shanghai, optimistic VE" = 0,
                                                                             "Shenzhen, baseline" = 0,
                                                                             "Shenzhen, optimistic VE" = 0,
                                                                             "Shenzhen, pessimistic VE" = 0,
                                                                             "Shiyan, baseline" = 0.572,
                                                                             "Shiyan, optimistic VE" = 0),
                      value = switch(input$location, 
                                     "China, baseline" = 0.577,
                                     "China, optimistic VE" = 0,
                                     "Shanghai, baseline" = 0.777,
                                     "Shanghai, optimistic VE" = 0,
                                     "Shenzhen, baseline" = 0,
                                     "Shenzhen, optimistic VE" = 0,
                                     "Shenzhen, pessimistic VE" = 0,
                                     "Shiyan, baseline" = 0.572,
                                     "Shiyan, optimistic VE" = 0))
  })
  
  
  output$values <- renderText({
    ModelID <- switch(input$location, 
                      "China, baseline" = 1,
                      "China, optimistic VE" = 2,
                      "Shanghai, baseline" = 3,
                      "Shanghai, optimistic VE" = 4,
                      "Shenzhen, baseline" = 5,
                      "Shenzhen, optimistic VE" = 6,
                      "Shenzhen, pessimistic VE" = 7,
                      "Shiyan, baseline" = 8,
                      "Shiyan, optimistic VE" = 9)
    
    current.model <- GP.model[[ModelID]]
    
    current.X <- data.frame(Antiviral	= input$Antiviral,
                            Hospital	= input$Hospital,
                            ICU	= input$ICU,
                            Mask = input$Mask,
                            Vac0_19	= input$Vac0_19,
                            Vac20_59	= input$Vac20_59,
                            Vac60_69	= input$Vac60_69,
                            Vac_70above = input$Vac_above70)
    
    min.Hospital <- switch(input$location, 
                           "China, baseline" = 5.06,
                           "China, optimistic VE" = 5.06,
                           "Shanghai, baseline" = 5.78,
                           "Shanghai, optimistic VE" = 5.78,
                           "Shenzhen, baseline" = 3.27,
                           "Shenzhen, optimistic VE" = 3.27,
                           "Shenzhen, pessimistic VE" = 3.27,
                           "Shiyan, baseline" = 6.82,
                           "Shiyan, optimistic VE" = 6.82)
    
    min.ICU <- switch(input$location, 
                      "China, baseline" = 8.33,
                      "China, optimistic VE" = 4.37,
                      "Shanghai, baseline" = 12.5,
                      "Shanghai, optimistic VE" = 6.14,
                      "Shenzhen, baseline" = 3.42,
                      "Shenzhen, optimistic VE" = 3.42,
                      "Shenzhen, pessimistic VE" = 3.42,
                      "Shiyan, baseline" = 7.38,
                      "Shiyan, optimistic VE" = 5.13)
    
    min.Vac_70above <- switch(input$location, 
                              "China, baseline" = 0.76,
                              "China, optimistic VE" = 0.33,
                              "Shanghai, baseline" = 0.979,
                              "Shanghai, optimistic VE" = 0.793,
                              "Shenzhen, baseline" = 0,
                              "Shenzhen, optimistic VE" = 0,
                              "Shenzhen, pessimistic VE" = 0,
                              "Shiyan, baseline" = 0,
                              "Shiyan, optimistic VE" = 0)
    
    min.Antiviral <- switch(input$location, 
                            "China, baseline" = 0.577,
                            "China, optimistic VE" = 0,
                            "Shanghai, baseline" = 0.777,
                            "Shanghai, optimistic VE" = 0,
                            "Shenzhen, baseline" = 0,
                            "Shenzhen, optimistic VE" = 0,
                            "Shenzhen, pessimistic VE" = 0,
                            "Shiyan, baseline" = 0.572,
                            "Shiyan, optimistic VE" = 0)
    
    
    current.X$Hospital <- (current.X$Hospital - min.Hospital)/(14.4 - min.Hospital) 
    current.X$ICU <- (current.X$ICU - min.ICU)/(48 - min.ICU)
    current.X$Vac_70above <- (current.X$Vac_70above - min.Vac_70above)/(1 - min.Vac_70above)
    current.X$Antiviral <- (current.X$Antiviral - min.Antiviral)/(1 - min.Antiviral)
    
    predict(current.model, as.matrix(current.X))$Y_hat
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
