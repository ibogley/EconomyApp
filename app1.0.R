library(shiny)
library(urbnmapr)
library(tidyverse)



ui <- fluidPage(
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("State","State",c("None",state.abb)),
      actionButton("Go","Go")
    ),
    mainPanel(
      plotOutput("NationalGraph")
    )),
  conditionalPanel(condition = "input.State != 'None'",id = "ModalWindow",
                   textOutput("State"),
                   actionButton("Reset","X")
                   )
)


server <- function(input, output) {
  
  State <- reactive({input$State})
  
  NationalSF <- get_urbn_map(map = "states", sf = TRUE)
  
  TestLogic <- reactive({!State() == "None"})
  
  output$NationalGraph <- renderPlot({
   NationalSF %>%
     ggplot(aes(fill = state_abbv == State())) + geom_sf()
  })
  
  output$NationalGraph <- renderPlot({
     NationalSF %>%
       ggplot(aes(fill = state_abbv == State())) + geom_sf()
    })
  
  output$State <- renderText({State()})
  
  observeEvent(input$Go,{
  })
  
  observeEvent(input$Reset,{
    updateSelectInput(inputId = "State",selected = "None")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
