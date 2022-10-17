library(shiny)
library(urbnmapr)
library(tidyverse)

ui <- fluidPage(
  tags$head(includeCSS("C:/Users/Ivar/Desktop/data_analysis/USEconomyApp/USEconomyApp/style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  selectInput("State","State",c("None",state.abb)),
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      plotOutput("NationalGraph")
    )),
  conditionalPanel(condition = "input.State != 'None'",id = "ModalWindow",
                   textOutput("State"),
                   plotOutput("StateGraph"),
                   actionButton("Reset","X"),
                   textOutput("Test")
                   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  State <- reactive({input$State})
  
  NationalSF <- get_urbn_map(map = "states", sf = TRUE)
  
  StateSF <- get_urbn_map(map = "counties",sf = TRUE)
  
  TestLogic <- reactive({!State() == "None"})
  
  CountySF <- reactive({
    if (TestLogic()) {
      StateSF %>% filter(state_abbv == State())
    }
    else {
      StateSF
      }
  })
  
  output$Test <- reactive({ifelse(TestLogic()==TRUE,"Success","Fail")})
  
  output$StateGraph <- renderPlot({
    CountySF() %>%
      ggplot(aes()) + geom_sf()
    })
  
  output$NationalGraph <- renderPlot({
    NationalSF %>%
      ggplot(aes()) + geom_sf()
  })
  
  output$State <- State
  
  observeEvent(input$Reset,{
    updateSelectInput(inputId = "State",selected = "None")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
