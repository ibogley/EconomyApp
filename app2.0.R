library(shiny)
library(urbnmapr)
library(tidyverse)
library(bea.R)
library(lubridate)
library(scales)



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
  conditionalPanel(condition = "input.Go>input.Reset",id = "ModalWindow",
                   actionButton("Reset","X"),
                   plotOutput("GDPGraph")
                   )
)


server <- function(input, output) {
  
  BEAKey <- Sys.getenv("BEA_API_KEY")
  
  BEAYears <- paste((year(Sys.Date())-10):year(Sys.Date()),collapse = ",")
  
  BEASpecsGDP <- list(
    "UserID" = BEAKey,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SQGDP2",
    "LineCode" = 1,
    "GeoFips" = "STATE",
    "Year" = BEAYears,
    "ResultFormat" = "json"
  )
  
  StateGDPDF <- beaGet(beaSpec = BEASpecsGDP) %>% 
    mutate(State = state.abb[match(GeoName,state.name)]) %>%
    pivot_longer(6:47,names_to = "Quarter",values_to = "GDP") %>%
    mutate(Quarter = gsub("^DataValue_","",Quarter),
           Date = as.Date(paste(substr(Quarter,1,4),as.integer(substr(Quarter,6,6))*3-2,1,sep = "-")),
           GDP = GDP*1000000) %>%
    .[,c(2,3,6,9,8)]
  
  NationalSF <- get_urbn_map(map = "states", sf = TRUE)
  
  output$NationalGraph <- renderPlot({
   NationalSF %>%
     ggplot(aes(fill = state_abbv == input$State)) + 
      geom_sf() + theme(legend.position = "none",axis.ticks = element_blank(),axis.text = element_blank())
  })
  
  output$GDPGraph <- renderPlot({
    StateGDPDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = Date, y = GDP)) + 
      geom_line() + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $)") +
      labs(title = paste("GDP of ",state.name[match(input$State,state.abb)]),sep = "") + 
      theme(plot.title = element_text(hjust = .5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
