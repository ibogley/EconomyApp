library(shiny)
library(tidyverse)
library(bea.R)

ui <- fluidPage(

    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  key <- Sys.getenv("BEA_API_KEY")
  
  beaSpecs <- list(
    "UserID" = key,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "CAGDP2",
    "LineCode" = 1,
    "GeoFips" = "COUNTY",
    "Year" = "2020",
    "ResultFormat" = "json"
  )
  
  beaParams(beaKey = key,"Regional") %>% view()
  
  data <- beaGet(beaSpecs) 
  
  data %>%
    filter(grepl("^41.*",GeoFips))
}

# Run the application 
shinyApp(ui = ui, server = server)
