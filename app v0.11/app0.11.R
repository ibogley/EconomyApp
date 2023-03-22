library(shiny)
library(urbnmapr)
library(tidyverse)
library(bea.R)
library(tigris)
#osm package instead of tigris to get highways?
library(lubridate)
library(scales)
library(shinycssloaders)
library(sf)
library(future.apply)
library(DT)
library(plotly)

plan(multisession)
options(tigris_use_cache = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

CurrentYear <- as.numeric(substr(Sys.Date(),1,4))

ui <- fluidPage(
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  
  
  
  #Main Page: Regional Selector 
  sidebarLayout(
    sidebarPanel(
      selectInput("StateOrRegion","State or Region",c("State","Region","Nation")),
      uiOutput("StateOrRegionSelector"),
      sliderInput("Years","Years",2000,CurrentYear,c(2000,CurrentYear),sep = ""),
      actionButton("Go","Go")
    ),
    mainPanel(
      withSpinner(plotOutput("NationalGraph"))
    )),
  
  
  
  
  #Stats Panel
  conditionalPanel(condition = "input.Go>input.Reset",id = "ModalWindow",
                   actionButton("Reset","X"),
                   br(),
                   sidebarLayout(
                     sidebarPanel(
                       h2("Compare"),
                       div(
                         id = "CompareContainer",
                         selectInput("ComparisonTimeScale","Time Scale",
                                     c("Year","Quarter")),
                         uiOutput("ComparisonTimes"),
                         uiOutput("ComparisonBreakdown")
                       )
                     ),
                     mainPanel(
                       tabsetPanel(
                         id = "StatTabs",
                         #Tab 1: GDP View
                         tabPanel(
                           title = "Economic Size",
                           mainPanel(
                             plotOutput("GDPGraph"),
                             div(
                               id = "GDPComparisonContainer",
                               plotOutput("GDPComparisonIndustry1",height = "242px",width = "242px"),
                               plotOutput("GDPComparisonIndustry2",height = "242px",width = "242px"),
                               plotOutput("GDPComparisonSector1",height = "242px",width = "242px"),
                               plotOutput("GDPComparisonSector2",height = "242px",width = "242px")
                             )
                           )
                         ),
                         #Tab 2: Labor View
                         tabPanel(title = "Human Capital: The Job Market",
                                  plotOutput("JobGraph"),
                                  plotOutput("PopulationGraph")
                         ),
                         #Tab 3: Capital View
                         tabPanel(title = "Capital: Business Investment"),
                         #Tab 4: Trade View
                         tabPanel(title = "Trade"),
                         #Tab 5: Geographic View
                         tabPanel(title = "Geographic",
                                  plotOutput("StateGraph"))
                         
                       )
                     )
                   )
  )
)


server <- function(input, output) {
  
  BEAKey <- Sys.getenv("BEA_API_KEY")
  
  BEAYears <- paste(2000:CurrentYear,collapse = ",")
  
  observeEvent(input$Go,{
    BEAYears <- paste(input$Years[1]:input$Years[2],collapse = ",")
  })
  
  
  
  source("scripts/GDPDataScript.R",local = TRUE)
  source("scripts/NationalGraph.R",local = TRUE)
  source("scripts/StateGraph.R",local = TRUE)
  
  
  
  observeEvent(input$Go,{
    source("scripts/ComparisonScript.R",local = TRUE)
    source("scripts/GDPGraph.R",local = TRUE)
    source("scripts/GDPComparisonGraph.R",local = TRUE)
    source("scripts/JobGraph.R",local = TRUE)
    source("scripts/PopulationGraph.R",local = TRUE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
