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

GDPIndustries <- c("Agriculture, forestry, fishing and hunting",
                   "Mining, quarrying, and oil and gas extraction",
                   "Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade",
                   "Transportation and warehousing","Information","Finance and insurance",
                   "Real estate and rental and leasing","Professional, scientific, and technical services",
                   "Management of companies and enterprises",
                   "Administrative and support and waste management and remediation services",
                   "Educational services","Health care and social assistance",
                   "Arts, entertainment, and recreation",
                   "Accommodation and food services",
                   "Other services (except government and government enterprises)",
                   "(Government) Federal civilian","(Government) Military","(Government) State and local")

HumanCapitalIndustries <- c("Farm employment","Forestry, fishing and related activities",
                            "Mining,quarrying, and oil and gas extraction","Utilities",
                            "Construction","Manufacturing","Wholesale trade","Retail trade",
                            "Transportation and warehousing","Information","Finance and insurance",
                            "Real estate and rental and leasing",
                            "Professional, scientific, and technical services",
                            "Management of companies and enterprises",
                            "Administrative and support and waste management and remediation services",
                            "Educational services","Health care and social assistance",
                            "Arts, entertainment, and recreation","Accomadation and food services",
                            "Other services (except government and government enterprises",
                            "(Government) Federal civilian","(Government) Military","(Government) State and local")

CurrentYear <- as.numeric(substr(Sys.Date(),1,4))
BEAKey <- Sys.getenv("BEA_API_KEY")
BEAYears <- paste(2000:CurrentYear,collapse = ",")

source("scripts/GDPDataScript.R",local = TRUE)
source("scripts/HCDataScript.R",local = TRUE)

ui <- fluidPage(
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  
  actionButton("OpenGranularity","Select Geography"),
  conditionalPanel("input.OpenGranularity>input.CloseGranularity",
                   id = "GranularityWindow",
                   actionButton("CloseGranularity","X"),
                   selectInput("GranularitySelector","Granularity",c("Nation","Region","State")),
                   uiOutput("GeographicSelector")
                   ),
  
  tabsetPanel(
    
    tabPanel(
      "Economic Size",
      sidebarLayout(
        sidebarPanel(
          h2("GDP (Economic Size)"),
          sliderInput("GDPYears","Years",
                      min= 2000,max = CurrentYear,
                      value = c(2000,CurrentYear),
                      sep = ""),
          selectInput("GDPIndustries","Industries",
                      choices = GDPIndustries,multiple = TRUE)
        ),
        mainPanel(
          plotOutput("GDPGraph")
        )
      )
    ),
    tabPanel(
      "Human Capital",
      sidebarLayout(
        sidebarPanel(
          h2("Human Capital"),
          sliderInput("HumanCapitalYears","Years",
                      min= 2000,max = CurrentYear,
                      value = c(2000,CurrentYear),
                      sep = ""),
          selectInput("HumanCapitalIndustries","Industries",
                      choices = HumanCapitalIndustries,
                      multiple = TRUE)
        ),
        mainPanel(
          plotOutput("HumanCapitalGraph")
        )
      )
    )
  )
  
  
)


server <- function(input, output) {
  
  
  
  output$GeographicSelector <- renderUI({
    if (input$GranularitySelector=="Region") {
      selectInput("RegionSelector","Region",choices = unique(state.region))
    } else if (input$GranularitySelector=="State") {
      selectInput("StateSelector","State",choices = state.name)
    } else {}
  })
  
  source("scripts/GDPGraph.R",local = TRUE)
  source("scripts/HCGraph.R",local = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
