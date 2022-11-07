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

options(tigris_use_cache = TRUE)

ui <- fluidPage(
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("State","State",c("None",state.name)),
      actionButton("Go","Go")
    ),
    mainPanel(
      withSpinner(plotOutput("NationalGraph"))
    )),
  conditionalPanel(condition = "input.Go>input.Reset",id = "ModalWindow",
                   actionButton("Reset","X"),
                   br(),
                   tabsetPanel(
                     tabPanel(title = "Typical Econ Stats",
                              withSpinner(plotOutput("GDPGraph")),
                              withSpinner(plotOutput("JobGraph")),
                              withSpinner(plotOutput("PopulationGraph"))),
                     tabPanel(title = "Geographic",
                              withSpinner(plotOutput("StateGraph"))),
                     tabPanel(title = "Trade/Investment")
                   ))
  )


server <- function(input, output) {
  
  BEAKey <- Sys.getenv("BEA_API_KEY")
  
  BEAYears <- paste((year(Sys.Date())-10):year(Sys.Date()),collapse = ",")
  
  BEASpecsPopulation <- list(
    "UserID" = BEAKey,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SAINC4",
    "LineCode" = 20,
    "GeoFips" = "STATE",
    "Year" = BEAYears,
    "ResultFormat" = "json"
  )
  
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
  
  BEASpecsEmployment <- list(
    "UserID" = BEAKey,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SAINC4",
    "LineCode" = 7010,
    "GeoFips" = "STATE",
    "Year" = BEAYears,
    "ResultFormat" = "json"
  )
  
  #Get state level trade data
 
  
  StatePopulationDF <- beaGet(beaSpec = BEASpecsPopulation) %>%
    mutate(State = gsub(" \\*","",GeoName)) %>%
    pivot_longer(6:15,names_to = "Year",values_to = "Population") %>%
    mutate(Year = as.integer(gsub("DataValue_","",Year))) %>%
    .[,c(2,3,6,7,8)]
  
  StateGDPDF <- beaGet(beaSpec = BEASpecsGDP) %>% 
    mutate(State = gsub(" \\*","",GeoName))  %>%
    pivot_longer(6:47,names_to = "Quarter",values_to = "GDP") %>%
    mutate(Quarter = gsub("^DataValue_","",Quarter),
           Date = as.Date(paste(
             substr(Quarter,1,4),
             as.integer(substr(Quarter,6,6))*3-2,1,
             sep = "-")),
           GDP = GDP*1000000) %>%
    .[,c(2,3,6,9,8)]
  
  StateJobsDF <- beaGet(beaSpec = BEASpecsEmployment) %>%
    mutate(State = gsub(" \\*","",GeoName)) %>%
    pivot_longer(6:15,names_to = "Year",values_to = "Jobs") %>%
    mutate(Year = as.integer(gsub("DataValue_","",Year))) %>%
    .[,c(2,3,6,7,8)]
  
  
  
  NationalSF <- get_urbn_map(map = "states", sf = TRUE) %>% st_transform(crs = 4326) %>% 
    st_simplify(dTolerance = 5000) %>%
    mutate(state_name = state.name[match(state_abbv,state.abb)])
  
  RoadsSF <- primary_roads() %>% st_transform(crs = 4326)
  
  output$NationalGraph <- renderPlot({
   NationalSF %>%
     ggplot(aes(fill = state_name == input$State)) + geom_sf() + 
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.text = element_blank())
  })
  
  
  output$StateGraph <- renderPlot({
    
    
    
    if (input$State %in% c("Alaska","Hawaii")) {
      StateSF <- states() %>% filter(NAME == input$State) %>% 
        st_transform(crs = 4326)
    } else {
      StateSF <- NationalSF %>% filter(state_name == input$State) 
    }
    
    RoadsStateSF <- RoadsSF[unlist(st_contains(StateSF,RoadsSF)[1]),]
    
    
    
    if (input$State == "Alaska") {
      StateSF %>%
        ggplot() + geom_sf()+
        geom_sf(data = RoadsStateSF %>% st_simplify(dTolerance = 1),color = "blue") +
        coord_sf(crs = 4326,xlim = c(-180,-130)) + 
        theme(axis.ticks = element_blank(),legend.position = "none",
              axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
        labs(title = "Primary Roads",color = "Primary Road")
    } else {
      StateSF %>%
        ggplot() + geom_sf() + 
        geom_sf(data = RoadsStateSF %>% st_simplify(dTolerance = 1),color = "blue") +
        theme(axis.ticks = element_blank(),legend.position = "none",
              axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
        labs(title = "Primary Roads",color = "Primary Road")
    }
  })
  
  output$PopulationGraph <- renderPlot({
    StatePopulationDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = Year, y = Population)) +
      geom_line() + ylab("Population") +
      labs(title = paste("Number of People in ",input$State," (Annually)",sep = "")) +
      theme(plot.title = element_text(hjust = .5))
  })
  
  output$GDPGraph <- renderPlot({
    StateGDPDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = Date, y = GDP)) + 
      geom_line() + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $)") +
      labs(title = paste("Economic Size of ",input$State," (GDP, Quarterly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5))
  })
  
  output$JobGraph <- renderPlot({
    StateJobsDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = Year, y = Jobs)) +
      geom_line() + ylab("Number of Jobs") +
      labs(title = paste("Number of Jobs in ",input$State," (Annually)",sep = "")) +
      theme(plot.title = element_text(hjust = .5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
