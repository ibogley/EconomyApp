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

plan(multisession)
options(tigris_use_cache = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ui <- fluidPage(
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  
  
  
  #Main Page: Regional Selector 
  sidebarLayout(
    sidebarPanel(
      selectInput("StateOrRegion","State or Region",c("State","Region","Nation")),
      uiOutput("StateOrRegionSelector"),
      actionButton("Go","Go")
    ),
    mainPanel(
      withSpinner(plotOutput("NationalGraph"))
    )),
  
  
  
  
  #Stats Panel
  conditionalPanel(condition = "input.Go>input.Reset",id = "ModalWindow",
                   actionButton("Reset","X"),
                   br(),
                   tabsetPanel(
                     #Tab 1: GDP View
                     tabPanel(title = "Economic Size",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("GDPBreakdown","Breakdown",choices = c("Sector","Industry")),
                                  dataTableOutput("VariableSelector")
                                ),
                                mainPanel(
                                  withSpinner(plotOutput("GDPGraph"))
                                )
                              )
                     ),
                     #Tab 2: Labor View
                     tabPanel(title = "Human Capital: The Job Market",
                              withSpinner(plotOutput("JobGraph")),
                              withSpinner(plotOutput("PopulationGraph"))
                     ),
                     #Tab 3: Capital View
                     tabPanel(title = "Capital: Business Investment"),
                     #Tab 4: Trade View
                     tabPanel(title = "Trade"),
                     #Tab 5: Geographic View
                     tabPanel(title = "Geographic",
                              withSpinner(plotOutput("StateGraph"))),
                     #Tab 6: Custom View
                     tabPanel(title = "Custom",
                     )
                     
                   ))
)


server <- function(input, output) {
  
  output$StateOrRegionSelector <- renderUI({
    if (input$StateOrRegion=="State") {
      selectInput("State","State",c("None",state.name))
    } else if (input$StateOrRegion=="Region"){
      selectInput("Region","Region",c("None",as.character(unique(state.region))))
    }
    
  })
  
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
  
  StatePopulationDF <- beaGet(beaSpec = BEASpecsPopulation) %>%
    mutate(State = gsub(" \\*","",GeoName), Region = state.region[match(State,state.name)],.before = "Code") %>%
    pivot_longer(8:ncol(.),names_to = "Year",values_to = "Population") %>%
    mutate(Year = as.integer(gsub("DataValue_","",Year))) %>%
    .[,c(1,2,6:9)]
  
  DFLineCodeMaster <- data.frame("LineCode" = c(3,6,10,11,12,34,35,36,45,51,56,60,64,65,69,70,76,79,82,84,85,86),
                                 "Industry" = c("Agriculture, forestry, fishing and hunting","Mining, quarrying, and oil and gas extraction",
                                                "Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade",
                                                "Transportation and warehousing","Information","Finance and insurance",
                                                "Real estate and rental and leasing","Professional, scientific, and technical services",
                                                "Management of companies and enterprises",
                                                "Administrative and support and waste management and remediation services",
                                                "Educational services","Health care and social assistance","Arts, entertainment, and recreation",
                                                "Accommodation and food services","Other services (except government and government enterprises)",
                                                "Federal civilian","Military","State and local")) %>%
    mutate(Sector = ifelse(LineCode>83,"Government","Private Industries")) %>% .[,c(1,3,2)]
  
  
  
  StateGDPDF <- lapply(c(3,6,10,11,12,34,35,36,45,51,56,60,64,65,69,70,76,79,82,84,85,86),function(x) {
    
    Sector <- DFLineCodeMaster$Sector[match(x,DFLineCodeMaster$LineCode)]
    
    Industry <- DFLineCodeMaster$Industry[match(x,DFLineCodeMaster$LineCode)]
    
    beaSpecs <- list(
      "UserID" = BEAKey,
      "Method" = "GetData",
      "datasetname" = "Regional",
      "TableName" = "SQGDP2",
      "LineCode" = x,
      "GeoFips" = "STATE",
      "Year" = BEAYears,
      "ResultFormat" = "json"
    )
    beaGet(beaSpec = beaSpecs) %>%
      filter(GeoName %in% state.name) %>%
      mutate(State = GeoName,Region = state.region[match(State,state.name)],Industry = Industry, 
             Sector = Sector,Statistic = "GDP, Current USD", .before = "Code") %>%
      select(-c(Code,GeoFips,GeoName)) %>%
      pivot_longer(8:ncol(.),names_to = "Quarter",values_to = "GDP") %>%
      mutate(Quarter = gsub("^DataValue_","",Quarter),
             Date = as.Date(paste(
               substr(Quarter,1,4),
               as.integer(substr(Quarter,6,6))*3-2,1,
               sep = "-"))) %>%
      .[,c(1:7,10,9)]
  }) %>%
    reduce(full_join)
  
  
  output$VariableSelector <- renderDataTable({
    if (input$GDPBreakdown=="Industry") {
      datatable(
        data.frame(
          Industry = c("Agriculture, forestry, fishing and hunting","Mining, quarrying, and oil and gas extraction",
                       "Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade",
                       "Transportation and warehousing","Information","Finance and insurance",
                       "Real estate and rental and leasing","Professional, scientific, and technical services",
                       "Management of companies and enterprises",
                       "Administrative and support and waste management and remediation services",
                       "Educational services","Health care and social assistance","Arts, entertainment, and recreation",
                       "Accommodation and food services","Other services (except government and government enterprises)",
                       "Federal civilian","Military","State and local")
        ),
        rownames = FALSE
      )
    }
  })
  
  
  StateJobsDF <- beaGet(beaSpec = BEASpecsEmployment) %>%
    mutate(State = gsub(" \\*","",GeoName), Region = state.region[match(State,state.name)],.before = "Code") %>%
    pivot_longer(8:ncol(.),names_to = "Year",values_to = "Jobs") %>%
    mutate(Year = as.integer(gsub("DataValue_","",Year))) %>%
    .[,c(1,2,6:9)]
  
  
  
  NationalSF <- get_urbn_map(map = "states", sf = TRUE) %>% st_transform(crs = 4326) %>% 
    st_simplify(dTolerance = 5000) %>%
    mutate(state_name = state.name[match(state_abbv,state.abb)],
           state_region = state.region[match(state_abbv,state.abb)])
  
  RoadsSF <- primary_roads() %>% st_transform(crs = 4326) %>%
    filter(grepl("I-",FULLNAME))
  
  output$NationalGraph <- renderPlot({
    if (input$StateOrRegion=="State") {
      NationalSF %>%
        ggplot(aes(fill = state_name == input$State)) + geom_sf() + 
        theme(legend.position = "none",
              axis.ticks = element_blank(),
              axis.text = element_blank())
    } else if (input$StateOrRegion=="Region"){
      NationalSF %>%
        ggplot(aes(fill = state_region == input$Region)) + geom_sf() + 
        theme(legend.position = "none",
              axis.ticks = element_blank(),
              axis.text = element_blank())
    }
    
  })
  
  
  output$StateGraph <- renderPlot({
    if (input$StateOrRegion=="State") {
      
      
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
          geom_sf(data = RoadsStateSF,color = "blue") +
          coord_sf(crs = 4326,xlim = c(-180,-130)) + 
          theme(axis.ticks = element_blank(),legend.position = "none",
                axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
          labs(title = "Interstate Highways",color = "Primary Road")
        
      } else {
        
        StateSF %>%
          ggplot() + geom_sf() + 
          geom_sf(data = RoadsStateSF,color = "blue") +
          theme(axis.ticks = element_blank(),legend.position = "none",
                axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
          labs(title = "Interstate Highways")
        
      }
    } else if (input$StateOrRegion=="Region") {
      
      StateSF <- NationalSF %>% filter(state_region == input$Region) 
      
      RoadsSFRows <- unlist(future_lapply(state.name[state.region == input$Region],function(x) {
        unlist(st_contains(NationalSF %>% filter(state_name == x),RoadsSF))
      }))
      
      RoadsStateSF <- RoadsSF[RoadsSFRows,]
      
      StateSF %>%
        ggplot() + geom_sf() + 
        geom_sf(data = RoadsStateSF,color = "blue") +
        theme(axis.ticks = element_blank(),legend.position = "none",
              axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
        labs(title = "Interstate Highways")
    }
    
  })
  
  output$PopulationGraph <- renderPlot({
    
    
    if (input$StateOrRegion == "State") {
      
      StatePopulationDF %>%
        filter(State == input$State) %>%
        ggplot(aes(x = Year, y = Population)) +
        geom_line() + ylab("Population") +
        labs(title = paste("Number of People in ",input$State," (Annually)",sep = "")) +
        theme(plot.title = element_text(hjust = .5))
      
    } else if (input$StateOrRegion == "Region") {
      
      StatePopulationDF %>%
        filter(Region == input$Region) %>%
        group_by(Year) %>%
        summarise(Population = sum(Population)) %>%
        ggplot(aes(x = Year, y = Population)) +
        geom_line() + ylab("Population") +
        labs(title = paste("Number of People in the ",input$Region," (Annually)",sep = "")) +
        theme(plot.title = element_text(hjust = .5))
      
    }
    
    
  })
  
  output$GDPGraph <- renderPlot({
    
    
    if (input$StateOrRegion == "State") {
      StateGDPDF %>%
        filter(State == input$State) %>%
        ggplot(aes(x = Date, y = GDP,fill = Sector)) + 
        geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
        ylab("GDP (Current USD $)") +
        labs(title = paste("Economic Size of ",input$State," (GDP, Quarterly)",sep = "")) + 
        theme(plot.title = element_text(hjust = .5))
    } else if (input$StateOrRegion == "Region") {
      StateGDPDF %>%
        filter(Region == input$Region) %>%
        group_by(Date) %>% summarise(GDP = sum(GDP)) %>%
        ggplot(aes(x = Date, y = GDP,fill = Sector)) + 
        geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
        ylab("GDP (Current USD $)") +
        labs(title = paste("Economic Size of the ",input$Region," (GDP, Quarterly)",sep = "")) + 
        theme(plot.title = element_text(hjust = .5))
    }
    
    
  })
  
  output$JobGraph <- renderPlot({
    
    
    if (input$StateOrRegion == "State") {
      StateJobsDF %>%
        filter(State == input$State) %>%
        ggplot(aes(x = Year, y = Jobs)) +
        geom_line() + ylab("Number of Jobs") +
        labs(title = paste("Number of Jobs in ",input$State," (Annually)",sep = "")) +
        theme(plot.title = element_text(hjust = .5))
    } else if (input$StateOrRegion == "Region") {
      StateJobsDF %>%
        filter(Region == input$Region) %>%
        group_by(Year) %>% summarise(Jobs = sum(Jobs)) %>%
        ggplot(aes(x = Year, y = Jobs)) +
        geom_line() + ylab("Number of Jobs") +
        labs(title = paste("Number of Jobs in the ",input$Region," (Annually)",sep = "")) +
        theme(plot.title = element_text(hjust = .5))
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
