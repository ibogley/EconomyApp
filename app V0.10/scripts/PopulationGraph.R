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


StatePopulationDF <- beaGet(beaSpec = BEASpecsPopulation) %>%
  mutate(State = gsub(" \\*","",GeoName), Region = state.region[match(State,state.name)],.before = "Code") %>%
  pivot_longer(8:ncol(.),names_to = "Year",values_to = "Population") %>%
  mutate(Year = as.integer(gsub("DataValue_","",Year))) %>%
  .[,c(1,2,6:9)]


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