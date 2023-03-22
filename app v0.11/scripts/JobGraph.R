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

BEASpecsEmployment2 <- list(
  "UserID" = BEAKey,
  "Method" = "GetData",
  "datasetname" = "Regional",
  "TableName" = "SAEMP25N",
  "LineCode" = 100,
  "GeoFips" = "STATE",
  "Year" = BEAYears,
  "ResultFormat" = "json"
)


StateJobsDF <- beaGet(beaSpec = BEASpecsEmployment) %>%
  mutate(State = gsub(" \\*","",GeoName), Region = state.region[match(State,state.name)],.before = "Code") %>%
  pivot_longer(8:ncol(.),names_to = "Year",values_to = "Jobs") %>%
  mutate(Year = as.integer(gsub("DataValue_","",Year))) %>%
  .[,c(1,2,6:9)]


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