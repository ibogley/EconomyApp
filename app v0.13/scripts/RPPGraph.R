output$RPPGraph <- renderPlot({
  if (input$GranularitySelector == "State") {
    #Statewide
    StateRPPDF %>%
      filter(State == input$StateSelector, 
             Description %in% c("All Items",input$RPPDescriptions),
             Year>=input$RPPYears[1],Year<=input$RPPYears[2]) %>% 
      ggplot(aes(x = Year, y = RPP, color = Description)) +
      geom_line() +
      ylab("Regional Price Parity Index (% of National Average)")
    
    
  } else if (input$GranularitySelector == "Region") {
    #RegionWide
    StateRPPDF %>%
      filter(Region == input$RegionSelector,
             Description ==input$RPPDescriptions,
             Year>=input$RPPYears[1],Year<=input$RPPYears[2]) %>% 
      ggplot(aes(x = Year, y = RPP, color = State)) +
      geom_line() +
      ylab("Regional Price Parity Index (% of National Average)")
    
    
  } else {
    #NationWide
    StateRPPDF %>%
      filter(State == "United States",
             Description %in% c("All Items",input$RPPDescriptions),
             Year>=input$RPPYears[1],Year<=input$RPPYears[2]) %>% 
      ggplot(aes(x = Year, y = RPP, color = Description)) +
      geom_line() +
      ylab("Regional Price Parity Index (% of National Average)")
  }
})