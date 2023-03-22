output$HumanCapitalGraph <- renderPlot({
  
  if (input$GranularitySelector == "State") {
    #Statewide
    TempDF <- StateHCDF %>%
      filter(State == input$StateSelector) %>%
      group_by(Year,Industry) %>% summarise(Jobs = sum(Jobs))
    
    if (length(input$HumanCapitalIndustries>0)) {
      IndustryBreakdown <- geom_bar(data = TempDF %>% 
                                      filter(Industry %in% gsub("(Government) ","",input$HumanCapitalIndustries)),
                                    aes(x = Year, y = Jobs,fill = Industry),stat = "identity")
    } else {
      IndustryBreakdown <- NULL
    }
    
    TempDF %>%
      ggplot(aes(x = Year, y = Jobs)) + 
      stat_summary(fun = sum,geom = "line") + scale_y_continuous(labels = comma) +
      ylab("Jobs") +
      labs(title = paste("Jobs in ",input$StateSelector," (Yearly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) + 
      IndustryBreakdown
  } else if (input$GranularitySelector == "Region") {
    #RegionWide
    TempDF <- StateHCDF %>%
      filter(Region == input$RegionSelector) %>%
      group_by(Year,Industry) %>% summarise(Jobs = sum(Jobs))
    
    if (length(input$HumanCapitalIndustries>0)) {
      IndustryBreakdown <- geom_bar(data = TempDF %>% 
                                      filter(Industry %in% gsub("(Government) ","",input$HumanCapitalIndustries)),
                                    aes(x = Year, y = Jobs,fill = Industry),stat = "identity")
    } else {
      IndustryBreakdown <- NULL
    }
    
    TempDF %>%
      ggplot(aes(x = Year, y = Jobs)) + 
      stat_summary(fun = sum,geom = "line") + scale_y_continuous(labels = comma) +
      ylab("Jobs") +
      labs(title = paste("Jobs in ",input$RegionSelector," (Yearly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
  } else {
    #NationWide
    TempDF <- StateHCDF %>%
      group_by(Year,Industry) %>% summarise(Jobs = sum(Jobs))
    
    if (length(input$HumanCapitalIndustries>0)) {
      IndustryBreakdown <- geom_bar(data = TempDF %>% 
                                      filter(Industry %in% gsub("(Government) ","",input$HumanCapitalIndustries)),
                                    aes(x = Year, y = Jobs,fill = Industry),stat = "identity")
    } else {
      IndustryBreakdown <- NULL
    }
    
    TempDF %>%
      ggplot(aes(x = Year, y = Jobs)) + 
      stat_summary(fun = sum,geom = "line") + scale_y_continuous(labels = comma) +
      ylab("Jobs") +
      labs(title = paste("Jobs in the US (Yearly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
      
  }
})