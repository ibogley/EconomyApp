output$GDPGraph <- renderPlot({
  
  if (input$GranularitySelector == "State") {
    #Statewide
    TempDF <- StateGDPDF %>%
      filter(State == input$StateSelector) %>%
      group_by(Date,Industry) %>% summarise(GDP = sum(GDP))
    
    if (length(input$GDPIndustries>0)) {
      IndustryBreakdown <- geom_bar(data = TempDF %>% 
                                      filter(Industry %in% gsub("(Government) ","",input$GDPIndustries)),
                                    aes(x = Date, y = GDP,fill = Industry),stat = "identity")
    } else {
      IndustryBreakdown <- NULL
    }
    
    TempDF %>%
      ggplot(aes(x = Date, y = GDP)) + 
      stat_summary(fun = sum,geom = "line") + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste("Economic Size of ",input$StateSelector," (GDP, Quarterly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) + 
      IndustryBreakdown
  } else if (input$GranularitySelector == "Region") {
    #RegionWide
    TempDF <- StateGDPDF %>%
      filter(Region == input$RegionSelector) %>%
      group_by(Date,Industry) %>% summarise(GDP = sum(GDP))
    
    if (length(input$GDPIndustries>0)) {
      IndustryBreakdown <- geom_bar(data = TempDF %>% 
                                      filter(Industry %in% gsub("(Government) ","",input$GDPIndustries)),
                                    aes(x = Date, y = GDP,fill = Industry),stat = "identity")
    } else {
      IndustryBreakdown <- NULL
    }
    
    TempDF %>%
      ggplot(aes(x = Date, y = GDP)) + 
      stat_summary(fun = sum,geom = "line") + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste("Economic Size of the ",input$RegionSelector," (GDP, Quarterly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
  } else {
    #NationWide
    TempDF <- StateGDPDF %>%
      group_by(Date,Industry) %>% summarise(GDP = sum(GDP))
    
    if (length(input$GDPIndustries>0)) {
      IndustryBreakdown <- geom_bar(data = TempDF %>% 
                                      filter(Industry %in% gsub("(Government) ","",input$GDPIndustries)),
                                    aes(x = Date, y = GDP,fill = Industry),stat = "identity")
    } else {
      IndustryBreakdown <- NULL
    }
    
    TempDF %>%
      ggplot(aes(x = Date, y = GDP)) + 
      stat_summary(fun = sum,geom = "line") + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste("Economic Size of the US (GDP, Quarterly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
      
  }
})