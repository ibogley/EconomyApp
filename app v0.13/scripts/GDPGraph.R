output$GDPGraph <- renderPlot({
  if (input$GranularitySelector == "State") {
    #Statewide
    TempDF <- StateGDPDF %>%
      filter(State == input$StateSelector,
             year(Date)>= input$GDPYears[1],
             year(Date)<= input$GDPYears[2]) %>%
      group_by(Date, Industry) %>% summarise(GDP = sum(GDP))
    
    if (length(input$GDPIndustries) > 0) {
      IndustryBreakdown <- geom_bar(
        data = TempDF %>%
          filter(
            Industry %in% gsub("(Government) ", "", input$GDPIndustries)
          ),
        aes(x = Date, y = GDP, fill = Industry),
        stat = "identity"
      )
    } else {
      IndustryBreakdown <- NULL
    }
    
    if (input$IncludeTotalGDP) {
      Plot <- TempDF %>%
        ggplot(aes(x = Date, y = GDP)) +
        stat_summary(fun = sum, geom = "line")
    } else {
      Plot <- data.frame() %>%
        ggplot()
    }
    
    Plot + scale_y_continuous(labels = comma, limits = c(0, NA)) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste(
        "Economic Size of ",
        input$StateSelector,
        " (GDP, Quarterly)",
        sep = ""
      )) +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
  } else if (input$GranularitySelector == "Region") {
    #RegionWide
    TempDF <- StateGDPDF %>%
      filter(Region == input$RegionSelector,
             year(Date)>= input$GDPYears[1],
             year(Date)<= input$GDPYears[2]) %>%
      group_by(Date, Industry) %>% summarise(GDP = sum(GDP))
    
    if (length(input$GDPIndustries) > 0) {
      IndustryBreakdown <- geom_bar(
        data = TempDF %>%
          filter(
            Industry %in% gsub("(Government) ", "", input$GDPIndustries)
          ),
        aes(x = Date, y = GDP, fill = Industry),
        stat = "identity"
      )
    } else {
      IndustryBreakdown <- NULL
    }
    
    if (input$IncludeTotalGDP) {
      Plot <- TempDF %>%
        ggplot(aes(x = Date, y = GDP)) +
        stat_summary(fun = sum, geom = "line")
    } else {
      Plot <- data.frame() %>%
        ggplot()
    }
    
    Plot + scale_y_continuous(labels = comma, limits = c(0, NA)) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste(
        "Economic Size of the ",
        input$RegionSelector,
        " (GDP, Quarterly)",
        sep = ""
      )) +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
  } else {
    #NationWide
    TempDF <- StateGDPDF %>%
      filter(year(Date)>= input$GDPYears[1],
             year(Date)<= input$GDPYears[2]) %>%
      group_by(Date, Industry) %>% summarise(GDP = sum(GDP))
    
    if (length(input$GDPIndustries) > 0) {
      IndustryBreakdown <- geom_bar(
        data = TempDF %>%
          filter(
            Industry %in% gsub("(Government) ", "", input$GDPIndustries)
          ),
        aes(x = Date, y = GDP, fill = Industry),
        stat = "identity"
      )
    } else {
      IndustryBreakdown <- NULL
    }
    
    if (input$IncludeTotalGDP) {
      Plot <- TempDF %>%
        ggplot(aes(x = Date, y = GDP)) +
        stat_summary(fun = sum, geom = "line")
    } else {
      Plot <- data.frame() %>%
        ggplot()
    }
    
    Plot + scale_y_continuous(labels = comma, limits = c(0, NA)) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste("Economic Size of the US (GDP, Quarterly)", sep = "")) +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      expand_limits(y = 0) +
      IndustryBreakdown
    
  }
})