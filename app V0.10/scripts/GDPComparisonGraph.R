output$GDPComparisonIndustry1 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Industry (",input$ComparisonY1,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1,
             quarter(Date)==input$ComparisonQ1)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Industry (",input$ComparisonY1,
      ", Quarter ",input$ComparisonQ1,")",
      sep = ""))
    
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  }
})





output$GDPComparisonIndustry2 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Industry (",input$ComparisonY2,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2,
             quarter(Date)==input$ComparisonQ2)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Industry (",input$ComparisonY2,
      ", Quarter ",input$ComparisonQ2,")",
      sep = ""))
    
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  }
})




output$GDPComparisonSector1 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Sector (",input$ComparisonY1,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1,
             quarter(Date)==input$ComparisonQ1)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Sector (",input$ComparisonY1,
      ", Quarter ",input$ComparisonQ1,")",
      sep = ""))
    
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  }
})


output$GDPComparisonSector2 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Sector (",input$ComparisonY2,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2,
             quarter(Date)==input$ComparisonQ2)
    
    PlotTitle <- labs(title = paste(
      "GDP of ",input$State," by Sector (",input$ComparisonY2,
      ", Quarter ",input$ComparisonQ2,")",
      sep = ""))
    
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5))
  }
})