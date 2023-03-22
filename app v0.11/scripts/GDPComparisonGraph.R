PlotTheme <- theme(plot.title = element_text(hjust = .5),legend.position = "none",
                   axis.title = element_blank(),axis.text = element_blank(),
                   axis.ticks = element_blank())

PlotText <- geom_text(aes(label = ""))

PlotLegend <- scale_fill_discrete(labels = label_wrap(5))

if (input$StateOrRegion == "State") {
  Geography <- input$State
} else if (input$StateOrRegion == "Region") {
  Geography <- paste("the ",input$Region,sep = "")
}

output$GDPComparisonIndustry1 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY1,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1,
             quarter(Date)==input$ComparisonQ1)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY1,
      ", Quarter ",input$ComparisonQ1,")",
      sep = ""))
    
  }
  
  if (length(input$GDPBreakdown_rows_selected)>0) {
    TempDF <- TempDF %>%
      mutate(Industry = ifelse(Industry %in% GDPIndustryBreakdown[input$GDPBreakdown_rows_selected,2],Industry,"Other")) %>%
      group_by(State,Region,Industry) %>% summarise(GDP = sum(GDP))
    
    PlotTheme <- theme(plot.title = element_text(hjust = .5),
                       axis.title = element_blank(),axis.text = element_blank(),
                       axis.ticks = element_blank()) 
    
    PlotText <- geom_text(aes(y = pos, label = paste(round(GDP/sum(GDP),2)*100,"%",sep = "")))
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      group_by(Industry) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      PlotTheme +
      PlotText +
      PlotLegend
    
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      group_by(Industry) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      PlotTheme +
      PlotText +
      PlotLegend
  }
})





output$GDPComparisonIndustry2 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY2,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2,
             quarter(Date)==input$ComparisonQ2)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY2,
      ", Quarter ",input$ComparisonQ2,")",
      sep = ""))
    
  }
  
  if (length(input$GDPBreakdown_rows_selected)>0) {
    TempDF <- TempDF %>%
      mutate(Industry = ifelse(Industry %in% GDPIndustryBreakdown[input$GDPBreakdown_rows_selected,2],Industry,"Other")) %>%
      group_by(State,Region,Industry) %>% summarise(GDP = sum(GDP))
    
    PlotTheme <- theme(plot.title = element_text(hjust = .5),
                       axis.title = element_blank(),axis.text = element_blank(),
                       axis.ticks = element_blank())
    
    PlotText <- geom_text(aes(y = pos, label = paste(round(GDP/sum(GDP),2)*100,"%",sep = "")))
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      group_by(Industry) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      PlotTheme +
      PlotText +
      PlotLegend
      
    
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      group_by(Industry) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Industry)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      PlotTheme +
      PlotText +
      PlotLegend
  }
})




output$GDPComparisonSector1 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY1,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY1,
             quarter(Date)==input$ComparisonQ1)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY1,
      ", Quarter ",input$ComparisonQ1,")",
      sep = ""))
    
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      group_by(Sector) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5),
            axis.title = element_blank(),axis.text = element_blank(),
            axis.ticks = element_blank()) +
      geom_text(aes(y = pos, label = paste(round(GDP/sum(GDP),2)*100,"%",sep = ""))) +
      PlotLegend
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      group_by(Sector) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5),
            axis.title = element_blank(),axis.text = element_blank(),
            axis.ticks = element_blank()) +
      geom_text(aes(y = pos, label = paste(round(GDP/sum(GDP),2)*100,"%",sep = ""))) + 
      PlotLegend
  }
})


output$GDPComparisonSector2 <- renderPlot({
  
  if (input$ComparisonTimeScale=="Year") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY2,")",
      sep = ""))
    
  } else if (input$ComparisonTimeScale=="Quarter") {
    
    TempDF <- StateGDPDF %>%
      filter(year(Date)==input$ComparisonY2,
             quarter(Date)==input$ComparisonQ2)
    
    PlotTitle <- labs(title = paste(
      "GDP: ",Geography," (",input$ComparisonY2,
      ", Quarter ",input$ComparisonQ2,")",
      sep = ""))
    
  }
  
  
  if (input$StateOrRegion == "State") {
    TempDF %>%
      filter(State == input$State) %>%
      group_by(Sector) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5),
            axis.title = element_blank(),axis.text = element_blank(),
            axis.ticks = element_blank()) +
      geom_text(aes(y = pos, label = paste(round(GDP/sum(GDP),2)*100,"%",sep = ""))) +
      PlotLegend
  } else if (input$StateOrRegion == "Region") {
    TempDF %>%
      filter(Region == input$Region) %>%
      group_by(Sector) %>% summarise(GDP = sum(GDP)) %>%
      mutate(csum = rev(cumsum(rev(GDP))),
             pos = GDP/2 + lead(csum,1),
             pos = ifelse(is.na(pos),GDP/2,pos)) %>%
      ggplot(aes(x = "", y = GDP, fill = Sector)) + 
      geom_bar(stat = "identity") +
      coord_polar("y") +
      PlotTitle + 
      theme(plot.title = element_text(hjust = .5),
            axis.title = element_blank(),axis.text = element_blank(),
            axis.ticks = element_blank()) +
      geom_text(aes(y = pos, label = paste(round(GDP/sum(GDP),2)*100,"%",sep = ""))) +
      PlotLegend
  }
})