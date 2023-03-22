output$GDPGraph <- renderPlot({
  if (input$StateOrRegion == "State") {
    StateGDPDF %>%
      filter(State == input$State) %>%
      group_by(Date) %>% summarise(GDP = sum(GDP)) %>%
      ggplot(aes(x = Date, y = GDP)) + 
      geom_line() + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste("Economic Size of ",input$State," (GDP, Quarterly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5))
  } else if (input$StateOrRegion == "Region") {
    StateGDPDF %>%
      filter(Region == input$Region) %>%
      group_by(Date) %>% summarise(GDP = sum(GDP)) %>%
      ggplot(aes(x = Date, y = GDP)) + 
      geom_line() + scale_y_continuous(labels = comma) +
      ylab("GDP (Current USD $1000)") +
      labs(title = paste("Economic Size of the ",input$Region," (GDP, Quarterly)",sep = "")) + 
      theme(plot.title = element_text(hjust = .5))
  }
})