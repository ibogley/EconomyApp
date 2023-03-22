library(shiny)


function(input, output, session) {
  
  ##################################################### UI #####################################################
  output$GeographicSelector <- renderUI({
    if (input$GranularitySelector=="Region") {
      selectInput("RegionSelector","Region",choices = unique(state.region))
    } else if (input$GranularitySelector=="State") {
      selectInput("StateSelector","State",choices = state.name)
    } else {}
  })
  
  output$RPPDescriptionUI <- renderUI({
    if (input$GranularitySelector == "Region") {
      selectInput("RPPDescriptions","Descriptions",
                  choices = RPPDescriptions,
                  multiple = FALSE)
    } else {
      selectInput("RPPDescriptions","Descriptions",
                  choices = RPPDescriptions[-1],
                  multiple = TRUE)
    }
  })
  
  
  ##################################################### GDP GRAPH #####################################################
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
  
  ##################################################### HC GRAPH #####################################################
  
  output$HumanCapitalGraph <- renderPlot({
    if (input$GranularitySelector == "State") {
      #Statewide
      TempDF <- StateHCDF %>%
        filter(State == input$StateSelector,
               Year>=input$HumanCapitalYears[1],
               Year<=input$HumanCapitalYears[2]) %>%
        group_by(Year, Industry) %>% summarise(Jobs = sum(Jobs))
      
      if (length(input$HumanCapitalIndustries) > 0) {
        IndustryBreakdown <- geom_bar(
          data = TempDF %>%
            filter(
              Industry %in% gsub("(Government) ", "", input$HumanCapitalIndustries)
            ),
          aes(x = Year, y = Jobs, fill = Industry),
          stat = "identity"
        )
      } else {
        IndustryBreakdown <- NULL
      }
      
      if (input$IncludeTotalJobs) {
        Plot <- TempDF %>%
          ggplot(aes(x = Year, y = Jobs)) +
          stat_summary(fun = sum, geom = "line")
      } else {
        Plot <- data.frame() %>%
          ggplot()
      }
      
      Plot + scale_y_continuous(labels = comma, limits = c(0, NA)) +
        ylab("Jobs") +
        labs(title = paste("Jobs in ", input$StateSelector, " (Yearly)", sep = "")) +
        theme(plot.title = element_text(hjust = .5)) +
        geom_hline(yintercept = 0) +
        IndustryBreakdown
    } else if (input$GranularitySelector == "Region") {
      #RegionWide
      TempDF <- StateHCDF %>%
        filter(Region == input$RegionSelector,
               Year>=input$HumanCapitalYears[1],
               Year<=input$HumanCapitalYears[2]) %>%
        group_by(Year, Industry) %>% summarise(Jobs = sum(Jobs))
      
      if (length(input$HumanCapitalIndustries) > 0) {
        IndustryBreakdown <- geom_bar(
          data = TempDF %>%
            filter(
              Industry %in% gsub("(Government) ", "", input$HumanCapitalIndustries)
            ),
          aes(x = Year, y = Jobs, fill = Industry),
          stat = "identity"
        )
      } else {
        IndustryBreakdown <- NULL
      }
      
      if (input$IncludeTotalJobs) {
        Plot <- TempDF %>%
          ggplot(aes(x = Year, y = Jobs)) +
          stat_summary(fun = sum, geom = "line")
      } else {
        Plot <- data.frame() %>%
          ggplot()
      }
      
      Plot + scale_y_continuous(labels = comma, limits = c(0, NA)) +
        ylab("Jobs") +
        labs(title = paste("Jobs in ", input$RegionSelector, " (Yearly)", sep = "")) +
        theme(plot.title = element_text(hjust = .5)) +
        geom_hline(yintercept = 0) +
        IndustryBreakdown
    } else {
      #NationWide
      TempDF <- StateHCDF %>%
        filter(Year>=input$HumanCapitalYears[1],
               Year<=input$HumanCapitalYears[2]) %>%
        group_by(Year, Industry) %>% summarise(Jobs = sum(Jobs))
      
      if (length(input$HumanCapitalIndustries) > 0) {
        IndustryBreakdown <- geom_bar(
          data = TempDF %>%
            filter(
              Industry %in% gsub("(Government) ", "", input$HumanCapitalIndustries)
            ),
          aes(x = Year, y = Jobs, fill = Industry),
          stat = "identity"
        )
      } else {
        IndustryBreakdown <- NULL
      }
      
      if (input$IncludeTotalJobs) {
        Plot <- TempDF %>%
          ggplot(aes(x = Year, y = Jobs)) +
          stat_summary(fun = sum, geom = "line")
      } else {
        Plot <- data.frame() %>%
          ggplot()
      }
      
      Plot + scale_y_continuous(labels = comma, limits = c(0, NA)) +
        ylab("Jobs") +
        labs(title = paste("Jobs in the US (Yearly)", sep = "")) +
        theme(plot.title = element_text(hjust = .5)) +
        geom_hline(yintercept = 0) +
        expand_limits(y = 0) +
        IndustryBreakdown
      
    }
  })
  ##################################################### FFR GRAPH #####################################################
  output$FFRGraph <- renderPlot({
    FFSDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$CreditYear[1], year <= input$CreditYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = FederalFundsRate/100)) + 
      labs(title = "Federal Funds Rate") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = percent)
  })
  ##################################################### Credit GRAPH #####################################################
  output$CreditGraph <- renderPlot({
    CreditDF %>% 
      filter(Year>=input$CreditYear[1],Year<=input$CreditYear[2],
             State == input$CreditState,!CreditRating == "N/A") %>% 
      mutate(Year = factor(Year,levels = input$CreditYear[1]:input$CreditYear[2]),
             CreditRating = factor(CreditRating,levels = c("BBB-","BBB","BBB+","A-","A","A+","AA-","AA","AA+","AAA"))) %>%
      ggplot(aes(
        x = Year,y = CreditRating,
        fill = CreditRating)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(breaks = min(CreditDF$Year):max(CreditDF$Year)) +
      labs(title = paste("Standard & Poor's Credit Rating: ",input$CreditState,sep = "")) +
      theme(legend.position = "none",plot.title = element_text(hjust = .5))
  })

}
