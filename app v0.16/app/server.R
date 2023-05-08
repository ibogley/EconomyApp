function(input, output, session) {
  ##################################################### UI: Regional #####################################################
  output$RegionalSelector <- renderUI({
    if (input$GranularitySelector=="Regional") {
      selectInput("RegionSelector","Region",unique(as.character(state.region)))
    } else if (input$GranularitySelector=="State") {
      selectInput("StateSelector","State",c(state.name))
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
        ylab("GDP (Current Millions USD)") +
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
        ylab("GDP (Current Millions USD)") +
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
      
      Plot + scale_y_continuous(labels = label_number(suffix = "B",scale = 1e-3), limits = c(0, NA)) +
        ylab("GDP (Billions USD)") +
        labs(title = paste("Economic Size of the US (Nominal GDP, Quarterly)", sep = "")) +
        theme(plot.title = element_text(hjust = .5)) +
        geom_hline(yintercept = 0) +
        expand_limits(y = 0) +
        IndustryBreakdown
      
    }
  })
  
  ##################################################### Employment GRAPH #####################################################
  
  output$EmploymentGraph <- renderPlot({
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
      
      Plot + scale_y_continuous(labels = label_number(suffix = "M",scale = 1e-6), limits = c(0, NA)) +
        ylab("") +
        labs(title = paste("Jobs in the US (Yearly, Millions)", sep = "")) +
        theme(plot.title = element_text(hjust = .5)) +
        geom_hline(yintercept = 0) +
        expand_limits(y = 0) +
        IndustryBreakdown
      
    }
  })
  
  ##################################################### Median Wage GRAPH #####################################################
  output$MedianWageGraph <- renderPlot({
    MedianWageDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$HumanCapitalYears[1], year <= input$HumanCapitalYears[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = MedianWage)) + 
      labs(title = "Real Median Weekly Wage: Wage and Salary Workers 16+") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = dollar) +
      ylab("")
  })
  
  ##################################################### Avg Wage GRAPH #####################################################
  output$AvgWageGraph <- renderPlot({
    AvgWageDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$HumanCapitalYears[1], year <= input$HumanCapitalYears[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = AvgWage)) + 
      labs(title = "Average Hourly Wage") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = dollar) +
      ylab("")
  })
  
  ##################################################### FFR GRAPH #####################################################
  output$FFRGraph <- renderPlot({
    FFSDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$CreditYear[1], year <= input$CreditYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = FederalFundsRate/100)) + 
      labs(title = "Federal Funds Rate (%)") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = percent) +
      ylab("")
  })
  
  ##################################################### Capital Stock GRAPH #####################################################
  output$CSGraph <- renderPlot({
    CapitalStockDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$CapitalYear[1], year <= input$CapitalYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = CapitalStockPPP)) + 
      labs(title = "Capital Stock: PPP (Millions 2017 USD)") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = label_number(suffix = "M",scale = 1e-6)) +
      ylab("")
  })
  
  ##################################################### S&P500 GRAPH #####################################################
  output$SP500Graph <- renderPlot({
    SP500DF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$StockYear[1], year <= input$StockYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = SP500)) + 
      labs(title = "S&P 500 Index") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      ylab("")
  })
  
  ##################################################### Dow Jones GRAPH #####################################################
  output$DowJonesGraph <- renderPlot({
    DowJonesDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$StockYear[1], year <= input$StockYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = DowJones)) + 
      labs(title = "Dow Jones Industrial Average") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      ylab("")
  })
  
  ##################################################### NASDAQ GRAPH #####################################################
  output$NASDAQGraph <- renderPlot({
    NASDAQDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$StockYear[1], year <= input$StockYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = NASDAQComposite)) + 
      labs(title = "NASDAQ Composite Index") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      ylab("")
  })
  
  ##################################################### M1 GRAPH #####################################################
  output$M1Graph <- renderPlot({
    M1DF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$MoneySupplyYear[1], year <= input$MoneySupplyYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = M1)) + 
      labs(title = "M1 (Billions USD)") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = label_number(suffix = "B")) +
      ylab("")
  })
  
  ##################################################### M2 GRAPH #####################################################
  output$M2Graph <- renderPlot({
    M2DF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$MoneySupplyYear[1], year <= input$MoneySupplyYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = M2)) + 
      labs(title = "M2 (Billions USD)") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = label_number(suffix = "B")) +
      ylab("")
  })
  
  ##################################################### Inflation GRAPH #####################################################
  output$InflationGraph <- renderPlot({
    InflationDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$PriceYear[1], year <= input$PriceYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = InflationConsumerPrices/100)) + 
      labs(title = "Inflation (Consumer Prices, %YoY)") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      scale_y_continuous(labels = percent) +
      ylab("")
  })
  
  ##################################################### CPI All Urban GRAPH #####################################################
  output$CPIGraph <- renderPlot({
    CPIAllDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$PriceYear[1], year <= input$PriceYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = CPI)) + 
      labs(title = "CPI, All Items for Urban Consumers") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      ylab("")
  })
  
  ##################################################### CPI Less Food, Energy GRAPH #####################################################
  output$CPILFEGraph <- renderPlot({
    CPIAllLFEDF %>%
      mutate(year = as.integer(substr(date,1,4))) %>%
      filter(year >= input$PriceYear[1], year <= input$PriceYear[2]) %>%
      ggplot() +
      geom_line(aes(x = date, y = CPILFE)) + 
      labs(title = "CPI, All Items for Urban Consumers less Food & Energy") +
      theme(plot.title = element_text(hjust = .5)) +
      geom_hline(yintercept = 0) +
      ylab("")
  })
  
  
}
