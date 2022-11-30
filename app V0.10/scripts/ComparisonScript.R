output$ComparisonTimes <- renderUI({
  UIElements <- list(
    numericInput("ComparisonY1","Comparison: Year (1)",CurrentYear-1,input$Years[1],input$Years[2]),
    numericInput("ComparisonY2","Comparison: Year (2)",CurrentYear,input$Years[1],input$Years[2])
  )
  
  
  if (input$ComparisonTimeScale=="Quarter") {
    UIElements <- list(
      numericInput("ComparisonY1","Comparison: Year (1)",CurrentYear-1,input$Years[1],input$Years[2]),
      numericInput("ComparisonQ1","Comparison: Quarter (1)",1,1,4),
      numericInput("ComparisonY2","Comparison: Year (2)",CurrentYear,input$Years[1],input$Years[2]),
      numericInput("ComparisonQ2","Comparison: Quarter (2)",1,1,4)
    )
  }
  
  UIElements
})

GDPIndustryBreakdown <- data.frame(Industry = c("Agriculture, forestry, fishing and hunting","Mining, quarrying, and oil and gas extraction",
                                                "Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade",
                                                "Transportation and warehousing","Information","Finance and insurance",
                                                "Real estate and rental and leasing","Professional, scientific, and technical services",
                                                "Management of companies and enterprises",
                                                "Administrative and support and waste management and remediation services",
                                                "Educational services","Health care and social assistance","Arts, entertainment, and recreation",
                                                "Accommodation and food services","Other services (except government and government enterprises)",
                                                "Federal civilian","Military","State and local")) %>%
  mutate(Sector = ifelse(
    Industry %in% c("Federal civilian","Military","State and local"),
    "Government","Private Industries")) %>%
  .[,2:1]

output$GDPBreakdown <- renderDataTable({
  GDPIndustryBreakdown %>%
    datatable(rownames = FALSE)
})

output$ComparisonBreakdown <- renderUI({
  if (input$StatTabs=="Economic Size") {
    dataTableOutput("GDPBreakdown")
  }
})


