fluidPage(

  
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  
  
  tabsetPanel(
    
    tabPanel(
      "Regional",
      sidebarLayout(
        sidebarPanel(
          selectInput("GranularitySelector","Granularity",c("National","Regional","State")),
          uiOutput("RegionalSelector")
        ),
        mainPanel(
          
        )
      )
    ),
    
    tabPanel(
      "Production",
      sidebarLayout(
        sidebarPanel(
          h2("GDP (Economic Size)"),
          sliderInput("GDPYears","Years",
                      min= 2000,max = CurrentYear,
                      value = c(2000,CurrentYear),
                      sep = ""),
          selectInput("GDPIndustries","Industries",
                      choices = GDPIndustries,multiple = TRUE),
          checkboxInput("IncludeTotalGDP","Include Total",value = TRUE)
        ),
        mainPanel(
          plotOutput("GDPGraph")
        )
      )
    ),
    
    tabPanel(
      "Employment",
      sidebarLayout(
        sidebarPanel(
          h2("Employment"),
          sliderInput("HumanCapitalYears","Years",
                      min= 2000,max = CurrentYear,
                      value = c(2000,CurrentYear),
                      sep = ""),
          selectInput("HumanCapitalIndustries","Industries",
                      choices = HumanCapitalIndustries,
                      multiple = TRUE),
          checkboxInput("IncludeTotalJobs","Include Total",value = TRUE)
        ),
        mainPanel(
          plotOutput("EmploymentGraph")
        )
      )
    ),
    
    tabPanel(
      "Credit",
      sidebarLayout(
        sidebarPanel(
          h2("Federal Funds Rate (Fed Interest Rate)"),
          sliderInput("CreditYear","Years",
                      min= CreditMinYear,max = CurrentYear,
                      value = c(CreditMinYear,CurrentYear),
                      sep = "")
        ),
        mainPanel(
          plotOutput("FFRGraph")
        )
      )
    ),
    
    tabPanel(
      "Capital Stock",
      sidebarLayout(
        sidebarPanel(
          h2("Capital Stock"),
          sliderInput("CapitalYear","Years",
                      min= 1950,max = CurrentYear,
                      value = c(1950,CurrentYear),
                      sep = ""
                      )
        ),
        mainPanel(
          plotOutput("CSGraph")
        )
      )
    ),
    
    tabPanel(
      "Stock Market",
      sidebarLayout(
        sidebarPanel(
          h2("Stock Market Indices"),
          sliderInput("StockYear","Years",
                      min= 1971,max = CurrentYear,
                      value = c(2013,CurrentYear),
                      sep = ""
          )
        ),
        mainPanel(
          plotOutput("SP500Graph"),
          plotOutput("DowJonesGraph"),
          plotOutput("NASDAQGraph")
        )
      )
    ),
    
    tabPanel(
      "Money Supply",
      sidebarPanel(
        h2("Money Supply"),
        sliderInput("MoneySupplyYear","Years",
                    min= 1975,max = CurrentYear,
                    value = c(1981,CurrentYear),
                    sep = ""
        )
      ),
      mainPanel(
        plotOutput("M1Graph"),
        plotOutput("M2Graph")
      )
    ),
    
    
    tabPanel(
      "Prices",
      sidebarPanel(
        h2("Prices"),
        sliderInput("PriceYear","Years",
                    min= 1947,max = CurrentYear,
                    value = c(1960,CurrentYear),
                    sep = ""
        )
      ),
      mainPanel(
        plotOutput("InflationGraph"),
        plotOutput("CPIGraph"),
        plotOutput("CPILFEGraph")
      )
    )
  )
)
