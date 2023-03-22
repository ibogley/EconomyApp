
library(shiny)

fluidPage(

  
  tags$head(includeCSS("style.css")),
  titlePanel("US 50 States: Economic Statistics"),
  
  actionButton("OpenGranularity","Select Geography"),
  conditionalPanel("input.OpenGranularity>input.CloseGranularity",
                   id = "GranularityWindow",
                   actionButton("CloseGranularity","X"),
                   selectInput("GranularitySelector","Granularity",c("Nation","Region","State")),
                   uiOutput("GeographicSelector")
  ),
  
  tabsetPanel(
    
    tabPanel(
      "Economic Size",
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
      "Human Capital",
      sidebarLayout(
        sidebarPanel(
          h2("Human Capital"),
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
          plotOutput("HumanCapitalGraph")
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
                      sep = ""),
          selectInput("CreditState","State",state.name),
          uiOutput("FFRDescriptionUI")
        ),
        mainPanel(
          plotOutput("FFRGraph"),
          plotOutput("CreditGraph")
        )
      )
    )
  )
)
