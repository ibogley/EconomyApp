library(shiny)
library(tidyverse)
library(bea.R)
library(lubridate)
library(scales)
library(shinycssloaders)
library(future.apply)
library(plotly)
library(fredr)
library(rvest)
library(janitor)

plan(multisession)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

GDPIndustries <- c("Agriculture, forestry, fishing and hunting",
                   "Mining, quarrying, and oil and gas extraction",
                   "Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade",
                   "Transportation and warehousing","Information","Finance and insurance",
                   "Real estate and rental and leasing","Professional, scientific, and technical services",
                   "Management of companies and enterprises",
                   "Administrative and support and waste management and remediation services",
                   "Educational services","Health care and social assistance",
                   "Arts, entertainment, and recreation",
                   "Accommodation and food services",
                   "Other services (except government and government enterprises)",
                   "(Government) Federal civilian","(Government) Military","(Government) State and local")

HumanCapitalIndustries <- c("Farm employment","Forestry, fishing and related activities",
                            "Mining,quarrying, and oil and gas extraction","Utilities",
                            "Construction","Manufacturing","Wholesale trade","Retail trade",
                            "Transportation and warehousing","Information","Finance and insurance",
                            "Real estate and rental and leasing",
                            "Professional, scientific, and technical services",
                            "Management of companies and enterprises",
                            "Administrative and support and waste management and remediation services",
                            "Educational services","Health care and social assistance",
                            "Arts, entertainment, and recreation","Accomadation and food services",
                            "Other services (except government and government enterprises",
                            "(Government) Federal civilian","(Government) Military","(Government) State and local")

CurrentYear <- as.numeric(substr(Sys.Date(),1,4))
BEAKey <- Sys.getenv("BEA_API_KEY")
BEAYears <- paste(2000:CurrentYear,collapse = ",")

##################################################### CREDIT RATING DATA #####################################################
CreditDF <- read_html("https://ballotpedia.org/State_credit_ratings") %>%
  html_node(".wikitable") %>% html_table() %>%
  row_to_names(1) %>% filter(State %in% state.name) %>%
  pivot_longer(-1,names_to = "Year",values_to = "CreditRating",names_transform = as.integer)


##################################################### FRED DATA #####################################################
FREDKey <- Sys.getenv("FRED_API_KEY")
fredr_set_key(FREDKey)
FFSDF <- fredr("FEDFUNDS") %>%
  select(date,value) %>% rename("FederalFundsRate" = value)

CapitalStockDF <- fredr("CKSPPPUSA666NRUG") %>%
  select(date,value) %>% rename("CapitalStockPPP" = value)

CreditMinYear <- min(c(year(FFSDF$date),CreditDF$Year))

SP500DF <- fredr("SP500") %>%
  select(date,value) %>% rename("SP500" = value)

DowJonesDF <- fredr("DJIA") %>%
  select(date,value) %>% rename("DowJones" = value)

NASDAQDF <- fredr("NASDAQCOM") %>%
  select(date,value) %>% rename("NASDAQComposite" = value)

M1DF <- fredr("WM1NS") %>%
  select(date,value) %>% rename("M1" = value)

M2DF <- fredr("WM2NS") %>%
  select(date,value) %>% rename("M2" = value)

InflationDF <- fredr("FPCPITOTLZGUSA") %>%
  select(date,value) %>% rename("InflationConsumerPrices" = value)

CPIAllDF <- fredr("CPIAUCSL") %>%
  select(date,value) %>% rename("CPI" = value)

CPIAllLFEDF <- fredr("CPILFESL") %>%
  select(date,value) %>% rename("CPILFE" = value)

##################################################### GDP DATA #####################################################
GDPDFLineCodeMaster <- data.frame("LineCode" = c(3,6,10,11,12,34,35,36,45,51,56,60,64,65,69,70,76,79,82,84,85,86),
                                  "Industry" = GDPIndustries) %>%
  mutate(Sector = ifelse(LineCode>83,"Government","Private Industries")) %>% .[,c(1,3,2)]


StateGDPDF <- future_lapply(GDPDFLineCodeMaster$LineCode,function(x) {
  
  Sector <- GDPDFLineCodeMaster$Sector[match(x,GDPDFLineCodeMaster$LineCode)]
  
  Industry <- GDPDFLineCodeMaster$Industry[match(x,GDPDFLineCodeMaster$LineCode)]
  
  beaSpecs <- list(
    "UserID" = BEAKey,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SQGDP2",
    "LineCode" = x,
    "GeoFips" = "STATE",
    "Year" = BEAYears,
    "ResultFormat" = "json"
  )
  beaGet(beaSpec = beaSpecs) %>%
    filter(GeoName %in% state.name) %>%
    mutate(State = GeoName,Region = state.region[match(State,state.name)],Industry = Industry, 
           Sector = Sector,Statistic = "GDP, Current USD", .before = "Code") %>%
    select(-c(Code,GeoFips,GeoName)) %>%
    pivot_longer(8:ncol(.),names_to = "Quarter",values_to = "GDP") %>%
    mutate(Quarter = gsub("^DataValue_","",Quarter),
           Date = as.Date(paste(
             substr(Quarter,1,4),
             as.integer(substr(Quarter,6,6))*3-2,1,
             sep = "-"))) %>%
    .[,c(1:7,10,9)]
}) %>%
  reduce(full_join)



##################################################### HC DATA #####################################################
HCDFLineCodeMaster <- data.frame("LineCode" = c(70,1:19*100,2001,2002,2010),
                                 "Industry" = HumanCapitalIndustries) %>%
  mutate(Sector = ifelse(LineCode>2000,"Government","Private Industries")) %>% .[,c(1,3,2)]


StateHCDF <- future_lapply(HCDFLineCodeMaster$LineCode,function(x) {
  
  Sector <- HCDFLineCodeMaster$Sector[match(x,HCDFLineCodeMaster$LineCode)]
  
  Industry <- HCDFLineCodeMaster$Industry[match(x,HCDFLineCodeMaster$LineCode)]
  
  beaSpecs <- list(
    "UserID" = BEAKey,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SAEMP25N",
    "LineCode" = x,
    "GeoFips" = "STATE",
    "Year" = BEAYears,
    "ResultFormat" = "json"
  )
  beaGet(beaSpec = beaSpecs) %>%
    filter(GeoName %in% state.name) %>%
    mutate(State = GeoName,Region = state.region[match(State,state.name)],Industry = Industry, 
           Sector = Sector,Statistic = "Jobs", .before = "Code") %>%
    select(-c(Code,GeoFips,GeoName)) %>%
    pivot_longer(8:ncol(.),names_to = "Year",values_to = "Jobs",
                 names_transform = function(x) {as.integer(gsub("DataValue_","",x))})
  
}) %>%
  reduce(full_join)


