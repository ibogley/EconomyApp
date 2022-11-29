DFLineCodeMaster <- data.frame("LineCode" = c(3,6,10,11,12,34,35,36,45,51,56,60,64,65,69,70,76,79,82,84,85,86),
                               "Industry" = c("Agriculture, forestry, fishing and hunting","Mining, quarrying, and oil and gas extraction",
                                              "Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade",
                                              "Transportation and warehousing","Information","Finance and insurance",
                                              "Real estate and rental and leasing","Professional, scientific, and technical services",
                                              "Management of companies and enterprises",
                                              "Administrative and support and waste management and remediation services",
                                              "Educational services","Health care and social assistance","Arts, entertainment, and recreation",
                                              "Accommodation and food services","Other services (except government and government enterprises)",
                                              "Federal civilian","Military","State and local")) %>%
  mutate(Sector = ifelse(LineCode>83,"Government","Private Industries")) %>% .[,c(1,3,2)]


StateGDPDF <- future_lapply(c(3,6,10,11,12,34,35,36,45,51,56,60,64,65,69,70,76,79,82,84,85,86),function(x) {
  
  Sector <- DFLineCodeMaster$Sector[match(x,DFLineCodeMaster$LineCode)]
  
  Industry <- DFLineCodeMaster$Industry[match(x,DFLineCodeMaster$LineCode)]
  
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