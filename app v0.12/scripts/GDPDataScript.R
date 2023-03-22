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
