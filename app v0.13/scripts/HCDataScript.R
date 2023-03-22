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
