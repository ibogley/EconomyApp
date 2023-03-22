RPPDFLineCodeMaster <- data.frame("LineCode" = c(1,2,3,4,5),
                                  "Description" = RPPDescriptions)


StateRPPDF <- future_lapply(RPPDFLineCodeMaster$LineCode,function(x) {
  
  Description <- RPPDFLineCodeMaster$Description[match(x,RPPDFLineCodeMaster$LineCode)]
  
  beaSpecs <- list(
    "UserID" = BEAKey,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SARPP",
    "LineCode" = x,
    "GeoFips" = "STATE",
    "Year" = BEAYears,
    "ResultFormat" = "json"
  )
  beaGet(beaSpec = beaSpecs) %>%
    mutate(State = GeoName,Region = state.region[match(State,state.name)], Description = Description,
           Statistic = "Regional Price Parity, % of National", .before = "Code") %>%
    select(-c(Code,GeoFips,GeoName)) %>%
    pivot_longer(7:ncol(.),names_to = "Year",values_to = "RPP") %>%
    mutate(Year = as.integer(gsub("^DataValue_","",Year)))
}) %>%
  reduce(full_join)
