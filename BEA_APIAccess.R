library(tidyverse)
library(bea.R)
key <- Sys.getenv("BEA_API_KEY")

beaSpecs <- list(
  "UserID" = key,
  "Method" = "GetData",
  "datasetname" = "Regional",
  "TableName" = "SQGDP2",
  "LineCode" = 1,
  "GeoFips" = "STATE",
  "Year" = "2022",
  "ResultFormat" = "json"
)

beaParams(beaKey = key,"Regional") %>% view()

data <- beaGet(beaSpecs) 

data %>%
  filter(grepl("^41.*",GeoFips))
