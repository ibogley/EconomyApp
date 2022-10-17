library(tidyverse)
library(bea.R)
key <- Sys.getenv("BEA_API_KEY")

beaSpecs <- list(
  "UserID" = key,
  "Method" = "GetData",
  "datasetname" = "Regional",
  "TableName" = "CAGDP2",
  "LineCode" = 1,
  "GeoFips" = "COUNTY",
  "Year" = "2020",
  "ResultFormat" = "json"
)

beaParams(beaKey = key,"Regional") %>% view()

data <- beaGet(beaSpecs) 

data %>%
  filter(grepl("^41.*",GeoFips))