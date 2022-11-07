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


lapply(c(3,6,10,11,12,34,35,36,45,51,56,60,64,65,69,70,76,79,82,84,85,86),function(x) {
  beaSpecs <- list(
    "UserID" = key,
    "Method" = "GetData",
    "datasetname" = "Regional",
    "TableName" = "SQGDP2",
    "LineCode" = x,
    "GeoFips" = "STATE",
    "Year" = "2022",
    "ResultFormat" = "json"
  )
  
  beaGet(beaSpecs,BEAKey)
})
