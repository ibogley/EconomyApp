library(bea.R)

BEAKey <- Sys.getenv("BEA_API_KEY")

beaParams(BEAKey,"NIPA")


BEAspecsInternational <- list(
  "UserID" = BEAKey,
  "Method" = "GetData",
  "datasetname" = "NIPA",
  "TableName" = "T40100",
  "GeoFips" = "ITA",
  "Year" = "2022",
  "Frequency" = "Q",
  "ResultFormat" = "json"
)

beaGet(BEAspecsInternational)

beaSearch("GDP") %>% view()
