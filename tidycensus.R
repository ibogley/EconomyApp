library(tidycensus)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

variable_df <- load_variables(2020,"acs5") %>%
  filter(concept == "EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER") %>% .[-1,] %>%
  mutate(label = gsub("Estimate!!Total:!!","",label))

variables <- variable_df %>% pull(name)
names(variables) <- variable_df$label

data <- get_acs("state",variables = variables)

data %>% filter(NAME == "Oregon") %>% data.frame() %>%
  mutate(estimate_andbelow = cumsum(estimate))

