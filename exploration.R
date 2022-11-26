library(plotly)

StateGDPIndustryDF %>% filter(Sector == "Private Industries",State == "Alabama") %>%
  ggplot(aes(x = Date, y = GDP,fill = Industry)) + geom_bar(stat = "identity")



StateGDPIndustryDF %>% group_by(Industry) %>% summarise(GDP = sum(GDP)) %>% .[order(.$GDP,decreasing = TRUE),]

RelevantIndustry <- c("Agriculture, forestry, fishing and hunting","Finance and insurance",
                      "Health care and social assistance","Manufacturing",
                      "Mining, quarrying, and oil and gas extraction","Real estate and rental and leasing")

p1 <- StateGDPIndustryDF %>% mutate(RelevantIndustry = ifelse(Industry %in% RelevantIndustry,Industry,"Other")) %>%
  group_by(Date,RelevantIndustry) %>% summarise(GDP = sum(GDP)) %>%
  ggplot(aes(x = Date, y = GDP, fill = RelevantIndustry)) +
  geom_bar(stat = "identity")

p2 <- StateGDPIndustryDF %>% mutate(RelevantIndustry = ifelse(Industry %in% RelevantIndustry,Industry,"Other")) %>%
  group_by(Date,RelevantIndustry) %>% summarise(GDP = sum(GDP)) %>% 
  mutate(QuarterGDP = sum(ifelse(.$Date==Date,GDP,0)),PctGDP = GDP/QuarterGDP) %>%
  ggplot(aes(x = Date, y = PctGDP, color = RelevantIndustry)) + geom_line()


ggplotly(p1)
ggplotly(p2)


StateGDPRelevantIndustryDF <- StateGDPIndustryDF %>% mutate(RelevantIndustry = ifelse(Industry %in% RelevantIndustry,Industry,"Other")) %>%
  group_by(Date,RelevantIndustry) %>% summarise(GDP = sum(GDP)) %>% 
  mutate(TotalQuarterGDP = sum(ifelse(.$Date==Date,GDP,0)),PctGDP = GDP/TotalQuarterGDP)

data.frame(RelevantIndustry = unique(StateGDPRelevantIndustryDF$RelevantIndustry)) %>%
  mutate(
    Pct2005Q1 = StateGDPRelevantIndustryDF$PctGDP[
      StateGDPRelevantIndustryDF$Date==min(StateGDPRelevantIndustryDF$Date)&
        StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry] %>%
      round(digits = 4),
    Pct2022Q2 = StateGDPRelevantIndustryDF$PctGDP[
      StateGDPRelevantIndustryDF$Date==max(StateGDPRelevantIndustryDF$Date)&
        StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry] %>%
      round(digits = 4),
    TotalGrowthPctofTotal = 
      (StateGDPRelevantIndustryDF$PctGDP[
        StateGDPRelevantIndustryDF$Date==max(StateGDPRelevantIndustryDF$Date)&
          StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry]-
      StateGDPRelevantIndustryDF$PctGDP[
        StateGDPRelevantIndustryDF$Date==min(StateGDPRelevantIndustryDF$Date)&
          StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry]) %>%
      round(digits = 4),
    GDP2005Q1 = 
      StateGDPRelevantIndustryDF$GDP[
        StateGDPRelevantIndustryDF$Date==min(StateGDPRelevantIndustryDF$Date)&
          StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry] %>%
      round(digits = -3),
    GDP2022Q2 = 
      StateGDPRelevantIndustryDF$GDP[
        StateGDPRelevantIndustryDF$Date==max(StateGDPRelevantIndustryDF$Date)&
          StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry] %>%
      round(digits = -3),
    TotalGrowth = 
      (StateGDPRelevantIndustryDF$GDP[
        StateGDPRelevantIndustryDF$Date==max(StateGDPRelevantIndustryDF$Date)&
          StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry]-
      StateGDPRelevantIndustryDF$GDP[
        StateGDPRelevantIndustryDF$Date==min(StateGDPRelevantIndustryDF$Date)&
          StateGDPRelevantIndustryDF$RelevantIndustry==RelevantIndustry]) %>%
      round(digits = -3)
  ) %>%
  arrange(-Pct2022Q2)


StateGDPIndustryDF %>% filter(Date == max(.$Date)) %>% group_by(Industry) %>% summarise(GDP = sum(GDP)) %>% 
  mutate(PctGDP = GDP/sum(.$GDP)) %>%
  .[order(.$PctGDP,decreasing = TRUE),]
