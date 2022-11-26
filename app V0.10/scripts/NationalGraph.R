output$StateOrRegionSelector <- renderUI({
  if (input$StateOrRegion=="State") {
    selectInput("State","State",c("None",state.name))
  } else if (input$StateOrRegion=="Region"){
    selectInput("Region","Region",c("None",as.character(unique(state.region))))
  }
  
})


NationalSF <- get_urbn_map(map = "states", sf = TRUE) %>% st_transform(crs = 4326) %>% 
  st_simplify(dTolerance = 5000) %>%
  mutate(state_name = state.name[match(state_abbv,state.abb)],
         state_region = state.region[match(state_abbv,state.abb)])


output$NationalGraph <- renderPlot({
  if (input$StateOrRegion=="State") {
    NationalSF %>%
      ggplot(aes(fill = state_name == input$State)) + geom_sf() + 
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.text = element_blank())
  } else if (input$StateOrRegion=="Region"){
    NationalSF %>%
      ggplot(aes(fill = state_region == input$Region)) + geom_sf() + 
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.text = element_blank())
  }
  
})
