RoadsSF <- primary_roads() %>% st_transform(crs = 4326) %>%
  filter(grepl("I-",FULLNAME))


output$StateGraph <- renderPlot({
  if (input$StateOrRegion=="State") {
    
    
    if (input$State %in% c("Alaska","Hawaii")) {
      StateSF <- states() %>% filter(NAME == input$State) %>% 
        st_transform(crs = 4326)
    } else {
      StateSF <- NationalSF %>% filter(state_name == input$State) 
    }
    
    RoadsStateSF <- RoadsSF[unlist(st_contains(StateSF,RoadsSF)[1]),]
    
    if (input$State == "Alaska") {
      
      StateSF %>%
        ggplot() + geom_sf()+
        geom_sf(data = RoadsStateSF,color = "blue") +
        coord_sf(crs = 4326,xlim = c(-180,-130)) + 
        theme(axis.ticks = element_blank(),legend.position = "none",
              axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
        labs(title = "Interstate Highways",color = "Primary Road")
      
    } else {
      
      StateSF %>%
        ggplot() + geom_sf() + 
        geom_sf(data = RoadsStateSF,color = "blue") +
        theme(axis.ticks = element_blank(),legend.position = "none",
              axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
        labs(title = "Interstate Highways")
      
    }
  } else if (input$StateOrRegion=="Region") {
    
    StateSF <- NationalSF %>% filter(state_region == input$Region) 
    
    RoadsSFRows <- unlist(future_lapply(state.name[state.region == input$Region],function(x) {
      unlist(st_contains(NationalSF %>% filter(state_name == x),RoadsSF))
    }))
    
    RoadsStateSF <- RoadsSF[RoadsSFRows,]
    
    StateSF %>%
      ggplot() + geom_sf() + 
      geom_sf(data = RoadsStateSF,color = "blue") +
      theme(axis.ticks = element_blank(),legend.position = "none",
            axis.text = element_blank(),plot.title = element_text(hjust = .5)) +
      labs(title = "Interstate Highways")
  }
  
})