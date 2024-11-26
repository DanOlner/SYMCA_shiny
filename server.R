#Installed check for package "reactlog", for visualising reactive graph
#Base "R package installed" check: https://stackoverflow.com/a/38082613/5023561
is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if(is_inst("reactlog")){
  cat('Reactlog installed, enabling. Use CTRL/CMD + F3 to open reactive graph plot.\n')
  options(shiny.reactlog = TRUE)
}


#DATA----

ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire.rds')

#SIC digit names from the columns - needs reordering
SICdigitnames <- colnames(ch)[which(colnames(ch) %in% c('SIC_SECTION_NAME','SIC_2DIGIT_NAME','SIC_3DIGIT_NAME','SIC_5DIGIT_NAME'))][c(4,2,3,1)]

#Begin by viewing SIC section
selectedSIClevel <- 1

#Gets every geo point!
# sectors <- unique(ch[SICdigitnames[selectedSIC]])

#Un-geo first
sectors <- ch %>%
  st_set_geometry(NULL) %>%
  select(SICdigitnames[selectedSIClevel]) %>%
  filter(!is.na(.[,1])) %>% # get rid of NAs in the one remaining column
  distinct() %>%
  pull

#Save copy so UI to use when loaded...
#Will load in there so don't need initial code here
# saveRDS(sectors,'data/initialSectionNames.rds')


# REACTIVE VALUES----

reactive_values <- 
  reactiveValues(
    sic_name = selectedSIClevel, #What digit level to show? 1 is SIC section
    sectors.livelist = readRDS('data/initialSectionNames.rds') # Named SIC sectors  - can change digit, so can drill down or up, need to repopulate this each time
  )






#SERVER FUNCTIONS----

function(input, output, session) {
 
  
  
  #OBSERVED EVENTS----
  
  ## This will set the sector first, from what the default in the input$sector_chosen is
  # observeEvent(input$sector_chosen,{
  #   
  #   cat('input$area_chosen observe triggered.\n')
  #   
  #   #problem this fixes: input invalidates as soon as a letter is deleted.
  #   #Could also use on of these as well, but let's just check the field is sensible before changing
  #   #https://shiny.rstudio.com/reference/shiny/1.7.0/debounce.html
  #   if(isolate(input$area_chosen) %in% isolate(sectors.livelist)){
  #     
  #     # drawLSOAs(isolate(map_df()))
  #     
  #     cat('And sectors found.\n')
  #     
  #   } else (
  #     
  #     cat('... but sectors not found yet. Hang on. \n')
  #     
  #   )
  #   
  # }, ignoreInit = T
  # )
  # 
  # 
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #MAP CODE------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##MAP FUNCTIONS----
  
  #User can choose which sector and sector type will be shown in the top level geography
  #Subset firm data to the appropriate sector(s)
  map_df = reactive({
    
    firms_to_view <- ch %>% filter(reactive_values$sic_name == input$sector_chosen)
    
    return(firms_to_view)
    
  })
  
  
  
  
  
  
  
  ##LEAFLET REACTIVES----
  
  #Initial map output creation (static elements only, dynamic changes in observes / leafletproxy)
  #See https://rstudio.github.io/leaflet/shiny.html
  output$map <- renderLeaflet({
    
    
    #Only static elements, observe below will do the dynamics
    #Set zoom fractional jumps for a bit more zoom control
    #https://stackoverflow.com/a/62863122/5023561
    leaflet(options = leafletOptions(zoomSnap = 0.1, zoomDelta=0.7, minZoom = 7)) %>%
      addTiles() %>%
      # setView(lng = -2, lat = 53, zoom = 7.2)#UK wide view
      setView(lng = 0, lat = 51.4, zoom = 10)#London view
    
  })
  
  #https://stackoverflow.com/a/62701468/5023561
  #For making sure data loads to map on initial load
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  
  
  #Add initial dynamic elements
  observe({
    
    cat("Leaflet proxy call.")
    
    #Change map when variable changed
    #See https://rstudio.github.io/leaflet/shiny.html -
    #Avoids redrawing whole map after each change
    
    #Only call back to map_df reactive once (though it's cached unless input changes, so shouldn't matter...)
    mapdata <- map_df()
    
    # draw_firms(mapdata)
    
  })
  
   

}#END INPUT OUTPUT SESSION FUNCTION
