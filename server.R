#Installed check for package "reactlog", for visualising reactive graph
#Base "R package installed" check: https://stackoverflow.com/a/38082613/5023561
is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if(is_inst("reactlog")){
  cat('Reactlog installed, enabling. Use CTRL/CMD + F3 to open reactive graph plot.\n')
  options(shiny.reactlog = TRUE)
}


#DATA (including a little pre-processing)----





#companies house data for South Yorkshire
ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire.rds')
#Convert to latlon and resave
# ch <- ch %>% st_transform("EPSG:4326")

#Two entries with employee values filled in by error, from financials
#Remove
# ch <- ch %>% filter(!CompanyNumber %in% c('08638732','11756651'))
# saveRDS(ch,'data/companieshouse_employees_n_sectors_southyorkshire.rds')





#South yorkshire local authority boundaries
# sy_boundaries <- st_read("data/mapdata/sy_localauthorityboundaries.shp")

#Convert to latlon and save as compressed
# sy_boundaries <- sy_boundaries %>% st_transform("EPSG:4326")
# saveRDS(sy_boundaries, 'data/mapdata/sy_localauthorityboundaries.rds')

#load once processed
sy_boundaries <- readRDS('data/mapdata/sy_localauthorityboundaries.rds')





#SIC digit names from the columns - needs reordering
SICdigitnames <- colnames(ch)[which(colnames(ch) %in% c('SIC_SECTION_NAME','SIC_2DIGIT_NAME','SIC_3DIGIT_NAME','SIC_5DIGIT_NAME'))][c(4,2,3,1)]

#Begin by viewing SIC section
selectedSIClevel <- 1

#Gets every geo point!
# sectors <- unique(ch[SICdigitnames[selectedSIC]])

#Un-geo first
# sectors <- ch %>%
#   st_set_geometry(NULL) %>%
#   select(SICdigitnames[selectedSIClevel]) %>%
#   filter(!is.na(.[,1])) %>% # get rid of NAs in the one remaining column
#   distinct() %>%
#   pull

#Save copy so UI to use when loaded...
#Will load in there so don't need initial code here
# saveRDS(sectors,'data/initialSectionNames.rds')


# REACTIVE VALUES----

reactive_values <- 
  reactiveValues(
    firms_to_view = ch,
    sic_level = selectedSIClevel, #What digit level to show? 1 is SIC section
    # sectors.livelist = readRDS('data/initialSectionNames.rds') # Named SIC sectors  - can change digit, so can drill down or up, need to repopulate this each time
  )






#SERVER FUNCTIONS----

function(input, output, session) {
 
  
  
  #OBSERVED EVENTS----
  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #MAP CODE------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##MAP FUNCTIONS----
  
  #User can choose which sector and sector type will be shown in the top level geography
  #Subset firm data to the appropriate sector(s)
  map_df = reactive({
    
    #Select sector name from appropriate sector SIC level column
    # firms_to_view <- ch %>% filter(SICdigitnames[reactive_values$sic_level] == input$sector_chosen)
    firms_to_view <- ch %>% filter(
      SIC_SECTION_NAME == isolate(input$sector_chosen),
      between(Employees_thisyear,isolate(input$employee_count_range[1]),isolate(input$employee_count_range[2]))
      )
    
    cat("In map_df. reactive_values$sic_level is: ", reactive_values$sic_level,"\n")
    cat("... input$sector_chosen is: ", input$sector_chosen,"\n")
    cat("... Size of filtered firms: ", firms_to_view %>% nrow,"\n")
    
    #Update slideer range for selected sector
    cat('min employees this year: ', min(firms_to_view$Employees_thisyear),'\n')
    
    # updateSliderInput(session, "employee_count_range", 
    #                   value = min(firms_to_view$Employees_thisyear) + 10, 
    #                   min = 0, 
    #                   max = max(firms_to_view$Employees_thisyear), 
    #                   step = 1
    # )
    
    cat("range bar: ", isolate(input$employee_count_range),"\n")
    
    
    return(firms_to_view)
    
  })
  
  
  
  #Add in the selected firms
  draw_firms <- function(mapdata){
    
    #Clear previous circlemarkers
    leafletProxy("map") %>% clearGroup("firms")
    leafletProxy("map") %>% clearControls()#clear legend before new one drawn
    
    # palette <- colorNumeric(palette = "BrBG", domain = mapdata$Employees_thisyear, na.color="transparent")
    
    #Own-made bins using classInt
    fisher_breaks <- classInt::classIntervals(mapdata$Employees_thisyear, n = 7, style = "fisher")$brks
    
    palette <- colorBin(palette = "RdBu", bins = fisher_breaks, domain = ch.manuf$Employees_thisyear)
    
    
    leafletProxy('map') %>%
      addCircleMarkers(
        data = mapdata,
        label = ~Company,#label will be the marker hover
        radius = ~ scales::rescale(Employees_thisyear, c(1,30)),
        color = ~palette(Employees_thisyear),
        fillColor = ~palette(Employees_thisyear),
        group = 'firms'
        # popup = paste0("Title", "<hr>", "Text 1", "<br>", "Text 2")) 
        ) %>%
      addLegend("topright", pal = palette, values = mapdata$Employees_thisyear,
                    title = "Employee count",
                    opacity = 1) %>%
      addScaleBar("topleft")
    
    
  }
  
  
  
  
  ##LEAFLET REACTIVES----
  
  #Initial map output creation (static elements only, dynamic changes in observes / leafletproxy)
  #See https://rstudio.github.io/leaflet/shiny.html
  output$map <- renderLeaflet({
    
    cat('Output map initial leaflet render.\n')
    
    
    #Only static elements, observe below will do the dynamics
    #Set zoom fractional jumps for a bit more zoom control
    #https://stackoverflow.com/a/62863122/5023561
    leaflet(options = leafletOptions(zoomSnap = 0.1, zoomDelta=0.7, minZoom = 7)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% #https://rstudio.github.io/leaflet/articles/basemaps.html
      # addTiles() %>%
      # setView(lng = -2, lat = 53, zoom = 7.2)#UK wide view
      # setView(lng = 0, lat = 51.4, zoom = 10)#London view
      setView(lng = -1.265910, lat = 53.441159, zoom = 10.7)#SY view
    
    
  })
  
  #https://stackoverflow.com/a/62701468/5023561
  #For making sure data loads to map on initial load
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  
  
  #Add initial dynamic elements
  observe({
    
    cat("Leaflet proxy call.\n")
    
    #Change map when variable changed
    #See https://rstudio.github.io/leaflet/shiny.html -
    #Avoids redrawing whole map after each change
    
    #Add in SY boundaries
    leafletProxy('map') %>%
      addPolygons(
        data = sy_boundaries,
        fill = F,
        color = 'black',
        weight = 2,
        opacity = 1
        # group = "sy_outline"
      ) 
    
    #Only call back to map_df reactive once (though it's cached unless input changes, so shouldn't matter...)
    mapdata <- map_df()
    
    draw_firms(mapdata)
    
  })
  
  
 
  
  
   

}#END INPUT OUTPUT SESSION FUNCTION
