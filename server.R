#Installed check for package "reactlog", for visualising reactive graph
#Base "R package installed" check: https://stackoverflow.com/a/38082613/5023561
is_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if(is_installed("reactlog")){
  cat('Reactlog installed, enabling. Use CTRL/CMD + F3 to open reactive graph plot.\n')
  options(shiny.reactlog = TRUE)
}


#DATA (including a little pre-processing)----


#Pre-processed below, saved, loaded above it

#companies house data for South Yorkshire
# ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire.rds')

#Version with SIC digit types in a single column
ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire_long.rds')

#Convert to latlon and resave
# ch <- ch %>% st_transform("EPSG:4326")

#Two entries with employee values filled in by error, from financials
#Remove
# ch <- ch %>% filter(!CompanyNumber %in% c('08638732','11756651'))
# saveRDS(ch,'data/companieshouse_employees_n_sectors_southyorkshire.rds')

#Make SIC digit names long, so selecting by digit level is a simple filter, not faffing with columns
# ch <- ch %>%
#   select(-c(SICCode.SicText_1:SICCode.SicText_4,SIC_5DIGIT_CODE,SIC_2DIGIT_CODE,SIC_2DIGIT_CODE_NUMERIC,SIC_3DIGIT_CODE,SIC_SECTION_LETTER,SIC_SECTION_CODE)) %>%
#   pivot_longer(cols = SIC_5DIGIT_NAME:SIC_SECTION_NAME, names_to = "SIC_digit", values_to = "sector_name", cols_vary = "slowest")
# 
# #That's duplicating values so is larger than the original, by about 50%
# pryr::object_size(ch)

#Update the digit values so is friendly in the dropdown. Here or column names, both fine!
# ch <- ch %>% 
#   mutate(
#     SIC_digit = case_when(
#       SIC_digit == "SIC_5DIGIT_NAME" ~ "5 digit",  
#       SIC_digit == "SIC_2DIGIT_NAME" ~ "2 digit",  
#       SIC_digit == "SIC_3DIGIT_NAME" ~ "3 digit",  
#       SIC_digit == "SIC_SECTION_NAME" ~ "Section"
#     )
#   )

#Save those digit names for the UI while we're here
# saveRDS(unique(ch$SIC_digit)[c(4,2,3,1)],'data/initialSICDigitNames.rds')

# saveRDS(ch,'data/companieshouse_employees_n_sectors_southyorkshire_long.rds')


#Ordered SIC lookup, so sectors are listed in correct order in dropdown (see prepcode)
SIClookup_long <- readRDS('data/SICorderedlookup.rds')



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

#Un-geo first (otherwise distinct gets every unique geo-point!)
# sectors <- ch %>%
#   st_set_geometry(NULL) %>%
#   select(SICdigitnames[selectedSIClevel]) %>%
#   filter(!is.na(.[,1])) %>% # get rid of NAs in the one remaining column
#   distinct() %>%
#   pull

#Save copy so UI to use when loaded...
#Will load in there so don't need initial code here
# saveRDS(sectors,'data/initialSectionNames.rds')



#SERVER FUNCTIONS----

function(input, output, session) {
 
  #OBSERVED EVENTS----
  
  #What's going on with firm data filters:
  #A single reactive filter for each different selection type e.g. sector name or employee range
  #A single reactive filter that combines all those
  #That last reactive filter is the one that leaflet proxy reactive knows about
  
  #Filter SIC digit type when digit dropdown changes
  filter_by_SICdigit <- reactive({
    
    df <- ch %>% filter(
      SIC_digit == input$sicdigit_chosen
    )
    
    glimpse(df)
    
    #When SIC digit is changed, the choice of sectors needs updating in the dropdown to those in that digit
    #TODO: better defaults for sector when changing digit (one based on previous selection?)
    
    newsectors <- SIClookup_long %>% filter(SIC_digit == isolate(input$sicdigit_chosen)) %>% select(sector_name) %>% pull %>% unique
    # newsectors <- unique(df$sector_name)
    
    # cat("New sectors: ", newsectors,"\n")
    
    # selectedsector = 64
    
    #quick hack to check issue
    if(isolate(input$sicdigit_chosen) == "Section") {
      selectedsector = 3}
    else if (isolate(input$sicdigit_chosen) == "2 digit") {
      selectedsector = 64
    }
    
    cat('Sector autoselected: ', newsectors[selectedsector],'\n')
    
    updateSelectInput(
      session,
      'sector_chosen',
      choices = newsectors,
      selected = newsectors[selectedsector]
    )
    
    cat('df being used in SIC digit selection trigger:\n')
    glimpse(df)
    
    return(df)
    
  })
  
  #Filter sector when sector dropdown changes
  filter_by_sector <- reactive({
    
    df <- ch %>% filter(
      sector_name == input$sector_chosen
    )
    
    cat('df being used in sector selection trigger:\n')
    glimpse(df)
    
    
  })
  
  #Filter sector when sector dropdown changes
  filter_by_employee <- reactive({
    
    df <- NULL
    
    #Check that change doesn't go beyond available data range
    # if(
    #   isolate(input$employee_count_range[1]) >= min(isolate(filter_by_sector()$Employees_thisyear)) &
    #   isolate(input$employee_count_range[2]) <= max(isolate(filter_by_sector()$Employees_thisyear))   
    #   ){
    # 
      df <- ch %>% filter(
        between(Employees_thisyear,input$employee_count_range[1],isolate(input$employee_count_range[2]))#isolate one of them, only get triggered once
      )
      
    # }
    
    cat('df being used in employee range selection trigger:\n')
    glimpse(df)
    cat('SIC digits present:\n')
    print(unique(df$SIC_digit))
    
    return(df)
    
  })
  
  
  #Combine different filters, retriggered if any of them changed, then invalidates leaflet proxy for redraw
  combined_filters <- reactive({
   
    cat("Final filter combination triggered....\n")
    
    #Take the SIC digit selection, filter down further by the sector selection
    #(Those sectors are unique, but keeping modular to tie to UI elements)
    df_subset <- filter_by_SICdigit() %>% filter(
      CompanyNumber %in% filter_by_sector()$CompanyNumber
    )
    
    cat('halfway df (sic digit and sector): \n')
    glimpse(df_subset)
    
    
    #Then filter further by the employee band and return result
    df_subset <- df_subset %>% filter(
      CompanyNumber %in% filter_by_employee()$CompanyNumber
      )
    # isolate(filter_by_sector()) %>% filter(
    #   CompanyNumber %in% filter_by_employee()$CompanyNumber
    
    cat('Final filtered df including employee range: \n')
    glimpse(df_subset)
    
    cat('component parts going into that (1) = filter_by_employee df (we just printed the subset, that should be fine): \n')
    glimpse(isolate(filter_by_employee()))
    
    return(df_subset)
     
  })
  
  
  #Update slider values if filtering by sector or SIC digit changes
  observe({
    
    #combo of both sector and digit
    df <- filter_by_SICdigit() %>% filter(CompanyNumber %in% filter_by_sector()$CompanyNumber)
    
    cat("Slider update code called. min and max employees this year:\n")
    cat(min(df$Employees_thisyear),",",max(df$Employees_thisyear),"\n")
    
    cat('df being used in slider update:\n')
    glimpse(df)
    
    #Add ten to min value if there are firms with more than ten employees, otherwise set to zero
    mintouse <- ifelse(min(df$Employees_thisyear) + 10 < max(df$Employees_thisyear), 10, 0)
    
    #Update slideer range for selected sector
    updateSliderInput(session, "employee_count_range",
                      value = c(mintouse, max(df$Employees_thisyear)),
                      min = 0,
                      max = max(df$Employees_thisyear),
                      step = 1
    )
    
    # #Update slideer range for selected sector
    # updateSliderInput(session, "employee_count_range", 
    #                   value = min(filter_by_sector()$Employees_thisyear) + 10, 
    #                   min = 0, 
    #                   max = max(isolate(filter_by_sector()$Employees_thisyear)), 
    #                   step = 1
    # )
    # 
    cat("range bar: ", isolate(input$employee_count_range),"\n")
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #MAP CODE------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## MAP FUNCTIONS----
  
  #Add in the selected firms
  draw_firms <- function(mapdata){
    
    #Clear previous circlemarkers
    leafletProxy("map") %>% clearGroup("firms")
    leafletProxy("map") %>% clearControls()#clear legend before new one drawn
    
    # palette <- colorNumeric(palette = "BrBG", domain = mapdata$Employees_thisyear, na.color="transparent")
    
    #Own-made bins using classInt
    #TODO: fix breaks when only single firms showing up (I think is the problem)
    fisher_breaks <- classInt::classIntervals(mapdata$Employees_thisyear, n = 7, style = "fisher")$brks
    
    palette <- colorBin(palette = "RdBu", bins = fisher_breaks, domain = ch.manuf$Employees_thisyear)
    
    
    leafletProxy('map') %>%
      addCircleMarkers(
        data = mapdata,
        label = ~Company,#label will be the marker hover
        radius = ~ scales::rescale(Employees_thisyear, c(1,50)),
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
  #For making sure data loads to map on initial load - only relevant if map isn't in first tab, but keep in case that changes
  # outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  
  
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
    
    draw_firms(combined_filters())
    
  })

}#END MAIN INPUT OUTPUT SESSION FUNCTION
