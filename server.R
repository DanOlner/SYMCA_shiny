#Installed check for package "reactlog", for visualising reactive graph
#Base "R package installed" check: https://stackoverflow.com/a/38082613/5023561
is_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if(is_installed("reactlog")){
  cat('Reactlog installed, enabling. Use CTRL/CMD + F3 to open reactive graph plot.\n')
  options(shiny.reactlog = TRUE)
}




#DATA----

#See prepcode.R for some preprocessing to get ready for live use

#companies house data for South Yorkshire
#Version with SIC digit types in a single column
# ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire_long.rds') %>%
#   select(Company,CompanyNumber,IncorporationDate,enddate,Employees_thisyear,Employees_lastyear,SIC_digit,sector_name,employee_diff_percent)
# 
# ch <- ch %>% 
#   mutate(
#     employee_diff_percent = round(employee_diff_percent)
#   )

# Version just for ML-categorised firms
ch = readRDS('data/ch_ml_firms.rds')


#South yorkshire local authority boundaries
sy_boundaries <- readRDS('data/mapdata/sy_localauthorityboundaries.rds')

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

reactive_values <- 
  reactiveValues(
    ch = ch,#put the dataframe in a reactive context so other reactives can see it
    count_of_firms = 1,
    count_of_employees = 0,
    stored_slidermin = 0,#Used so we can revert to previous values if the slider range would make firm count zero
    stored_slidermax = 0,
    stored_maxfirmcount = 0
  )


#SERVER FUNCTIONS----

function(input, output, session) { 
  
  # MISC REACTIVES----
  
  #Change this first one depending on whether firm count is zero
  output$firm_count <- renderUI({
    if (reactive_values$count_of_firms == 0) {
      HTML(paste0('<span style="color: #eb3434"><strong>Zero firms have this employee count - change slider values</strong></span>'))
    } else {
      HTML(paste0("Number of firms displayed: <strong>", reactive_values$count_of_firms, "</strong>"))
    }
  })
  
  output$employee_count <- renderUI(HTML(paste0("Number of employees in selected firms: <strong>", reactive_values$count_of_employees, "</strong>")))

  # Disable sliders on startup (toggles default to OFF)
  shinyjs::disable("health_tech_percentile_range")
  shinyjs::disable("clean_energy_percentile_range")
  shinyjs::disable("advanced_manufacturing_percentile_range")
  shinyjs::disable("defence_percentile_range")

  # Enable/disable sliders based on toggle state
  observeEvent(input$health_tech_enabled, {
    if (input$health_tech_enabled) {
      shinyjs::enable("health_tech_percentile_range")
    } else {
      shinyjs::disable("health_tech_percentile_range")
    }
  })

  observeEvent(input$clean_energy_enabled, {
    if (input$clean_energy_enabled) {
      shinyjs::enable("clean_energy_percentile_range")
    } else {
      shinyjs::disable("clean_energy_percentile_range")
    }
  })

  observeEvent(input$advanced_manufacturing_enabled, {
    if (input$advanced_manufacturing_enabled) {
      shinyjs::enable("advanced_manufacturing_percentile_range")
    } else {
      shinyjs::disable("advanced_manufacturing_percentile_range")
    }
  })

  observeEvent(input$defence_enabled, {
    if (input$defence_enabled) {
      shinyjs::enable("defence_percentile_range")
    } else {
      shinyjs::disable("defence_percentile_range")
    }
  })

  #OBSERVED EVENTS FOR THE COMPANIES HOUSE DATAFRAME----
  
  #Filter SIC digit type when digit dropdown changes
  # filter_by_SICdigit <- reactive({
  #   
  #   inc("Filter by SIC digit called.")
  #   
  #   #Filter CH data to selected SIC digit
  #   df <- reactive_values$ch %>% filter(
  #     SIC_digit == input$sicdigit_chosen
  #   )
  #   
  #   #When SIC digit is changed, the choice of sectors needs updating in the dropdown to those in that digit
  #   #TODO: better defaults for sector when changing digit (one based on previous selection?)
  #   
  #   newsectors <- SIClookup_long %>% filter(SIC_digit == isolate(input$sicdigit_chosen)) %>% select(sector_name) %>% pull %>% unique
  #   # newsectors <- unique(df$sector_name)
  #   
  #   # cat("New sectors: ", newsectors,"\n")
  #   
  #   # selectedsector = 64
  #   
  #   #quick hack to check issue
  #   if(isolate(input$sicdigit_chosen) == "Section") {
  #     selectedsector = 3}
  #   else if (isolate(input$sicdigit_chosen) == "2 digit") {
  #     selectedsector = 64
  #   }
  #   
  #   cat('Sector autoselected: ', newsectors[selectedsector],'\n')
  #   
  #   
  #   #Update the sector selection to new SIC digit
  #     updateSelectInput(
  #       session,
  #       'sector_chosen',
  #       choices = newsectors,
  #       selected = newsectors[selectedsector]
  #     )
  #   
  #   # cat('df being used in SIC digit selection trigger:\n')
  #   # glimpse(df)
  #   
  #   return(df)
  #   
  # })
  
  
 
  #MAIN REACTIVE WHERE EACH INPUT FROM UI HAS DEPENDENCY
  #Some other observers needed for e.g. resetting employee range when sector or SIC digit changes
  combined_filters <- reactive({
    
    if(input$mapdisplayvar_switch == TRUE){
      
      #Use global / non reactive ch because we need to filter down (and back) from firms with two values for employee count
      reactive_values$ch <- ch %>% 
        mutate(
          mapdisplay_column = Employees_thisyear
        )
      
      #FALSE changes to percent diff - need to filter down to only firms with employee vals for both timepoints
      #TODO: add note to site saying that for % change, min employees last year is 5 or more
      
    } else {
      
      reactive_values$ch <- ch %>% 
        filter(
          !is.na(Employees_lastyear),
          Employees_lastyear > 4
        ) %>% 
        mutate(
          mapdisplay_column = employee_diff_percent
          # displayvar = employee_diff_percent
        )
      
    }
    
    # Filter by sector percentiles (only when toggle is enabled) and employee count
    # Percentile values are 0-1 in data, sliders are 0-100
    df <- reactive_values$ch %>%
      filter(
        # Only apply Health Tech filter if toggle is ON
        !input$health_tech_enabled | (
          health_tech_percentile >= input$health_tech_percentile_range[1] / 100 &
          health_tech_percentile <= input$health_tech_percentile_range[2] / 100
        ),
        # Only apply Clean Energy filter if toggle is ON
        !input$clean_energy_enabled | (
          clean_energy_percentile >= input$clean_energy_percentile_range[1] / 100 &
          clean_energy_percentile <= input$clean_energy_percentile_range[2] / 100
        ),
        # Only apply Advanced Manufacturing filter if toggle is ON
        !input$advanced_manufacturing_enabled | (
          advanced_manufacturing_percentile >= input$advanced_manufacturing_percentile_range[1] / 100 &
          advanced_manufacturing_percentile <= input$advanced_manufacturing_percentile_range[2] / 100
        ),
        # Only apply Defence filter if toggle is ON
        !input$defence_enabled | (
          defence_percentile >= input$defence_percentile_range[1] / 100 &
          defence_percentile <= input$defence_percentile_range[2] / 100
        ),
        # Employee count filter always applies
        Employees_thisyear >= input$employee_count_range[1],
        Employees_thisyear <= input$employee_count_range[2]
      )


    reactive_values$count_of_firms <- nrow(df)

    reactive_values$count_of_employees <- sum(df$Employees_thisyear, na.rm = T)

    df

  })
  
  
  
  # Commented out - no longer filtering by single sector, using percentile sliders instead

  # #Update slider values if filtering by sector changes
  # observe({
  #
  #   #Filter by sector to get slider vals from
  #   #Can use global ch, just need that employee val range
  #   df <- ch %>% filter(
  #     sector == input$sector_chosen
  #   )
  #
  #   inc('Setting slider vals after sector change. Size of df: ', nrow(df))
  #
  #   # cat(inc(),": Slider update code called. min and max employees this year:\n")
  #   # cat(min(df$Employees_thisyear),",",max(df$Employees_thisyear),"\n")
  #   ct(": 'Slider update values after sector change' called. min and max employees this year: ",min(df$Employees_thisyear),",",max(df$Employees_thisyear))
  #
  #   # cat('df being used in slider update:\n')
  #   # glimpse(df)
  #
  #   #Add ten to min value if there are firms with more than ten employees
  #   #And there are 25+ firms in this sector
  #   #therwise set to zero
  #   mintouse <- ifelse(min(df$Employees_thisyear) + 10 < max(df$Employees_thisyear) & nrow(df) > 24, 10, 0)
  #
  #   updateSliderInput(session, "employee_count_range",
  #                     value = c(mintouse, max(df$Employees_thisyear)),
  #                     min = 0,
  #                     max = max(df$Employees_thisyear),
  #                     step = 1
  #   )
  #
  #   #Store max df employees from here as will be correct value if need to reset elsewhere
  #   reactive_values$stored_maxfirmcount <- max(df$Employees_thisyear)
  #
  #   # ct("range bar now outputting: ", isolate(input$employee_count_range))
  #
  # })
  
  
  
  
  
  
  # #Check that slider changes leave at least four firms visible
  observeEvent(input$employee_count_range, {
    
    inc("observed slider change")
    
    reactive_values$stored_slidermin = isolate(input$employee_count_range[1])
    reactive_values$stored_slidermax = isolate(input$employee_count_range[2])

  })
 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  #MAP CODE------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## MAP FUNCTIONS----
  
  #Add in the selected firms
  draw_firms <- function(mapdata){
    
    #First cat wrapper increments counter
    inc("In draw_firms.")
    
    ct("Data going into map with this range: ",min(mapdata$Employees_thisyear),max(mapdata$Employees_thisyear))
    # glimpse(mapdata)
    
    ct("Has main df changed via toggle? Looks like this in draw_firms:")
    # glimpse(ch)
    
    
    
    #Clear previous circlemarkers
    leafletProxy("map") %>% clearGroup("firms")
    leafletProxy("map") %>% clearControls()#clear legend before new one drawn
    
    #Own-made bins using classInt
    #TODO: fix breaks when only single firms showing up (I think is the problem)
    
    #This is just a cat wrapper that adds line break (so can be same inputs as inc)
    ct("Making legend bins. Min max employee count here: ", min(mapdata$Employees_thisyear),max(mapdata$Employees_thisyear))
    
    #MAKE LEGEND, DIFFERENT ONE FOR EACH VARIABLE TYPE AS PERCENT DIFF IS NEG AND POS AND WANT NICE BREAK AT ZERO
   
    # debugonce(returnpalette)
    # ct("Toggle state prior to palette function call: ", isolate(change_display_column()))
    
    
    #DON'T RUN IF NO FIRMS TO SHOW
    if(nrow(mapdata) > 0){
      
      ct('Firms found. Going to attempt to make palette with following df:')
      glimpse(mapdata)
    
      palette <- returnpalette(mapdata$mapdisplay_column, isolate(input$mapdisplayvar_switch), n = 7)
      
      #change colour for polarity of % change
      percenttext <- ifelse(
        mapdata$employee_diff_percent >0, 
        paste0('<span style="color: #64eb34">',round(mapdata$employee_diff_percent,2),'%</span>'),
        paste0('<span style="color: #eb3434">',round(mapdata$employee_diff_percent,2),'%</span>')
        )
      
      #Change shape of data so middling sized points are more prominent
      if(isolate(input$mapdisplayvar_switch)){
        
        mapdata <- mapdata %>%
          mutate(
            tweaked_markersizevalue = sqrt(mapdisplay_column)
              )
        
      } else {
        
        mapdata <- mapdata %>%
          mutate(
            #Nice little "do for neg numbers too even tho makes no math sense" line from https://stackoverflow.com/a/64191142/5023561
            tweaked_markersizevalue = sign(mapdisplay_column) * abs(mapdisplay_column)^(1 / 2)
              )
  
      }
      
      # glimpse(mapdata)
      
      # minmarker = 15
      # maxmarker = 30
      
      minmarker = 30
      maxmarker = 50
      
      #First layer: clear dark marker to help other markers stand out (check performance...)
      leafletProxy('map') %>%
        addCircleMarkers(
          data = mapdata,
          radius = ~ scales::rescale( tweaked_markersizevalue , c(1, ifelse(isolate(input$mapdisplayvar_switch),maxmarker+4,minmarker+4))),#smaller circles if change
          color = 'black',
          weight = 2,
          fillOpacity = 0,
          opacity = 1.,
          group = 'firms'
        ) 
      
      #Top layer with labels etc
      leafletProxy('map') %>%
        addCircleMarkers(
          data = mapdata,
          label = ~Company,#label will be the marker hover
          radius = ~ scales::rescale( tweaked_markersizevalue , c(1, ifelse(isolate(input$mapdisplayvar_switch),maxmarker,minmarker))),#smaller circles if change
          color = ~palette(mapdisplay_column),
          fillColor = ~palette(mapdisplay_column),
          opacity = 0.75,
          popup = paste0('<br><strong><a href="https://',mapdata$website,'" target="_blank">',mapdata$Company,'</a>',"</strong><br>","Employees this year: ",mapdata$Employees_thisyear,"<br>Employees last year: ",mapdata$Employees_lastyear,"<br>Change from last year: ",percenttext,"<br>Incorporation date: ",mapdata$IncorporationDate,'<br>Most recent accounts scraped: ',mapdata$enddate,'<br><strong><a href="',paste0("https://find-and-update.company-information.service.gov.uk/company/",mapdata$CompanyNumber),'" target="_blank">Companies House page</a>',"</strong>"),#this does NOT need to be in formula for some arbitrary reason
          # popup = paste0("<strong>",mapdata$Company,"</strong><br>","Employees this year: ",mapdata$Employees_thisyear,"<br>Employees last year: ",mapdata$Employees_lastyear,"<br>Change from last year: ",percenttext,"<br>Incorporation date: ",mapdata$IncorporationDate,'<br>Most recent accounts scraped: ',mapdata$enddate,'<br><strong><a href="',paste0("https://find-and-update.company-information.service.gov.uk/company/",mapdata$CompanyNumber),'" target="_blank">Companies House page</a>',"</strong>"),#this does NOT need to be in formula for some arbitrary reason
          group = 'firms'
        ) %>%
        addLegend("topright", pal = palette, values = mapdata$mapdisplay_column,
                  title = ifelse(isolate(input$mapdisplayvar_switch),"Employee count","% change employees"),
                  opacity = 1) %>%
        addScaleBar("topleft")
      
      
     
      
    }#end if
    
    
    
  }
  
  
  ##LEAFLET REACTIVES----
  
  #Initial map output creation (static elements only, dynamic changes in observes / leafletproxy)
  #See https://rstudio.github.io/leaflet/shiny.html
  output$map <- renderLeaflet({
    
    inc('Output map initial leaflet render.')
    
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
    
    inc("Leaflet proxy call.")
    
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
    
    #Drawfirms is just a function, so reactive-wise, just part of this observe.
    #A change in combined_filters() invalidates this observe.
    draw_firms(combined_filters())
    
  })
  
}#END MAIN INPUT OUTPUT SESSION FUNCTION
