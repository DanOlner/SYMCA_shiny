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
ch <- readRDS('data/companieshouse_employees_n_sectors_southyorkshire_long.rds') %>%
  select(Company,CompanyNumber,IncorporationDate,enddate,Employees_thisyear,Employees_lastyear,SIC_digit,sector_name)

ch <- ch %>%
  mutate(
    employee_diff_percent = ((Employees_thisyear - Employees_lastyear)/Employees_lastyear) * 100
  )

#Keep only most recent account date to avoid duplication
#(Duplicates from e.g. older accounts have varying employee numbers, so combining in final filter combo gets mismatched vals compared to what's asked via employee number slider)
#TODO: process so time series of values from multiple accounts can be displayed
ch <- ch %>% 
  group_by(CompanyNumber) %>% 
  filter(enddate == max(enddate)) %>% 
  ungroup()





#Quick hack to check toggle switch works - 
#Use existing col name being used, reassign that to the two display columns
#So rename the original first
#Mutate: 1 - copy employees_thisyear into new col employees_mostrecent
#We'll then use "Employees_thisyear" as the stand in "variable column" to overwrite
ch <- ch %>%
  mutate(
    employees_mostrecent = Employees_thisyear
  )


#Make longer so most recent employee count and percent change are in one column so can filter on it
#65mb before... 90mb after. Args for not just making longer the whole time! Toh it's in server memory, not being sent to client
# chk <- ch %>%
#   pivot_longer(cols = c(Employees_thisyear,employee_diff_percent),names_to = 'display_val', values_to = 'value')

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

reactive_values <- 
  reactiveValues(
    ch = ch,#put the dataframe in a reactive context so other reactives can see it
    count_of_firms = 0
  )


#SERVER FUNCTIONS----

function(input, output, session) {
  
  disable("sicdigit_chosen") 
  
  # MISC REACTIVES----
  
  output$firm_count <- renderUI(HTML(paste0("Number of firms displayed: <strong>", reactive_values$count_of_firms, "</strong>")))
  
  #OBSERVED EVENTS FOR THE COMPANIES HOUSE DATAFRAME----
  
  
  #SWAP MAIN DISPLAY VARIABLE - EMPLOYEE COUNT VS PERCENT CHANGE SINCE LAST SUBMITTED ACCOUNTS
  #Doing as reactive so can control order - this needs to happen before subsequent filters
  # change_display_column <- reactive({
  #   
  #   #Just create a new column from one of the two we're using
  #   #That column has a single name, which will be used to display
  #   #And state of switch used to decide on legend (percent change will diverge across zero, so want different)
  #   inc('Toggle switch triggered.')
  #   ct("Current val: ",isolate(input$mapdisplayvar_switch))
  #   
  #   if(input$mapdisplayvar_switch == TRUE){
  #     
  #     #Use global / non reactive ch because we need to filter down (and back) from firms with two values for employee count
  #     reactive_values$ch <- ch %>% 
  #       mutate(
  #         Employees_thisyear = employees_mostrecent
  #         # displayvar = Employees_thisyear
  #       )
  #     
  #     #FALSE changes to percent diff - need to filter down to only firms with employee vals for both timepoints
  #     #TODO: add note to site saying that for % change, min employees last year is 5 or more
  #     
  #   } else {
  #     
  #     reactive_values$ch <- ch %>% 
  #       filter(
  #         !is.na(Employees_lastyear),
  #         Employees_lastyear > 4
  #         ) %>% 
  #       mutate(
  #         Employees_thisyear = employee_diff_percent
  #         # displayvar = employee_diff_percent
  #       )
  #     
  #   }
  #   
  #   ct("Current state of ch:")
  #   glimpse(reactive_values$ch)
  #   
  #   #Just return switch val to indicate has run
  #   return(isolate(input$mapdisplayvar_switch))
  #   
  # })
  # 
  
  
  
  #What's going on with firm data filters:
  #A single reactive filter for each different selection type e.g. sector name or employee range
  #A single reactive filter that combines all those
  #That last reactive filter is the one that leaflet proxy reactive knows about
  
  #Filter SIC digit type when digit dropdown changes
  filter_by_SICdigit <- reactive({
    
    inc("Filter by SIC digit called.")
    
    #Filter CH data to selected SIC digit
    df <- reactive_values$ch %>% filter(
      SIC_digit == input$sicdigit_chosen
    )
    
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
    
    
    #Update the sector selection to new SIC digit
      updateSelectInput(
        session,
        'sector_chosen',
        choices = newsectors,
        selected = newsectors[selectedsector]
      )
    
    # cat('df being used in SIC digit selection trigger:\n')
    # glimpse(df)
    
    return(df)
    
  })
  
  #Filter sector when sector dropdown changes
  filter_by_sector <- reactive({
    
    inc("Filter by sector called.")
    
    df <- reactive_values$ch %>% filter(
      sector_name == input$sector_chosen
    )
    
    # cat('df being used in sector selection trigger:\n')
    # glimpse(df)
    
    
  })
  
  #Filter sector when sector dropdown changes
  filter_by_employee <- reactive({
    
    inc("Filter by employee (value slider) called.")
    
    #isolate one of them, only get triggered once
    df <- reactive_values$ch %>% filter(
      employees_mostrecent >= input$employee_count_range[1] & employees_mostrecent <= isolate(input$employee_count_range[2])
      # Employees_thisyear >= input$employee_count_range[1] & Employees_thisyear <= isolate(input$employee_count_range[2])
    )
    
    ct('df being used in employee range selection trigger has this employee range:',min(df$employees_mostrecent),max(df$employees_mostrecent))
    # glimpse(df)
    # cat('SIC digits present:\n')
    # print(unique(df$SIC_digit))
    
    return(df)
    
  })
  
  
  #Combine different filters, retriggered if any of them changed, then invalidates leaflet proxy for redraw
  # combined_filters <- reactive({
  #   
  #   # cat(inc(),": Final filter combination triggered....\n")
  #   inc("Final filter combination triggered....")
  #   
  #   #Toggle first, to change display column
  #   #This will change the global ch dataframe, so don't need to do anything else here directly
  #   #It'll get picked up in the filters next
  #   ct("Setting current display column in ch dataframe INSIDE COMBINED_FILTERS, state is: ", change_display_column())
  #   
  #   #Take the SIC digit selection, filter down further by the sector selection
  #   #(Those sectors are unique, but keeping modular to tie to UI elements)
  #   df_subset <- filter_by_SICdigit() %>% filter(
  #     CompanyNumber %in% filter_by_sector()$CompanyNumber
  #   )
  #   
  #   # cat('halfway df (sic digit and sector): \n')
  #   # glimpse(df_subset)
  #   
  #   
  #   #Then filter further by the employee band and return result
  #   df_subset <- df_subset %>% filter(
  #     CompanyNumber %in% filter_by_employee()$CompanyNumber
  #   )
  #   # isolate(filter_by_sector()) %>% filter(
  #   #   CompanyNumber %in% filter_by_employee()$CompanyNumber
  #   
  #   # cat('Final filtered df including employee range: \n')
  #   # glimpse(df_subset)
  #   
  #   # cat('component parts going into that (1) = filter_by_employee df (we just printed the subset, that should be fine): \n')
  #   # glimpse(isolate(filter_by_employee()))
  #   
  #   #set count of firms to display
  #   reactive_values$count_of_firms <- nrow(df_subset)
  #   
  #   #report all employee range values here
  #   # ct("Filter by SIC_digit -- employee range: ", 
  #   #    min(isolate(filter_by_SICdigit()$Employees_thisyear)),
  #   #    max(isolate(filter_by_SICdigit()$Employees_thisyear))
  #   # ) 
  #   # ct("Filter by sector -- employee range: ", 
  #   #    min(isolate(filter_by_sector()$Employees_thisyear)),
  #   #    max(isolate(filter_by_sector()$Employees_thisyear))
  #   # ) 
  #   # ct("Filter by employee -- employee range: ", 
  #   #    min(isolate(filter_by_employee()$Employees_thisyear)),
  #   #    max(isolate(filter_by_employee()$Employees_thisyear))
  #   # ) 
  #   
  #   #Save result for inspection
  #   # saveRDS(df_subset, 'local/final_filtered.rds')
  #   
  #   return(df_subset)
  #   
  # })

  
  
  
  
  combined_filters <- reactive({
    
    if(input$mapdisplayvar_switch == TRUE){
      
      #Use global / non reactive ch because we need to filter down (and back) from firms with two values for employee count
      reactive_values$ch <- ch %>% 
        mutate(
          Employees_thisyear = employees_mostrecent
          # displayvar = Employees_thisyear
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
          Employees_thisyear = employee_diff_percent
          # displayvar = employee_diff_percent
        )
      
    }
    
    df <- reactive_values$ch %>% 
      filter(
        sector_name == input$sector_chosen,
        SIC_digit == input$sicdigit_chosen,
        employees_mostrecent >= input$employee_count_range[1] & employees_mostrecent <= isolate(input$employee_count_range[2])
      )
    
    reactive_values$count_of_firms <- nrow(df)
    
    df
    
  })
  
  
  
  #Update slider values if filtering by sector or SIC digit changes
  observe({

    #combo of both sector and digit to get slider vals from
    #Can use global ch, just need that employee val range
    df <- ch %>% filter(
      sector_name == input$sector_chosen,
      SIC_digit == input$sicdigit_chosen
    )

    # cat(inc(),": Slider update code called. min and max employees this year:\n")
    # cat(min(df$Employees_thisyear),",",max(df$Employees_thisyear),"\n")
    inc(": 'Slider update values after sector/digit change' called. min and max employees this year: ",min(df$employees_mostrecent),",",max(df$employees_mostrecent))

    # cat('df being used in slider update:\n')
    # glimpse(df)

    #Add ten to min value if there are firms with more than ten employees, otherwise set to zero
    mintouse <- ifelse(min(df$employees_mostrecent) + 10 < max(df$employees_mostrecent), 10, 0)

    updateSliderInput(session, "employee_count_range",
                      value = c(mintouse, max(df$employees_mostrecent)),
                      min = 0,
                      max = max(df$employees_mostrecent),
                      step = 1
    )

    cat("range bar: ", isolate(input$employee_count_range),"\n")

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
    
    #Look at manuf, the default df being used on startup
    #Up to 60, there's every value
    #ch.manuf %>% st_set_geometry(NULL) %>% select(Employees_thisyear) %>% arrange(Employees_thisyear) %>% distinct() %>% View
    
    #This is just a cat wrapper that adds line break (so can be same inputs as inc)
    # ct("Making legend bins. Min max employee count here: ", min(mapdata$Employees_thisyear),max(mapdata$Employees_thisyear))
    
    #MAKE LEGEND, DIFFERENT ONE FOR EACH VARIABLE TYPE
   
    # debugonce(returnpalette)
    # ct("Toggle state prior to palette function call: ", isolate(change_display_column()))
    
    palette <- returnpalette(mapdata$Employees_thisyear, isolate(input$mapdisplayvar_switch), n = 7)
    
    #change colour for polarity of % change
    percenttext <- ifelse(
      mapdata$employee_diff_percent >0, 
      paste0('<span style="color: #64eb34">',round(mapdata$employee_diff_percent,2),'%</span>'),
      paste0('<span style="color: #eb3434">',round(mapdata$employee_diff_percent,2),'%</span>')
      )
    
    #Change shape of data so middling sized points are more prominent
    #Nice little "do for neg numbers too even tho makes no math sense" line from https://stackoverflow.com/a/64191142/5023561
    # mapdata <- mapdata %>% 
    #   mutate(
    #     tweaked_markersizevalue = ifelse(
    #       isolate(change_display_column()),#TRUE is employee count, FALSE is % diff
    #       sqrt(Employees_thisyear),
    #       sign(Employees_thisyear) * abs(Employees_thisyear)^(1 / 2)
    #     )
    #   )
    
    leafletProxy('map') %>%
      addCircleMarkers(
        data = mapdata,
        label = ~Company,#label will be the marker hover
        radius = ~ scales::rescale( Employees_thisyear , c(1, ifelse(isolate(input$mapdisplayvar_switch),50,30))),#smaller circles if change
        color = ~palette(Employees_thisyear),
        fillColor = ~palette(Employees_thisyear),
        opacity = 0.75,
        popup = paste0("<strong>",mapdata$Company,"</strong><br>","Employees this year: ",mapdata$employees_mostrecent,"<br>Employees last year: ",mapdata$Employees_lastyear,"<br>Change from last year: ",percenttext,"<br>Incorporation date: ",mapdata$IncorporationDate,'<br>Most recent accounts scraped: ',mapdata$enddate,'<br><strong><a href="',paste0("https://find-and-update.company-information.service.gov.uk/company/",mapdata$CompanyNumber),'">Companies House page</a>',"</strong>"),#this does NOT need to be in formula for some arbitrary reason
        group = 'firms'
      ) %>%
      addLegend("topright", pal = palette, values = mapdata$Employees_thisyear,
                title = ifelse(isolate(input$mapdisplayvar_switch),"Employee count","% change employees"),
                opacity = 1) %>%
      addScaleBar("topleft")
    
    
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
