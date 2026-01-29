# ML sector definitions - the four machine-learned categories
ml_sectors <- c("health_tech", "clean_energy", "advanced_manufacturing", "defence")

# Human-readable labels for display
ml_sector_labels <- c(
  "health_tech" = "Health Tech",
  "clean_energy" = "Clean Energy",
  "advanced_manufacturing" = "Advanced Manufacturing",
  "defence" = "Defence"
)

default_sector = "health_tech"

# ui elements  ----

sectorselect_input_panel <-
  function(){
    selectInput(
      inputId = 'sector_chosen',
      label = 'Select ML sector:',
      choices = setNames(ml_sectors, ml_sector_labels),
      selected = default_sector,
      selectize = TRUE
    )
  }


toggleSwitch <- 
  function(){
    materialSwitch(inputId = "mapdisplayvar_switch", label = HTML("<b>Show most recent employee count OR change in employees since previous accounts</b>"),value = TRUE)
  }


# Panel layouts ----


about_tab_panel <- 
  function(title){
    tabPanel(title,  
             fluidRow(
               column(width = 11, includeMarkdown("text/about.md"), offset = 1)
             )
    )
  }



summary_panel <-
  function(title){
    tabPanel(
      title,
      
    )
  }





# Site layout ----

fluidPage(
  
  useShinyjs(),
  
  titlePanel("South Yorkshire Companies House map"),
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              
              tabPanel("Map", 
                       sidebarLayout(
                         sidebarPanel(
                           h4(strong("Drag/zoom on map")),
                           h4(strong("Hover for name")),
                           h4(strong("Click on firms for more details")),
                          sectorselect_input_panel(),
                          sliderInput("employee_count_range",
                                      label = "Employee count range (inclusive):",
                                      min = 0, max = 500, value = c(1, 500)),
                          toggleSwitch(),
                           # uiOutput("mapdisplayvar_switch")#placeholder, will create toggle switch in server
                           htmlOutput("firm_count"),#reactive displays current count of firms being shown on map    
                           htmlOutput("employee_count")#reactive displays current count of employees in selected firms
                         ),
                         mainPanel(
                           leafletOutput("map", height = 1000))
                       ) 
              ),
              
              summary_panel('Analytics'),
              
              about_tab_panel('About')
              
  )                
  
  
)
