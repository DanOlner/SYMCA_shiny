# ML sector definitions - the four machine-learned categories
ml_sectors <- c("health_tech", "clean_energy", "advanced_manufacturing", "defence")

# Human-readable labels for display
ml_sector_labels <- c(
  "health_tech" = "Health Tech",
  "clean_energy" = "Clean Energy",
  "advanced_manufacturing" = "Advanced Manufacturing",
  "defence" = "Defence"
)

# ui elements  ----

# Sector percentile sliders - filter firms by their classification strength in each sector
# Each sector has a toggle to enable/disable the filter, plus a range slider
sector_percentile_sliders <- function() {
  tagList(
    h5(strong("Filter by sector percentile (0-100%):")),
    p("Toggle ON to filter by that sector's percentile range."),

    # Health Tech
    fluidRow(
      column(3, materialSwitch("health_tech_enabled", "Health Tech", value = FALSE, status = "success")),
      column(9, sliderInput("health_tech_percentile_range",
                label = NULL,
                min = 0, max = 100, value = c(0, 100), step = 1))
    ),

    # Clean Energy
    fluidRow(
      column(3, materialSwitch("clean_energy_enabled", "Clean Energy", value = FALSE, status = "success")),
      column(9, sliderInput("clean_energy_percentile_range",
                label = NULL,
                min = 0, max = 100, value = c(0, 100), step = 1))
    ),

    # Advanced Manufacturing
    fluidRow(
      column(3, materialSwitch("advanced_manufacturing_enabled", "Adv. Manuf.", value = FALSE, status = "success")),
      column(9, sliderInput("advanced_manufacturing_percentile_range",
                label = NULL,
                min = 0, max = 100, value = c(0, 100), step = 1))
    ),

    # Defence
    fluidRow(
      column(3, materialSwitch("defence_enabled", "Defence", value = FALSE, status = "success")),
      column(9, sliderInput("defence_percentile_range",
                label = NULL,
                min = 0, max = 100, value = c(0, 100), step = 1))
    )
  )
}

# Commented out - replaced by percentile sliders
# sectorselect_input_panel <-
#   function(){
#     selectInput(
#       inputId = 'sector_chosen',
#       label = 'Select ML sector:',
#       choices = setNames(ml_sectors, ml_sector_labels),
#       selected = default_sector,
#       selectize = TRUE
#     )
#   }


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
                          sliderInput("employee_count_range",
                                      label = "Employee count range (inclusive):",
                                      min = 0, max = 500, value = c(1, 500)),
                          sector_percentile_sliders(),
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
