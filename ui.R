#Load initial SIC section names - need to populate dropdown in the UI
default_digit = "Section"
# default_digit = "2 digit"

#Load list of sectors based on what SIC digit we're viewing
sectors<- readRDS('data/SICorderedlookup.rds') %>% filter(SIC_digit == default_digit) %>% select(sector_name) %>% pull %>% unique


default_sector = "Manufacturing"
# default_sector = "72 : Scientific research and development"

# ui elements  ----

sectorselect_input_panel <-
  function(){
    selectInput(
      inputId = 'sector_chosen',
      label = 'Select sector from dropdown or begin inputting its name below:',
      choices = sectors,
      selected = default_sector,
      selectize = T,
      
    )
  }

sicdigitselect_input_panel <-
  function(){
    selectInput(
      inputId = 'sicdigit_chosen',
      label = 'Select SIC digit level (Section to 5 digit) [disabled]',
      choices = readRDS('data/initialSICDigitNames.rds'),
      selected = default_digit,
      selectize = T
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
                           sicdigitselect_input_panel(),
                           sliderInput("employee_count_range",
                                       label = "Employee count range (inclusive):",
                                       min = 0, max = 957, value = c(10,957)),
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
