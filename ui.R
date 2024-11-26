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
      selectize = T
    )
  }

sicdigitselect_input_panel <-
  function(){
    selectInput(
      inputId = 'sicdigit_chosen',
      label = 'Select SIC digit level (Section to 5 digit)',
      choices = readRDS('data/initialSICDigitNames.rds'),
      selected = default_digit,
      selectize = T
    )
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
      title
    )
  }





# Site layout ----

fluidPage(

    titlePanel("South Yorkshire Companies House map"),

    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                
                tabPanel("Map", 
                         sidebarLayout(
                           sidebarPanel(
                             h4(strong("Explore sectors")),
                             sectorselect_input_panel(),
                             sicdigitselect_input_panel(),
                             sliderInput("employee_count_range",
                                         label = "Employee count range (inclusive):",
                                         min = 0, max = 957, value = c(11,957)),
                             htmlOutput("firm_count")#reactive displays current count of firms being shown on map    
                           ),
                           mainPanel(
                             leafletOutput("map", height = 1000))
                         ) 
                ),
                
                summary_panel('Analytics'),
                
                about_tab_panel('About')
                
    )                
    

)
