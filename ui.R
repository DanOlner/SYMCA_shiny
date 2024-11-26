#Load initial SIC section names - need to populate dropdown in the UI
#Saved in server
sectors<- readRDS('data/initialSectionNames.rds')

#Hardcoding range for now
#range(ch$Employees_thisyear[ch$SIC_SECTION_NAME=='Manufacturing'], na.rm = T)

# ui elements  ----

summary_input_panel <-
  function(){
    selectInput(
      inputId = 'sector_chosen',
      label = 'To choose a sector to view, begin inputting its name below:',
      choices = sectors,
      selected = 'Manufacturing',
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
                             p(
                               'Drag & zoom'
                             ),
                             summary_input_panel(),
                             sliderInput("employee_count_range",
                                         label = "Employee count range (inclusive):",
                                         min = 0, max = 957, value = c(11,957))#see above, hardcoding initial, will update when sector changed
                             # area_searcher_panel()
                           ),
                           mainPanel(
                             leafletOutput("map", height = 1000))
                         ) 
                ),
                
                summary_panel('Summary and plots'),
                
                about_tab_panel('About')
                
                
    )                
    

)
