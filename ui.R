#Load initial SIC section names - need to populate dropdown in the UI
#Saved in server
sectors<- readRDS('data/initialSectionNames.rds')


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

    titlePanel("South Yorkshire business map"),

    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                
                tabPanel("Map", 
                         sidebarLayout(
                           sidebarPanel(
                             h4(strong("Explore sectors")),
                             p(
                               'Drag & zoom'
                             ),
                             summary_input_panel()
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
