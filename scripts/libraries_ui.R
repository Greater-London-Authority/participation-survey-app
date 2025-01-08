fluidPage(
  includeCSS("FORMATTING\\GLAstyle.css"),
  useShinyjs(rmd=T),
  fluidRow(
    column(7,#style = "background-color: #fafafa !important;",
           div(class='tab-panel-ui',
               shinyjs::hidden(
                 pickerInput(
                   inputId="libraries_select",
                   label="Question",
                   choices = LIBRARIES_QUESTIONS,
                   selected=1,
                   multiple=F
                 )
               )
               ,
               # div(style = "position:absolute;left:20vw; display: flex; justify-content: space-between;", 
               #     uiOutput("libraries_previous_btn"),
               #     uiOutput("libraries_next_btn")
               # ),
               navset_bar(
                 #nav_item(HTML('<span style="border-left:<b>Controls</b>')),
                 #type = "pills", 
                 nav_panel(
                   'Chart view', 
                   htmltools::tagAppendAttributes(
                     shinyWidgets::awesomeCheckboxGroup(
                       inputId='libraries_compOps',
                       label='',
                       choices=c(
                         'England mean'='mean',
                         'England error (95% CI)'='error'
                       ),
                       selected=c('mean'),
                       inline=T
                     ),
                     class='comp-ops-append'
                   ),
                   shinycssloaders::withSpinner(
                     highchartOutput('libraries_chart', height='88vh')
                   )
                 ),
                 nav_panel(
                   'Map view',
                   shinycssloaders::withSpinner(
                     highchartOutput('libraries_map', height='88vh')
                   )
                 ),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_item(uiOutput("libraries_previous_btn")),
                 nav_item(uiOutput("libraries_next_btn")),
                 
                 
                 nav_item(
                   shinyjs::hidden(
                     pickerInput(
                       inputId = "hidden1",
                       choices = list(
                         Group1 = c("1"),
                         Group2 = c("A")
                       )
                     )
                   )
                 )
                 ,
                 fluidRow(
                   column(3),
                   column(5)#,
                 )
               )
           )
    ),
    column(5,
           fluidRow(
             h2('Libraries', style='color:#ff38bad9 !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
           ),
           fluidRow(
             div(
               style = "background-color: #ff38bad9 !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
               div(class='text-ui',
                   htmlOutput("libraries_region_text"),
                   #HTML('&darr;')
                   shinyjs::hidden(
                     div(id='libraries-text-drilldown',
                         icon('arrow-down-long'),
                         div(style='height:1.2vh'),
                         htmlOutput("libraries_borough_text")
                     )
                   )
               )
               
             )
             
           )
    )
  )
)