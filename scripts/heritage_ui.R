fluidPage(
  includeCSS("FORMATTING\\GLAstyle.css"),
  useShinyjs(rmd=T),
  fluidRow(
    column(7,#style = "background-color: #fafafa !important;",
           div(class='tab-panel-ui',
               shinyjs::hidden(
                 pickerInput(
                   inputId="heritage_select",
                   label="Question",
                   choices = HERITAGE_QUESTIONS,
                   selected=1,
                   multiple=F
                 )
               )
               ,
               # div(style = "position:absolute;left:20vw; display: flex; justify-content: space-between;", 
               #     uiOutput("heritage_previous_btn"),
               #     uiOutput("heritage_next_btn")
               # ),
               navset_bar(
                 #nav_item(HTML('<span style="border-left:<b>Controls</b>')),
                 #type = "pills", 
                 nav_panel(
                   'Chart view', 
                   htmltools::tagAppendAttributes(
                     shinyWidgets::awesomeCheckboxGroup(
                       inputId='heritage_compOps',
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
                     highchartOutput('heritage_chart', height='88vh')
                   )
                 ),
                 nav_panel(
                   'Map view',
                   shinycssloaders::withSpinner(
                     highchartOutput('heritage_map', height='88vh')
                   )
                 ),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_item(uiOutput("heritage_previous_btn")),
                 nav_item(uiOutput("heritage_next_btn")),
                 
                 
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
             h2('Heritage', style='color:#5ea15d !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
           ),
           fluidRow(
             div(
               style = "background-color: #5ea15d !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
               div(class='text-ui',
                   htmlOutput("heritage_region_text"),
                   #HTML('&darr;')
                   shinyjs::hidden(
                     div(id='heritage-text-drilldown',
                         icon('arrow-down-long'),
                         div(style='height:1.2vh'),
                         htmlOutput("heritage_borough_text")
                     )
                   )
               )
               
             )
             
           )
    )
  )
)