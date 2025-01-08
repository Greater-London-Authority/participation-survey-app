fluidPage(
  includeCSS("FORMATTING\\GLAstyle.css"),
  useShinyjs(rmd=T),
  fluidRow(
    column(7,#style = "background-color: #fafafa !important;",
           div(class='tab-panel-ui',
               shinyjs::hidden(
                 pickerInput(
                   inputId="sport_select",
                   label="Question",
                   choices = SPORT_QUESTIONS,
                   selected=1,
                   multiple=F
                 )
               )
               ,
               # div(style = "position:absolute;left:20vw; display: flex; justify-content: space-between;", 
               #     uiOutput("sport_previous_btn"),
               #     uiOutput("sport_next_btn")
               # ),
               navset_bar(
                 #nav_item(HTML('<span style="border-left:<b>Controls</b>')),
                 #type = "pills", 
                 nav_panel(
                   'Chart view', 
                   htmltools::tagAppendAttributes(
                     shinyWidgets::awesomeCheckboxGroup(
                       inputId='sport_compOps',
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
                     highchartOutput('sport_chart', height='88vh')
                   )
                 ),
                 nav_panel(
                   'Map view',
                   shinycssloaders::withSpinner(
                     highchartOutput('sport_map', height='88vh')
                   )
                 ),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_item(uiOutput("sport_previous_btn")),
                 nav_item(uiOutput("sport_next_btn")),
                 
                 
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
             h2('Sport', style='color:#d82222 !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
           ),
           fluidRow(
             div(
               style = "background-color: #d82222 !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
               div(class='text-ui',
                   htmlOutput("sport_region_text"),
                   #HTML('&darr;')
                   shinyjs::hidden(
                     div(id='sport-text-drilldown',
                         icon('arrow-down-long'),
                         div(style='height:1.2vh'),
                         htmlOutput("sport_borough_text")
                     )
                   )
               )
               
             )
             
           )
    )
  )
)