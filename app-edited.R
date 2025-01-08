
library(shiny)
library(tidyverse)
library(rvest)
library(readxl)
library(readODS)
library(geojsonio)
library(gglaplot)
library(highcharter)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)

source('scripts/funs.R')


DOWNLOAD_LATEST_DATA <- F

SURVEY_PATH <- "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods"
BOUNDS_REGION_PATH <- "data/regions-simplified3-topo.json"
BOUNDS_BOROUGH_PATH <- "data/boroughs-simplified3-topo.json"
RELEASE_YEAR <- '2023_24'

REGION_QUESTION_NUM <- 1:12
#G2 not working
REGION_QUESTION_LIST <- list( 
  code=c(
    'A2','A30','A54','B2',
    'C2','C5',
    'D2','D5','E2','E5',
    'G2','I5',
    'H2',
    'J2','J6','J7','J8','J9','J10','J12'
  ),
  theme=c(
    'Arts','Arts','Arts','Arts',
    'Libraries','Libraries',
    'Heritage','Heritage','Heritage','Heritage',
    'Sport','Sport',
    'Tourism',
    'Internet','Internet','Internet','Internet','Internet','Internet','Internet'
  ),
  color=c('blue','blue','blue','blue',
                'pink','pink',
                'green','green','green','green',
                'red','red',
                'purple',
                'orange','orange','orange','orange','orange','orange','orange'
  )
)

BOROUGH_QUESTION_LIST <- list( 
  code=c(
    'A3','A31','A55','B3',
    'C3','C6',
    'D3','D6','E3','E6',
    '','',
    '',
    'J3','','','','','',''
  ),
  theme=c(
    'Arts','Arts','Arts','Arts',
    'Libraries','Libraries',
    'Heritage','Heritage','Heritage','Heritage',
    '','',
    'Tourism',
    'Internet','','','','','',''
  ),
  color=c('blue','blue','blue','blue',
                'pink','pink',
                'green','green','green','green',
                '','',
                '',
                'orange','','','','','',''
  )
)
QUESTION_LIST <- list('region'=REGION_QUESTION_LIST, 'borough'=BOROUGH_QUESTION_LIST)
#DRILLDOWN_FLAGs <- 



ARTS_QUESTIONS <- 1:4
LIBRARIES_QUESTIONS <- 5:6
HERITAGE_QUESTIONS <- 7:10
SPORT_QUESTIONS <- 11:12


`%ni%` <- Negate(`%in%`)

#'[____________________________________________________________________________]

# output_list <- lapply(
#   1:length(REGION_QUESTION_LIST[[1]]), function(q) {
#     #print(q)
#     generate_regional_plot(
#       SURVEY_PATH, RELEASE_YEAR,
#       list('code'=REGION_QUESTION_LIST[[1]][q],'theme'=REGION_QUESTION_LIST[[2]][q], 'color'=REGION_QUESTION_LIST[[3]][q])
#     )
#   }
# )

#list('code'=QUESTION_LIST[[1]][[1]][1],'theme'=QUESTION_LIST[[1]][[2]][1], 'color'=QUESTION_LIST[[1]][[3]][1])
bounds_region <- geojsonio::geojson_read(BOUNDS_REGION_PATH, what = "list")
bounds_borough <- geojsonio::geojson_read(BOUNDS_BOROUGH_PATH, what = "list")
options("scipen"=100, "digits"=4)

df_list <- lapply(
  #1:length(QUESTION_LIST[['region']][['code']]), function(q) {
  1:12, function(q) {
    
    df_region <- generate_region_frame(
      SURVEY_PATH, 
      RELEASE_YEAR,
      list('code'=QUESTION_LIST[['region']][['code']][[q]],'theme'=QUESTION_LIST[['region']][['theme']][q], 'color'=QUESTION_LIST[['region']][['color']][q])
    )
    
    if (QUESTION_LIST[['borough']][['code']][[q]]!="") {
      df_region[['dataframe']] <- df_region[['dataframe']] %>%
        mutate(
          drilldown_central = case_when(region=='London'~'london-central'),
          drilldown_error = case_when(region=='London'~'london-error')
        )
      df_borough <- generate_borough_frame(
        SURVEY_PATH, 
        RELEASE_YEAR,
        list('code'=QUESTION_LIST[['borough']][['code']][[q]],'theme'=QUESTION_LIST[['borough']][['theme']][q], 'color'=QUESTION_LIST[['borough']][['color']][q])
      )
      return(list('region'=df_region, 'borough'=df_borough))
    }
    else {
      df_region[['dataframe']]$drilldown_central <- ''
      df_region[['dataframe']]$drilldown_error <- ''
      return(list('region'=df_region))
    }
  }
)



#source('scripts/master.R')
# knitr::opts_chunk$set(echo = TRUE,scipen=999)
# knitr::opts_chunk$set(warning = FALSE, message = FALSE) 


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    includeCSS("FORMATTING/GLAstyle.css"),
    useShinyjs(rmd=T),
    fluidRow(
      column(7,#style = "background-color: #fafafa !important;",
             div(class='tab-panel-ui',
                 shinyjs::hidden(
                   pickerInput(
                     inputId="arts_select",
                     label="Question",
                     choices = ARTS_QUESTIONS,
                     selected=ARTS_QUESTIONS[1],
                     multiple=F
                   )
                 )
                 ,
                 # div(style = "position:absolute;left:20vw; display: flex; justify-content: space-between;", 
                 #     uiOutput("arts_previous_btn"),
                 #     uiOutput("arts_next_btn")
                 # ),
                 navset_bar(
                   #nav_item(HTML('<span style="border-left:<b>Controls</b>')),
                   #type = "pills", 
                   nav_panel(
                     'Chart view', 
                     htmltools::tagAppendAttributes(
                       shinyWidgets::awesomeCheckboxGroup(
                         inputId='arts_compOps',
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
                       highchartOutput('arts_chart', height='88vh')
                     )
                   ),
                   nav_panel(
                     'Map view',
                     shinycssloaders::withSpinner(
                       highchartOutput('arts_map', height='88vh')
                     )
                   ),
                   nav_spacer(),
                   nav_spacer(),
                   nav_spacer(),
                   nav_spacer(),
                   nav_spacer(),
                   nav_spacer(),
                   nav_item(uiOutput("arts_previous_btn")),
                   nav_item(uiOutput("arts_next_btn")),
                   
                   
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
               h2('Arts', style='color:#6da7ded9 !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
             ),
             fluidRow(
               div(
                 style = "background-color: #6da7ded9 !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
                 div(class='text-ui',
                     htmlOutput("arts_region_text"),
                     #HTML('&darr;')
                     shinyjs::hidden(
                       div(id='arts-text-drilldown',
                           icon('arrow-down-long'),
                           div(style='height:1.2vh'),
                           htmlOutput("arts_borough_text")
                       )
                     )
                 )
                 
               )
               
             )
      )
    ),
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
    ),
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
    ),
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  
  output$arts_previous_btn <- renderUI({
    actionButton(
      "arts_previous", 
      "Previous",
      icon=icon("backward")
    )
  })
  output$arts_next_btn <- renderUI({
    actionButton(
      "arts_next", 
      "Next",
      icon=icon("forward")
    )
  })
  # output$selected <- renderText({
  #   paste0('You have selected ', input$arts_select)
  # })
  output$arts_chart <- renderHighchart({
    generate_drilldown_chart(input$arts_select, df_list, 'arts')
  })
  output$arts_map <- renderHighchart({
    generate_drilldown_map(input$arts_select, df_list, bounds_region, bounds_borough)
  })
  
  
  output$arts_region_text <- renderUI({
    generate_region_text(input$arts_select, df_list)
  })
  output$arts_borough_text <- renderUI({
    generate_borough_text(input$arts_select, df_list)
  })
  
  
  observeEvent(input$arts_previous, {
    current <- which(ARTS_QUESTIONS == input$arts_select)
    if(current > 1) {
      updateSelectInput(
        session, "arts_select",
        selected = ARTS_QUESTIONS[current - 1]
      )
      update_drilldown_chart(input$arts_select, df_list, "arts_chart") 
      update_drilldown_map(input$arts_select, df_list, "arts_map")
      generate_region_text(input$arts_select, df_list)
      shinyjs::html(id = 'arts_region_text', html =  generate_region_text(input$arts_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  # 
  observeEvent(input$arts_next, {
    current <- which(ARTS_QUESTIONS == input$arts_select)
    if(current < length(ARTS_QUESTIONS)){
      updateSelectInput(
        session, "arts_select",
        selected = ARTS_QUESTIONS[current + 1]
      )
      update_drilldown_chart(input$arts_select, df_list, "arts_chart") 
      update_drilldown_map(input$arts_select, df_list, "arts_map")
      shinyjs::html(id = 'arts_region_text', html = generate_region_text(input$arts_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  
  observeEvent(c(input$arts_compOps, input$arts_currLevel, input$arts_select), {
    
    print(input$arts_currLevel)
    question <- as.numeric(input$arts_select)
    print(question)
    df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
    df_borough <- df_list[[question ]][['borough']][['dataframe']]
    if ('error'%in%input$arts_compOps & 'mean'%ni%input$arts_compOps) {
      #browser()
      if (is.null(input$arts_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:0,
                      color:'#ffffff00',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$arts_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:0,
                      color:'#ffffff00'',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:0,
                      color:'#ffffff00',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else if ('mean'%in%input$arts_compOps & 'error'%ni%input$arts_compOps) {
      #browser()
      if (is.null(input$arts_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
      }
      else if (input$arts_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      
    }
    else if ('mean'%in%input$arts_compOps & 'error'%in%input$arts_compOps) {
      if (is.null(input$arts_currLevel)) {
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$arts_currLevel==0) {
        
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        
      }
      else{
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else {
      #browser()#
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
            )
      )
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#arts_chart').highcharts();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:0,
                      color:'#ffffff00',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
            )
      )
      
      #browser()
    }
  }, ignoreInit=T, ignoreNULL=T, priority=0)
  
  
  
  observeEvent(
    c(input$arts_currLevel), {
      
      #browser()
      req(input$arts_currLevel)
      selected <- input$arts_compOps
      if (input$arts_currLevel==0) {
        updateAwesomeCheckboxGroup(
          session, "arts_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('arts_chart') %>%
          hcpxy_update(
            subtitle=list(
              text="Click 'Back to Regions' to drillup"
            )
          )
        #highchartProxy
        
      }
      else {
        updateAwesomeCheckboxGroup(
          session, "arts_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('arts_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }
      
      observeEvent(c(input$arts_select), {
        updateAwesomeCheckboxGroup(
          session, "arts_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('arts_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  observeEvent(c(input$arts_currLevel, input$currLevelMap), {
    
    req(input$arts_currLevel)
    if (input$arts_currLevel==0) {
      shinyjs::show('arts-text-drilldown')
    }
    else  {
      shinyjs::hide('arts-text-drilldown')
    }
    observeEvent(c(input$arts_select), {
      shinyjs::hide('arts-text-drilldown')
    }, ignoreInit=T, ignoreNULL=T, priority=-2)
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  
  output$libraries_previous_btn <- renderUI({
    actionButton(
      "libraries_previous", 
      "Previous",
      icon=icon("backward")
    )
  })
  output$libraries_next_btn <- renderUI({
    actionButton(
      "libraries_next", 
      "Next",
      icon=icon("forward")
    )
  })
  # output$selected <- renderText({
  #   paste0('You have selected ', input$libraries_select)
  # })
  output$libraries_chart <- renderHighchart({
    generate_drilldown_chart(input$libraries_select, df_list, 'libraries')
  })
  output$libraries_map <- renderHighchart({
    generate_drilldown_map(input$libraries_select, df_list, bounds_region, bounds_borough)
  })
  
  
  output$libraries_region_text <- renderUI({
    generate_region_text(input$libraries_select, df_list)
  })
  output$libraries_borough_text <- renderUI({
    generate_borough_text(input$libraries_select, df_list)
  })
  
  
  observeEvent(input$libraries_previous, {
    current <- which(LIBRARIES_QUESTIONS == input$libraries_select)
    if(current > 1) {
      updateSelectInput(
        session, "libraries_select",
        selected = LIBRARIES_QUESTIONS[current - 1]
      )
      update_drilldown_chart(input$libraries_select, df_list, "libraries_chart") 
      update_drilldown_map(input$libraries_select, df_list, "libraries_map")
      generate_region_text(input$libraries_select, df_list)
      shinyjs::html(id = 'libraries_region_text', html =  generate_region_text(input$libraries_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  # 
  observeEvent(input$libraries_next, {
    current <- which(LIBRARIES_QUESTIONS == input$libraries_select)
    if(current < length(LIBRARIES_QUESTIONS)){
      updateSelectInput(
        session, "libraries_select",
        selected = LIBRARIES_QUESTIONS[current + 1]
      )
      update_drilldown_chart(input$libraries_select, df_list, "libraries_chart") 
      update_drilldown_map(input$libraries_select, df_list, "libraries_map")
      shinyjs::html(id = 'libraries_region_text', html = generate_region_text(input$libraries_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  
  observeEvent(c(input$libraries_compOps, input$libraries_currLevel, input$libraries_select), {
    
    print(input$libraries_currLevel)
    question <- as.numeric(input$libraries_select)
    print(question)
    df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
    df_borough <- df_list[[question ]][['borough']][['dataframe']]
    if ('error'%in%input$libraries_compOps & 'mean'%ni%input$libraries_compOps) {
      #browser()
      if (is.null(input$libraries_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$libraries_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else if ('mean'%in%input$libraries_compOps & 'error'%ni%input$libraries_compOps) {
      #browser()
      if (is.null(input$libraries_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
      }
      else if (input$libraries_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      
    }
    else if ('mean'%in%input$libraries_compOps & 'error'%in%input$libraries_compOps) {
      if (is.null(input$libraries_currLevel)) {
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$libraries_currLevel==0) {
        
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        
      }
      else{
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else {
      #browser()#
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
            )
      )
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#libraries_chart').highchlibraries();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
            )
      )
      
      #browser()
    }
  }, ignoreInit=T, ignoreNULL=T, priority=0)
  observeEvent(
    c(input$libraries_currLevel), {
      
      #browser()
      req(input$libraries_currLevel)
      selected <- input$libraries_compOps
      if (input$libraries_currLevel==0) {
        updateAwesomeCheckboxGroup(
          session, "libraries_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('libraries_chart') %>%
          hcpxy_update(
            subtitle=list(
              text="Click 'Back to Regions' to drillup"
            )
          )
        #highchartProxy
        
      }
      else {
        updateAwesomeCheckboxGroup(
          session, "libraries_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('libraries_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }
      
      observeEvent(c(input$libraries_select), {
        updateAwesomeCheckboxGroup(
          session, "libraries_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('libraries_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  observeEvent(c(input$libraries_currLevel, input$currLevelMap), {
    
    req(input$libraries_currLevel)
    if (input$libraries_currLevel==0) {
      shinyjs::show('libraries-text-drilldown')
    }
    else  {
      shinyjs::hide('libraries-text-drilldown')
    }
    observeEvent(c(input$libraries_select), {
      shinyjs::hide('libraries-text-drilldown')
    }, ignoreInit=T, ignoreNULL=T, priority=-2)
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  
  output$heritage_previous_btn <- renderUI({
    actionButton(
      "heritage_previous", 
      "Previous",
      icon=icon("backward")
    )
  })
  output$heritage_next_btn <- renderUI({
    actionButton(
      "heritage_next", 
      "Next",
      icon=icon("forward")
    )
  })
  # output$selected <- renderText({
  #   paste0('You have selected ', input$heritage_select)
  # })
  output$heritage_chart <- renderHighchart({
    generate_drilldown_chart(input$heritage_select, df_list, 'heritage')
  })
  output$heritage_map <- renderHighchart({
    generate_drilldown_map(input$heritage_select, df_list, bounds_region, bounds_borough)
  })
  
  
  output$heritage_region_text <- renderUI({
    generate_region_text(input$heritage_select, df_list)
  })
  output$heritage_borough_text <- renderUI({
    generate_borough_text(input$heritage_select, df_list)
  })
  
  
  observeEvent(input$heritage_previous, {
    current <- which(HERITAGE_QUESTIONS == input$heritage_select)
    if(current > 1) {
      updateSelectInput(
        session, "heritage_select",
        selected = HERITAGE_QUESTIONS[current - 1]
      )
      update_drilldown_chart(input$heritage_select, df_list, "heritage_chart") 
      update_drilldown_map(input$heritage_select, df_list, "heritage_map")
      generate_region_text(input$heritage_select, df_list)
      shinyjs::html(id = 'heritage_region_text', html =  generate_region_text(input$heritage_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  # 
  observeEvent(input$heritage_next, {
    current <- which(HERITAGE_QUESTIONS == input$heritage_select)
    if(current < length(HERITAGE_QUESTIONS)){
      updateSelectInput(
        session, "heritage_select",
        selected = HERITAGE_QUESTIONS[current + 1]
      )
      update_drilldown_chart(input$heritage_select, df_list, "heritage_chart") 
      update_drilldown_map(input$heritage_select, df_list, "heritage_map")
      shinyjs::html(id = 'heritage_region_text', html = generate_region_text(input$heritage_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  
  observeEvent(c(input$heritage_compOps, input$heritage_currLevel, input$heritage_select), {
    
    print(input$heritage_currLevel)
    question <- as.numeric(input$heritage_select)
    print(question)
    df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
    df_borough <- df_list[[question ]][['borough']][['dataframe']]
    if ('error'%in%input$heritage_compOps & 'mean'%ni%input$heritage_compOps) {
      #browser()
      if (is.null(input$heritage_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$heritage_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else if ('mean'%in%input$heritage_compOps & 'error'%ni%input$heritage_compOps) {
      #browser()
      if (is.null(input$heritage_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
      }
      else if (input$heritage_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      
    }
    else if ('mean'%in%input$heritage_compOps & 'error'%in%input$heritage_compOps) {
      if (is.null(input$heritage_currLevel)) {
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$heritage_currLevel==0) {
        
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        
      }
      else{
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else {
      #browser()#
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
            )
      )
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#heritage_chart').highchheritage();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
            )
      )
      
      #browser()
    }
  }, ignoreInit=T, ignoreNULL=T, priority=0)
  observeEvent(
    c(input$heritage_currLevel), {
      
      #browser()
      req(input$heritage_currLevel)
      selected <- input$heritage_compOps
      if (input$heritage_currLevel==0) {
        updateAwesomeCheckboxGroup(
          session, "heritage_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('heritage_chart') %>%
          hcpxy_update(
            subtitle=list(
              text="Click 'Back to Regions' to drillup"
            )
          )
        #highchartProxy
        
      }
      else {
        updateAwesomeCheckboxGroup(
          session, "heritage_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('heritage_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }
      
      observeEvent(c(input$heritage_select), {
        updateAwesomeCheckboxGroup(
          session, "heritage_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('heritage_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  observeEvent(c(input$heritage_currLevel, input$currLevelMap), {
    
    req(input$heritage_currLevel)
    if (input$heritage_currLevel==0) {
      shinyjs::show('heritage-text-drilldown')
    }
    else  {
      shinyjs::hide('heritage-text-drilldown')
    }
    observeEvent(c(input$heritage_select), {
      shinyjs::hide('heritage-text-drilldown')
    }, ignoreInit=T, ignoreNULL=T, priority=-2)
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  
  output$sport_previous_btn <- renderUI({
    actionButton(
      "sport_previous", 
      "Previous",
      icon=icon("backward")
    )
  })
  output$sport_next_btn <- renderUI({
    actionButton(
      "sport_next", 
      "Next",
      icon=icon("forward")
    )
  })
  # output$selected <- renderText({
  #   paste0('You have selected ', input$sport_select)
  # })
  output$sport_chart <- renderHighchart({
    generate_drilldown_chart(input$sport_select, df_list, 'sport')
  })
  output$sport_map <- renderHighchart({
    generate_drilldown_map(input$sport_select, df_list, bounds_region, bounds_borough)
  })
  
  
  output$sport_region_text <- renderUI({
    generate_region_text(input$sport_select, df_list)
  })
  output$sport_borough_text <- renderUI({
    generate_borough_text(input$sport_select, df_list)
  })
  
  
  observeEvent(input$sport_previous, {
    current <- which(SPORT_QUESTIONS == input$sport_select)
    if(current > 1) {
      updateSelectInput(
        session, "sport_select",
        selected = SPORT_QUESTIONS[current - 1]
      )
      update_drilldown_chart(input$sport_select, df_list, "sport_chart") 
      update_drilldown_map(input$sport_select, df_list, "sport_map")
      generate_region_text(input$sport_select, df_list)
      shinyjs::html(id = 'sport_region_text', html =  generate_region_text(input$sport_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  # 
  observeEvent(input$sport_next, {
    current <- which(SPORT_QUESTIONS == input$sport_select)
    if(current < length(SPORT_QUESTIONS)){
      updateSelectInput(
        session, "sport_select",
        selected = SPORT_QUESTIONS[current + 1]
      )
      update_drilldown_chart(input$sport_select, df_list, "sport_chart") 
      update_drilldown_map(input$sport_select, df_list, "sport_map")
      shinyjs::html(id = 'sport_region_text', html = generate_region_text(input$sport_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  
  observeEvent(c(input$sport_compOps, input$sport_currLevel, input$sport_select), {
    
    print(input$sport_currLevel)
    question <- as.numeric(input$sport_select)
    print(question)
    df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp_lb, prop_resp_ub, drilldown_error)
    df_borough <- df_list[[question ]][['borough']][['dataframe']]
    if ('error'%in%input$sport_compOps & 'mean'%ni%input$sport_compOps) {
      #browser()
      if (is.null(input$sport_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$sport_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else if ('mean'%in%input$sport_compOps & 'error'%ni%input$sport_compOps) {
      #browser()
      if (is.null(input$sport_currLevel)) {
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
      }
      else if (input$sport_currLevel==0) { # drill level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else { # top level
        #browser()
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
                )
              )
             
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      
    }
    else if ('mean'%in%input$sport_compOps & 'error'%in%input$sport_compOps) {
      if (is.null(input$sport_currLevel)) {
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
      else if (input$sport_currLevel==0) {
        
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_borough$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_borough$prop_resp_lb),",
                      to:",mean(df_borough$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        
      }
      else{
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:",mean(df_region_central$prop_resp),",
                      color:'#d82222',
                      zIndex:99
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
        delay(10,
              shinyjs::runjs(
                paste0(
                  "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:",mean(df_region_error$prop_resp_lb),",
                      to:",mean(df_region_error$prop_resp_ub),",
                      color:'#d822221F',
                      zIndex:98
                    }]
                  });
                console.log(chart);
             "
                )
              )
        )
      }
    }
    else {
      #browser()#
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotBands:
                    [{
                      from:0,
                      to:0,
                      color:'#d822221F',
                      zIndex:98

                    }]
                  });
                console.log(chart);
             "
            )
      )
      delay(10,
            shinyjs::runjs(
              "
                var chart = $('#sport_chart').highchsport();
                  chart.yAxis[0].update({plotLines:
                    [{
                      value:-10,
                      color:'#d82222',
                      zIndex:99

                    }]
                  });
                console.log(chart);
             "
            )
      )
      
      #browser()
    }
  }, ignoreInit=T, ignoreNULL=T, priority=0)
  observeEvent(
    c(input$sport_currLevel), {
      
      #browser()
      req(input$sport_currLevel)
      selected <- input$sport_compOps
      if (input$sport_currLevel==0) {
        updateAwesomeCheckboxGroup(
          session, "sport_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('sport_chart') %>%
          hcpxy_update(
            subtitle=list(
              text="Click 'Back to Regions' to drillup"
            )
          )
        #highchartProxy
        
      }
      else {
        updateAwesomeCheckboxGroup(
          session, "sport_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('sport_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }
      
      observeEvent(c(input$sport_select), {
        updateAwesomeCheckboxGroup(
          session, "sport_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        highchartProxy('sport_chart') %>%
          hcpxy_update(
            subtitle=list(
              text='Click to drilldown into London by Borough'
            )
          )
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  observeEvent(c(input$sport_currLevel, input$currLevelMap), {
    
    req(input$sport_currLevel)
    if (input$sport_currLevel==0) {
      shinyjs::show('sport-text-drilldown')
    }
    else  {
      shinyjs::hide('sport-text-drilldown')
    }
    observeEvent(c(input$sport_select), {
      shinyjs::hide('sport-text-drilldown')
    }, ignoreInit=T, ignoreNULL=T, priority=-2)
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
