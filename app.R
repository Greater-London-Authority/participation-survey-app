
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
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(scrollrevealR)
library(tinter)
library(countup)

source('scripts/funs.R')


DOWNLOAD_LATEST_DATA <- F
SURVEY_PATH <- "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods"
BOUNDS_REGION_PATH <- "data/regions-simplified3-topo.json"
BOUNDS_BOROUGH_PATH <- "data/london_421-simplify41.json"
RELEASE_YEAR <- '2023_24'
REGION_QUESTION_NUM <- 1:13

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
  color=c(
    'blue','blue','blue','blue',
    'pink','pink',
    'green','green','green','green',
    'red','red',
    'orange',
    'purple','purple','purple','purple','purple','purple','purple'
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
  color=c(
    'blue','blue','blue','blue',
    'pink','pink',
    'green','green','green','green',
    '','',
    '',
    'orange','','','','','',''
  )
)
QUESTION_LIST <- list('region'=REGION_QUESTION_LIST, 'borough'=BOROUGH_QUESTION_LIST)
ARTS_QUESTIONS <- 1:4
LIBRARIES_QUESTIONS <- 5:6
HERITAGE_QUESTIONS <- 7:10
SPORT_QUESTIONS <- 11:12
TOURISM_QUESTIONS <- 13
QUESTION_RANK_DIRECTION <- c(
  'D', 'A', 'A', 'D', 
  'D', 'D', 
  'D', 'D', 'D', 'D',
  'D', 'D',
  'D'
)
THEMES <- c('arts', 'libraries', 'heritage', 'sport', 'tourism')
`%ni%` <- Negate(`%in%`)

# RANKING ASC/DESC LIST

#'[____________________________________________________________________________]
bounds_region <- geojsonio::geojson_read(BOUNDS_REGION_PATH, what = "list")
bounds_borough <- geojsonio::geojson_read(BOUNDS_BOROUGH_PATH, what = "list")
options("scipen"=100, "digits"=4)

df_list <- lapply(
  #1:length(QUESTION_LIST[['region']][['code']]), function(q) {
  REGION_QUESTION_NUM, function(q) {
    region <- generate_region_frame(
      SURVEY_PATH, 
      RELEASE_YEAR,
      list('code'=QUESTION_LIST[['region']][['code']][[q]],'theme'=QUESTION_LIST[['region']][['theme']][q], 'color'=QUESTION_LIST[['region']][['color']][q]),
      QUESTION_RANK_DIRECTION[q]
    )
    
    if (QUESTION_LIST[['borough']][['code']][[q]]!="") {
      region[['dataframe']] <- region[['dataframe']] %>%
        mutate(
          drilldown_central = case_when(region=='London'~'london-central'),
          drilldown_error = case_when(region=='London'~'london-error')
        )
      borough <- generate_borough_frame(
        SURVEY_PATH, 
        RELEASE_YEAR,
        list('code'=QUESTION_LIST[['borough']][['code']][[q]],'theme'=QUESTION_LIST[['borough']][['theme']][q], 'color'=QUESTION_LIST[['borough']][['color']][q])
      )
      return(list('region'=region, 'borough'=borough))
    }
    else {
      region[['dataframe']]$drilldown_central <- ''
      region[['dataframe']]$drilldown_error <- ''
      return(list('region'=region))
    }
  }
)

df_headline <- generate_headline_frame(df_list, REGION_QUESTION_NUM, THEMES)






ui <- fluidPage(
  includeCSS("www/GLAstyle.css"),
  useShinyjs(),
  shinybrowser::detect(),
  
  
  #=============================================================================
  # Header UI
  #=============================================================================
  div(id='header_ui',
    includeHTML('header.html')
  ),
  
  #=============================================================================
  # Contents UI
  #=============================================================================
  # div(style="height:100vh; background-color: #ffffff",
  #   div(style="height: 50vh;",
  #   span(style="color:#353D42; background-color:#ffffff00; font-size:456%;  padding-left:2vw"Background</h1>   
  #   <div class="row">
  #   <div class="column left">
  #   <img src="data:image/svg+xml;charset=utf-8;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB2ZXJzaW9uPSIxLjEiIHdpZHRoPSIxMDAwIiBoZWlnaHQ9IjEwMDAiIHZpZXdCb3g9IjAgMCAxMDAwIDEwMDAiIHhtbDpzcGFjZT0icHJlc2VydmUiPg0KPGRlc2M+Q3JlYXRlZCB3aXRoIEZhYnJpYy5qcyAzLjUuMDwvZGVzYz4NCjxkZWZzPg0KPC9kZWZzPg0KPHJlY3QgeD0iMCIgeT0iMCIgd2lkdGg9IjEwMCUiIGhlaWdodD0iMTAwJSIgZmlsbD0iI2ZmZmZmZiIvPg0KPGcgdHJhbnNmb3JtPSJtYXRyaXgoLTI3LjQ3MDYgMCAwIC0yNy40NzA2IDY2OS44NTI4IDc1Mi4yNjQ2KSIgaWQ9Ijc3MzA4Ij4NCjxwYXRoIHN0eWxlPSJzdHJva2U6IG5vbmU7IHN0cm9rZS13aWR0aDogMTsgc3Ryb2tlLWRhc2hhcnJheTogbm9uZTsgc3Ryb2tlLWxpbmVjYXA6IGJ1dHQ7IHN0cm9rZS1kYXNob2Zmc2V0OiAwOyBzdHJva2UtbGluZWpvaW46IG1pdGVyOyBzdHJva2UtbWl0ZXJsaW1pdDogNDsgaXMtY3VzdG9tLWZvbnQ6IG5vbmU7IGZvbnQtZmlsZS11cmw6IG5vbmU7IGZpbGw6IHJnYigwLDAsMCk7IGZpbGwtcnVsZTogbm9uemVybzsgb3BhY2l0eTogMTsiIHZlY3Rvci1lZmZlY3Q9Im5vbi1zY2FsaW5nLXN0cm9rZSIgdHJhbnNmb3JtPSIgdHJhbnNsYXRlKC0xMiwgLTEyKSIgZD0iTSAxOS4wOTIgNi44NjMgYyAtMS41MDQgMi4zMSAtMS43NzkgNC40NSAtMS42ODEgNS42ODggYyA2LjEzMiAtMC4xMDEgNS42OTYgNi40NDkgMS4zOSA2LjQ0OSBjIC0xLjgzIDAgLTMuODAxIC0xLjMzOCAtMy44MDEgLTQuMjc1IGMgMCAtMi43MjQgMS40MTIgLTUuODQ1IDQuMDkyIC03Ljg2MiB6IG0gLTEzIDAgYyAtMS41MDQgMi4zMSAtMS43NzkgNC40NSAtMS42ODEgNS42ODggYyA2LjEzMiAtMC4xMDEgNS42OTYgNi40NDkgMS4zOSA2LjQ0OSBjIC0xLjgzIDAgLTMuODAxIC0xLjMzOCAtMy44MDEgLTQuMjc1IGMgMCAtMi43MjQgMS40MTIgLTUuODQ1IDQuMDkyIC03Ljg2MiB6IG0gMTYuOTA4IC0zLjg2MyBjIC02LjEwOCAxLjIwNiAtMTAgNi41ODQgLTEwIDExLjcyNSBjIDAgMy45NyAyLjc4NiA2LjI3NSA1LjgwMSA2LjI3NSBjIDIuNjE1IDAgNS4xOTkgLTEuNzk3IDUuMTk5IC00Ljk3OSBjIDAgLTIuNjAxIC0xLjkwNSAtNC43NTcgLTQuMzk2IC01LjE0OSBjIDAuMjE3IC0yLjAwNCAyLjE2NSAtNC45MTEgNC4zOCAtNS43NDYgbCAtMC45ODQgLTIuMTI2IHogbSAtMTMgMCBjIC02LjEwOCAxLjIwNiAtMTAgNi41ODQgLTEwIDExLjcyNSBjIDAgMy45NyAyLjc4NiA2LjI3NSA1LjgwMSA2LjI3NSBjIDIuNjE1IDAgNS4xOTkgLTEuNzk3IDUuMTk5IC00Ljk3OSBjIDAgLTIuNjAxIC0xLjkwNSAtNC43NTcgLTQuMzk2IC01LjE0OSBjIDAuMjE3IC0yLjAwNCAyLjE2NSAtNC45MTEgNC4zOCAtNS43NDYgbCAtMC45ODQgLTIuMTI2IHoiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPg0KPC9nPg0KPGcgdHJhbnNmb3JtPSJtYXRyaXgoMjcuNDM2NyAwIDAgMjcuNDM2NyAzMjkuNzQwNCAyNDcuNDMwMykiIGlkPSIyMzQ3NzIiPg0KPHBhdGggc3R5bGU9InN0cm9rZTogbm9uZTsgc3Ryb2tlLXdpZHRoOiAxOyBzdHJva2UtZGFzaGFycmF5OiBub25lOyBzdHJva2UtbGluZWNhcDogYnV0dDsgc3Ryb2tlLWRhc2hvZmZzZXQ6IDA7IHN0cm9rZS1saW5lam9pbjogbWl0ZXI7IHN0cm9rZS1taXRlcmxpbWl0OiA0OyBpcy1jdXN0b20tZm9udDogbm9uZTsgZm9udC1maWxlLXVybDogbm9uZTsgZmlsbDogcmdiKDAsMCwwKTsgZmlsbC1ydWxlOiBub256ZXJvOyBvcGFjaXR5OiAxOyIgdmVjdG9yLWVmZmVjdD0ibm9uLXNjYWxpbmctc3Ryb2tlIiB0cmFuc2Zvcm09IiB0cmFuc2xhdGUoLTEyLCAtMTIpIiBkPSJNIDE5LjA5MiA2Ljg2MyBjIC0xLjUwNCAyLjMxIC0xLjc3OSA0LjQ1IC0xLjY4MSA1LjY4OCBjIDYuMTMyIC0wLjEwMSA1LjY5NiA2LjQ0OSAxLjM5IDYuNDQ5IGMgLTEuODMgMCAtMy44MDEgLTEuMzM4IC0zLjgwMSAtNC4yNzUgYyAwIC0yLjcyNCAxLjQxMiAtNS44NDUgNC4wOTIgLTcuODYyIHogbSAtMTMgMCBjIC0xLjUwNCAyLjMxIC0xLjc3OSA0LjQ1IC0xLjY4MSA1LjY4OCBjIDYuMTMyIC0wLjEwMSA1LjY5NiA2LjQ0OSAxLjM5IDYuNDQ5IGMgLTEuODMgMCAtMy44MDEgLTEuMzM4IC0zLjgwMSAtNC4yNzUgYyAwIC0yLjcyNCAxLjQxMiAtNS44NDUgNC4wOTIgLTcuODYyIHogbSAxNi45MDggLTMuODYzIGMgLTYuMTA4IDEuMjA2IC0xMCA2LjU4NCAtMTAgMTEuNzI1IGMgMCAzLjk3IDIuNzg2IDYuMjc1IDUuODAxIDYuMjc1IGMgMi42MTUgMCA1LjE5OSAtMS43OTcgNS4xOTkgLTQuOTc5IGMgMCAtMi42MDEgLTEuOTA1IC00Ljc1NyAtNC4zOTYgLTUuMTQ5IGMgMC4yMTcgLTIuMDA0IDIuMTY1IC00LjkxMSA0LjM4IC01Ljc0NiBsIC0wLjk4NCAtMi4xMjYgeiBtIC0xMyAwIGMgLTYuMTA4IDEuMjA2IC0xMCA2LjU4NCAtMTAgMTEuNzI1IGMgMCAzLjk3IDIuNzg2IDYuMjc1IDUuODAxIDYuMjc1IGMgMi42MTUgMCA1LjE5OSAtMS43OTcgNS4xOTkgLTQuOTc5IGMgMCAtMi42MDEgLTEuOTA1IC00Ljc1NyAtNC4zOTYgLTUuMTQ5IGMgMC4yMTcgLTIuMDA0IDIuMTY1IC00LjkxMSA0LjM4IC01Ljc0NiBsIC0wLjk4NCAtMi4xMjYgeiIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIi8+DQo8L2c+DQo8L3N2Zz4=" alt="Quote" style="width:15vw;height:10vh; margin-top: 5vh;">
  #   </div>
  #   <div class="column right">
  #   <div id="background-info-tabs"></div>
  #   </div>
  #   </div>
  #   </div>
  #   <div style="height:50vh; background-color: #353D42; margin-left:-1.2vw; margin-right:-1.2vw">
  #   <div style="height: -2.5vh;"> </div>
  #   <div align='right' style="color:#ffffff; background-color:#ffffff00; font-size:456%;  padding-right:2vw; position:relative; top:-5vh;">Contents</div>
  #   <div style="height: 2.5vh;"> </div>
  #   <div class='flexrow-container' style="display: flex;height: 33.8vh;padding: 1em;color: white;outline: none; align-items: flex-end;">
  #   <div class="flexrow-box">
  #   <a href='#headlines_ui' style="text-decoration: none;">
  #   <div class="icon-box bounce" style="background:#353D42; margin-top: 4vh;">
  #   <i class="fa-solid fa-chart-simple fa-2x"></i>
  #   </div>
  #   <p style="color:#353D42; text-align: center; height:10vh; line-height:10vh; margin-top: 2vh;">Headlines</p>
  #   </a>
  #   </div>
  #   <div class="flexrow-box">
  #   <a href='#arts_ui' style="text-decoration: none;">
  #   <div class="icon-box bounce" style="background:#6da7de; margin-top: 4vh;">
  #   <i class="fa-solid fa-masks-theater fa-2x"></i>
  #   </div>
  #   <p style="color:#353D42; text-align: center; height:10vh; line-height:10vh; margin-top: 2vh;">Arts</p>
  #   </a>
  #   </div>
  #   <div class="flexrow-box">
  #   <a href='#libraries_ui' style="text-decoration: none;">
  #   <div class="icon-box bounce" style="background:#ff38ba; margin-top: 4vh;">
  #   <i class="fa-solid fa-book-open fa-2x"></i>
  #   </div>
  #   <p style="color:#353D42; text-align: center; height:10vh; line-height:10vh; margin-top: 2vh;">Libraries</p>
  #   </a>
  #   </div>
  #   <div class="flexrow-box">
  #   <a href='#heritage_ui' style="text-decoration: none;">
  #   <div class="icon-box bounce" style="background:#5ea15d; margin-top: 4vh;">
  #   <i class="fa-solid fa-building-columns fa-2x"></i>
  #   </div>
  #   <p style="color:#353D42; text-align: center; height:10vh; line-height:10vh; margin-top: 2vh;">Heritage</p>
  #   </a>     
  #   </div>
  #   <div class="flexrow-box">
  #   <a href='#sport_ui' style="text-decoration: none;">
  #   <div class="icon-box bounce" style="background:#d82222; margin-top: 4vh;">
  #   <i class="fa-solid fa-person-biking fa-2x"></i>
  #   </div>
  #   <p style="color:#353D42; text-align: center; height:10vh; line-height:10vh; margin-top: 2vh;">Sport</p>
  #   </a>
  #   </div>
  #   <div class="flexrow-box">
  #   <a href='#tourism_ui' style="text-decoration: none;">
  #   <div class="icon-box bounce" style="background:#eb861e; margin-top: 4vh;">
  #   <i class="fa-solid fa-mountain-sun fa-2x"></i>
  #   </div>
  #   <p style="color:#353D42; text-align: center; height:10vh; line-height:10vh; margin-top: 2vh;">Tourism</p>
  #   </a>
  #   </div>
  #   </div>
  #   </div>
  #   </div>
  #   
  #   
  # 
  
  
  div(id='contents_ui',
      includeHTML('contents.html')
  ),
  div(style='height:6vh;'),
  
  
  #=============================================================================
  # Headlines UI
  #=============================================================================
  div(id='headlines_ui',
      fluidRow(
        column(7,
               div(class='tab-panel-ui',
                   # shinyjs::hidden(
                   #   pickerInput(
                   #     inputId="arts_select",
                   #     label="Question",
                   #     choices = ARTS_QUESTIONS,
                   #     selected=ARTS_QUESTIONS[1],
                   #     multiple=F
                   #   )
                   # ),
                   bslib::navset_bar(
                     id='headlines_tab',
                     nav_panel(
                       title='Chart view', 
                       value='chart',
                       div(style='height:15vh; ',#background-color: #e10000;
                           div(class='chart-title-buffer',
                               div(HTML('Headline rank scores for London by DCMS sector'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                           ),
                           div(class='chart-subtitle-buffer',
                               div(HTML("Headline rank scores represent London's average (mean) ranking across selected survey questions relative to the other 8 regions in England; the number of questions, <span style='font-style:normal !important'>q</span>, used to construct this average is shown below the axis label of each data point"), style='font-size:2.4vh; line-height:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250; font-style: italic !important ')
                           )
                           #question<br>relative
                           #average<br>is shown
                           #,
                           # shinyWidgets::prettyCheckboxGroup(
                           #   inputId='headlines_compOps',
                           #   label='',
                           #   choices=c(
                           #     'England mean'='mean',
                           #     'England error (95% CI)'='error'
                           #   ),
                           #   selected=c('mean'),
                           #   inline=T
                           # )
                       ),
                       div(class='chart-buffer'),
                       shinycssloaders::withSpinner(
                         highchartOutput('headlines_chart', height='73vh'),
                         color = "#353D42"
                       )
                     ),
                     # nav_panel(
                     #   title='Map view',
                     #   value='map',
                     #   div(style='height:16vh;',
                     #       div(style='height: .5vh'),
                     #       div(class='chart-title-buffer',
                     #           div(textOutput('arts_title_map'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                     #       ),
                     #       div(class='chart-subtitle-buffer',
                     #           div(textOutput('arts_subtitle_map'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                     #       )
                     #   ),
                     #   shinycssloaders::withSpinner(
                     #     highchartOutput('arts_map', height='72vh'),
                     #     color = "#6da7ded9"
                     #   )
                     # ),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_item(
                       #shinyjs::hidden(uiOutput("arts_previous_btn")) ,
                       # bsTooltip(
                       #   id = "arts_previous_btn",
                       #   title = 'View previous "Arts" question',
                       #   placement = "bottom",
                       #   trigger = "hover"
                       # )
                     ),
                     nav_item(
                       #uiOutput("arts_next_btn"),
                       # bsTooltip(
                       #   id = "arts_next_btn",
                       #   title = 'View next "Arts" question',
                       #   placement = "bottom",
                       #   trigger = "hover"
                       # )
                     ),
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
                     ),
                     fluidRow(
                       column(3),
                       column(5)#,
                     )
                   )
               )
        ), 
        column(5,
               fluidRow(
                 h2('Headlines', style='color:#353D42 !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
               ),
               fluidRow(
                 div(
                   style = "background-color: #353D42 !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%;",
                   div(class='text-ui',
                       div(style='height:2vh'),
                       HTML(
                         '
                           <span style="color:#ffffff; font-size:2.2vw;  line-height:2.2vw;">
                             Of the 9 regions in England,<br>
                           </span>
                           <span style="color:#ffffff; font-size:2.8vw;  line-height:2.8vw; ">
                             London ranks:
                           </span>
                           <span style="color:#ffffff; font-size:1.4vw;  line-height:1.6vw; margin-top:-2vh">
                             <div style="height:2vh"></div>
                             <ul>
                               <li>3<sup>rd</sup> for participation in <span style="color:#6da7de; font-weight:bold">Arts</span></li>
                               <li>1<sup>st</sup> for participation in <span style="color:#ff38ba; font-weight:bold">Libraries</span></li>
                               <li>2<sup>nd</sup> for participation in <span style="color:#5ea15d; font-weight:bold">Heritage</span></li>
                               <li>8<sup>th</sup> for participation in <span style="color:#d82222; font-weight:bold">Sport</span></li>
                               <li>9<sup>th</sup> for participation in <span style="color:#eb861e; font-weight:bold">Tourism</span></li>
                             </ul>
                           </span>
                         '
                       )
                       #,
                       # add key high scores on specific questions
                       
                       
                      #div(id='countUp-ui-arts'),
                       #generate_countUp(reactiveVal__countFromTo$countTo, reactiveVal__countFromTo$countFrom, 'arts'),
                       #htmlOutput("arts_region_headline_text"),
                       #shinycssloaders::withSpinner(htmlOutput("arts_region_summary_text"),color='#ffffff', size=.5, proxy.height='25vh'),
                       #shinycssloaders::withSpinner(,color = "#ffffff"),
                       #uiOutput("mouseOver_ui"),
                       # shinyjs::hidden(
                       #   div(id='arts-text-drilldown',
                       #       icon('arrow-down-long'),
                       #       div(style='height:1.4vh'),
                       #       htmlOutput("arts_borough_text")
                       #   )
                       # )
                   )
                 )
               ),
               fluidRow(
                 div(style='height:2.5vh;'),
                 div(class='section_nav_panel',style='margin-left:1vw; margin-right:0vw;',
                   bslib::navset_bar(
                     nav_item(
                       #div(class='box headlines',
                       HTML(
                         '
                         <div>
                           <a href="#headlines_ui" style="text-decoration: none;">
                             <div class="box headlines active">
                               <i class="fa-solid fa-chart-simple fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                       )
                     ),
                     nav_spacer(),
                     nav_item(
                       HTML(
                         '
                         <div>
                           <a href="#arts_ui" style="text-decoration: none;">
                             <div class="box arts">
                               <i class="fa-solid fa-masks-theater fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '

                       )
                     ),
                     nav_spacer(),
                     nav_item(
                       HTML(
                         '
                         <div>
                           <a href="#libraries_ui" style="text-decoration: none;">
                             <div class="box libraries">
                               <i class="fa-solid fa-book-open fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                       )
                     ),
                     nav_spacer(),
                     nav_item(
                       HTML(
                         '
                         <div>
                           <a href="#heritage_ui" style="text-decoration: none;">
                             <div class="box heritage">
                               <i class="fa-solid fa-building-columns fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                       )
                     ),
                     nav_spacer(),
                     nav_item(
                       HTML(
                         '
                         <div>
                           <a href="#sport_ui" style="text-decoration: none;">
                             <div class="box sport">
                               <i class="fa-solid fa-person-biking fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                       )
                     ),
                     nav_spacer(),
                     nav_item(
                       HTML(
                         '
                         <div>
                           <a href="#tourism_ui" style="text-decoration: none;">
                             <div class="box tourism">
                               <i class="fa-solid fa-mountain-sun fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                       )
                     )#,
                   )
                 )
               )
        )
      )
  ),
  div(style='height:6vh;'),
  
  # Of the 9 regions in England, London ranks:
  #  1st for participation in Arts
  #  2nd for participation in Libraries
  #  8th for participation in Sport, and
  #  9th for participation in Tourism
  #
  #  Participation in London was especially high for the following questions
  #
  #
  
  
  # div(id='headlines_ui',
  #   #div(style='height:5vh;'),
  #   fluidRow(
  #     column(7,
  #       div(style='height:15vh; ',#background-color: #e10000;
  #         div(class='chart-title-buffer',
  #           div(HTML('A chart title'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
  #         ),
  #         div(class='chart-subtitle-buffer',
  #           div(HTML('A chart subtitle'), style='font-size:2.4vh; line-height:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
  #         ),
  #       ),
  #       div(class='chart-buffer'),
  #       shinycssloaders::withSpinner(
  #         highchartOutput('headlines_chart', height='73vh'),
  #         color = "#353D42"
  #       )
  #     ),
  #     column(5,
  #       fluidRow(
  #         h2('Headlines', style='color:#353D42; !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
  #       ),
  #       fluidRow(
  #         div(
  #           style = "background-color:#353D42; !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
  #           div(class='text-ui',
  #             div(style='height:2vh')
  #           )
  #         )
  #       )
  #     )
  #   )
  # ),
  # div(style='height:6vh;'),
  #       
      
  
  
  #=============================================================================
  # Arts UI
  #=============================================================================
  div(id='arts_ui',
  fluidRow(
    column(7,
      div(class='tab-panel-ui',
        shinyjs::hidden(
          pickerInput(
            inputId="arts_select",
            label="Question",
            choices = ARTS_QUESTIONS,
            selected=ARTS_QUESTIONS[1],
            multiple=F
          )
        ),
        bslib::navset_bar(
          id='arts_tab',
          nav_panel(
            title='Chart view', 
            value='chart',
            div(style='height:15vh; ',#background-color: #e10000;
              div(class='chart-title-buffer',
                div(textOutput('arts_title_chart'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
              ),
              div(class='chart-subtitle-buffer',
                div(textOutput('arts_subtitle_chart'), style='font-size:2.4vh; line-height:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
              ),
              shinyWidgets::prettyCheckboxGroup(
                inputId='arts_compOps',
                label='',
                choices=c(
                  'England mean'='mean',
                  'England error (95% CI)'='error'
                ),
                selected=c('mean'),
                inline=T
              )
            ),
            div(class='chart-buffer'),
            shinycssloaders::withSpinner(
              highchartOutput('arts_chart', height='73vh'),
              color = "#6da7ded9"
            )
          ),
          nav_panel(
            title='Map view',
            value='map',
            div(style='height:16vh;',
              div(style='height: .5vh'),
              div(class='chart-title-buffer',
                div(textOutput('arts_title_map'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
              ),
              div(class='chart-subtitle-buffer',
                div(textOutput('arts_subtitle_map'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
              )
            ),
            shinycssloaders::withSpinner(
              highchartOutput('arts_map', height='72vh'),
              color = "#6da7ded9"
            )
          ),
          nav_spacer(),
          nav_spacer(),
          nav_spacer(),
          nav_spacer(),
          nav_spacer(),
          nav_spacer(),
          nav_item(
            shinyjs::hidden(uiOutput("arts_previous_btn")) ,
            bsTooltip(
              id = "arts_previous_btn",
              title = 'View previous "Arts" question',
              placement = "bottom",
              trigger = "hover"
            )
          ),
          nav_item(
            uiOutput("arts_next_btn"),
            bsTooltip(
              id = "arts_next_btn",
              title = 'View next "Arts" question',
              placement = "bottom",
              trigger = "hover"
            )
          ),
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
          ),
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
            div(style='height:2vh'),
            div(id='countUp-ui-arts'),
            #generate_countUp(reactiveVal__countFromTo$countTo, reactiveVal__countFromTo$countFrom, 'arts'),
            htmlOutput("arts_region_headline_text"),
            shinycssloaders::withSpinner(htmlOutput("arts_region_summary_text"),color='#ffffff', size=.5, proxy.height='25vh'),
            #shinycssloaders::withSpinner(,color = "#ffffff"),
            #uiOutput("mouseOver_ui"),
            shinyjs::hidden(
              div(id='arts-text-drilldown',
                icon('arrow-down-long'),
                div(style='height:1.4vh'),
                htmlOutput("arts_borough_text")
              )
            )
          )
        )
      ),
      fluidRow(
        div(style='height:2.5vh;'),
        div(class='section_nav_panel',style='margin-left:1vw; margin-right:0vw;',
            bslib::navset_bar(
              nav_item(
                #div(class='box headlines',
                HTML(
                  '
                         <div>
                           <a href="#headlines_ui" style="text-decoration: none;">
                             <div class="box headlines">
                               <i class="fa-solid fa-chart-simple fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                )
              ),
              nav_spacer(),
              nav_item(
                HTML(
                  '
                         <div>
                           <a href="#arts_ui" style="text-decoration: none;">
                             <div class="box arts active">
                               <i class="fa-solid fa-masks-theater fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                  
                )
              ),
              nav_spacer(),
              nav_item(
                HTML(
                  '
                         <div>
                           <a href="#libraries_ui" style="text-decoration: none;">
                             <div class="box libraries">
                               <i class="fa-solid fa-book-open fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                )
              ),
              nav_spacer(),
              nav_item(
                HTML(
                  '
                         <div>
                           <a href="#heritage_ui" style="text-decoration: none;">
                             <div class="box heritage">
                               <i class="fa-solid fa-building-columns fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                )
              ),
              nav_spacer(),
              nav_item(
                HTML(
                  '
                         <div>
                           <a href="#sport_ui" style="text-decoration: none;">
                             <div class="box sport">
                               <i class="fa-solid fa-person-biking fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                )
              ),
              nav_spacer(),
              nav_item(
                HTML(
                  '
                         <div>
                           <a href="#tourism_ui" style="text-decoration: none;">
                             <div class="box tourism">
                               <i class="fa-solid fa-mountain-sun fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                )
              )#,
            )
        )
      )
    )
  )
  ),
  div(style='height:6vh;'),
  
  #=============================================================================
  # Libraries UI
  #=============================================================================
  div(id='libraries_ui',
      fluidRow(
        column(7,
               div(class='tab-panel-ui',
                   shinyjs::hidden(
                     pickerInput(
                       inputId="libraries_select",
                       label="Question",
                       choices = LIBRARIES_QUESTIONS,
                       selected = LIBRARIES_QUESTIONS[1],
                       multiple=F
                     )
                   ),
                   bslib::navset_bar(
                     id='libraries_tab',
                     nav_panel(
                       title='Chart view',
                       value='chart',
                       div(style='height:15vh',
                           div(style='height: .5vh'),
                           div(class='chart-title-buffer',
                               div(textOutput('libraries_title_chart'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                           ),
                           div(class='chart-subtitle-buffer',
                               div(textOutput('libraries_subtitle_chart'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                           ),
                           shinyWidgets::prettyCheckboxGroup(
                             inputId='libraries_compOps',
                             label='',
                             choices=c(
                               'England mean'='mean',
                               'England error (95% CI)'='error'
                             ),
                             selected=c('mean'),
                             inline=T
                           )
                       ),
                       shinycssloaders::withSpinner(
                         highchartOutput('libraries_chart', height='72vh'),
                         color = "#ff38ba"
                       )
                     ),
                     nav_panel(
                       title='Map view',
                       value='map',
                       div(style='height:16vh',
                           div(style='height: .5vh'),
                           div(class='chart-title-buffer',
                               div(textOutput('libraries_title_map'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                           ),
                           div(class='chart-subtitle-buffer',
                               div(textOutput('libraries_subtitle_map'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                           )
                       ),
                       shinycssloaders::withSpinner(
                         highchartOutput('libraries_map', height='72vh'),
                         color = "#ff38ba"
                       )
                     ),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_item(
                       shinyjs::hidden(uiOutput("libraries_previous_btn")),
                       bsTooltip(
                         id = "libraries_previous_btn",
                         title = 'View previous "Libraries" question',
                         placement = "bottom",
                         trigger = "hover"
                       )
                     ),
                     nav_item(
                       uiOutput("libraries_next_btn"),
                       bsTooltip(
                         id = "libraries_next_btn",
                         title = 'View next "Libraries" question',
                         placement = "bottom",
                         trigger = "hover"
                       )
                     ),
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
                     ),
                     fluidRow(
                       column(3),
                       column(5)#,
                     )
                   )
               )
        ),
        column(5,
               fluidRow(
                 h2('Libraries', style='color:#ff38ba !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
               ),
               fluidRow(
                 div(
                   style = "background-color:#ff38ba !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
                   div(class='text-ui',
                       div(style='height:2vh'),
                       div(id='countUp-ui-libraries'),
                       #generate_countUp(reactiveVal__countFromTo$countTo, reactiveVal__countFromTo$countFrom, 'arts'),
                       htmlOutput("libraries_region_headline_text"),
                       shinycssloaders::withSpinner(htmlOutput("libraries_region_summary_text"),color = "#ffffff"),
                       shinyjs::hidden(
                         div(id='libraries-text-drilldown',
                             icon('arrow-down-long'),
                             div(style='height:1.4vh'),
                             htmlOutput("libraries_borough_text")
                         )
                       )
                   )
                 )
               ),
               fluidRow(
                 div(style='height:2.5vh;'),
                 div(class='section_nav_panel',style='margin-left:1vw; margin-right:0vw;',
                     bslib::navset_bar(
                       nav_item(
                         #div(class='box headlines',
                         HTML(
                           '
                         <div>
                           <a href="#headlines_ui" style="text-decoration: none;">
                             <div class="box headlines">
                               <i class="fa-solid fa-chart-simple fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#arts_ui" style="text-decoration: none;">
                             <div class="box arts">
                               <i class="fa-solid fa-masks-theater fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                         
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#libraries_ui" style="text-decoration: none;">
                             <div class="box libraries active">
                               <i class="fa-solid fa-book-open fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#heritage_ui" style="text-decoration: none;">
                             <div class="box heritage">
                               <i class="fa-solid fa-building-columns fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#sport_ui" style="text-decoration: none;">
                             <div class="box sport">
                               <i class="fa-solid fa-person-biking fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#tourism_ui" style="text-decoration: none;">
                             <div class="box tourism">
                               <i class="fa-solid fa-mountain-sun fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       )#,
                     )
                 )
               )
        )
      )
  ),
  div(style='height:6vh;'),
  
  

  #=============================================================================
  # Heritage UI
  #=============================================================================
  div(id='heritage_ui',
  fluidRow(
    column(7,
           div(class='tab-panel-ui',
               shinyjs::hidden(
                 pickerInput(
                   inputId="heritage_select",
                   label="Question",
                   choices = HERITAGE_QUESTIONS,
                   selected = HERITAGE_QUESTIONS[1],
                   multiple=F
                 )
               ),
               bslib::navset_bar(
                 id='heritage_tab',
                 nav_panel(
                   title='Chart view',
                   value='chart',
                   div(style='height:15vh',
                       div(style='height: .5vh'),
                       div(class='chart-title-buffer',
                           div(textOutput('heritage_title_chart'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                       ),
                       div(class='chart-subtitle-buffer',
                           div(textOutput('heritage_subtitle_chart'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                       ),
                       shinyWidgets::prettyCheckboxGroup(
                         inputId='heritage_compOps',
                         label='',
                         choices=c(
                           'England mean'='mean',
                           'England error (95% CI)'='error'
                         ),
                         selected=c('mean'),
                         inline=T
                       )
                   ),
                   shinycssloaders::withSpinner(
                     highchartOutput('heritage_chart', height='72vh'),
                     color = "#5ea15d"
                   )
                 ),
                 nav_panel(
                   title='Map view',
                   value='map',
                   div(style='height:16vh',
                       div(style='height: .5vh'),
                       div(class='chart-title-buffer',
                           div(textOutput('heritage_title_map'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                       ),
                       div(class='chart-subtitle-buffer',
                           div(textOutput('heritage_subtitle_map'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                       )
                   ),
                   shinycssloaders::withSpinner(
                     highchartOutput('heritage_map', height='72vh'),
                     color = "#5ea15d"
                   )
                 ),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_item(
                   shinyjs::hidden(uiOutput("heritage_previous_btn")),
                   bsTooltip(
                     id = "heritage_previous_btn",
                     title = 'View previous "Heritage" question',
                     placement = "bottom",
                     trigger = "hover"
                   )
                   
                 ),
                 nav_item(
                   uiOutput("heritage_next_btn"),
                   bsTooltip(
                     id = "heritage_next_btn",
                     title = 'View next "Heritage" question',
                     placement = "bottom",
                     trigger = "hover"
                   )
                 ),
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
                 ),
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
               style = "background-color:#5ea15d !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
               div(class='text-ui',
                   div(style='height:2vh'),
                   div(id='countUp-ui-heritage'),
                   #generate_countUp(reactiveVal__countFromTo$countTo, reactiveVal__countFromTo$countFrom, 'arts'),
                   htmlOutput("heritage_region_headline_text"),
                   shinycssloaders::withSpinner(htmlOutput("heritage_region_summary_text"),color = "#ffffff"),
                   shinyjs::hidden(
                     div(id='heritage-text-drilldown',
                         icon('arrow-down-long'),
                         div(style='height:1.4vh'),
                         htmlOutput("heritage_borough_text")
                     )
                   )
               )
             )
           ),
           fluidRow(
             div(style='height:2.5vh;'),
             div(class='section_nav_panel',style='margin-left:1vw; margin-right:0vw;',
                 bslib::navset_bar(
                   nav_item(
                     #div(class='box headlines',
                     HTML(
                       '
                         <div>
                           <a href="#headlines_ui" style="text-decoration: none;">
                             <div class="box headlines">
                               <i class="fa-solid fa-chart-simple fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#arts_ui" style="text-decoration: none;">
                             <div class="box arts">
                               <i class="fa-solid fa-masks-theater fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                       
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#libraries_ui" style="text-decoration: none;">
                             <div class="box libraries">
                               <i class="fa-solid fa-book-open fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#heritage_ui" style="text-decoration: none;">
                             <div class="box heritage active">
                               <i class="fa-solid fa-building-columns fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#sport_ui" style="text-decoration: none;">
                             <div class="box sport">
                               <i class="fa-solid fa-person-biking fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#tourism_ui" style="text-decoration: none;">
                             <div class="box tourism">
                               <i class="fa-solid fa-mountain-sun fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   )#,
                 )
             )
           )
    )
  )
  ),
  div(style='height:6vh;'), # 6vh originally
  #=============================================================================
  # Sport UI
  #=============================================================================
  div(id='sport_ui',
  fluidRow(
    column(7,
           div(class='tab-panel-ui',
               shinyjs::hidden(
                 pickerInput(
                   inputId="sport_select",
                   label="Question",
                   choices = SPORT_QUESTIONS,
                   selected = SPORT_QUESTIONS[1],
                   multiple=F
                 )
               ),
               bslib::navset_bar(
                 id='sport_tab',
                 nav_panel(
                   title='Chart view',
                   value='chart',
                   div(style='height:15vh',
                       div(style='height: .5vh'),
                       div(class='chart-title-buffer',
                           div(textOutput('sport_title_chart'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                       ),
                       div(class='chart-subtitle-buffer',
                           div(textOutput('sport_subtitle_chart'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                       ),
                       shinyWidgets::prettyCheckboxGroup(
                         inputId='sport_compOps',
                         label='',
                         choices=c(
                           'England mean'='mean',
                           'England error (95% CI)'='error'
                         ),
                         selected=c('mean'),
                         inline=T
                       )
                   ),
                   shinycssloaders::withSpinner(
                     highchartOutput('sport_chart', height='72vh'),
                     color = "#d82222"
                   )
                 ),
                 nav_panel(
                   title='Map view',
                   value='map',
                   div(style='height:16vh',
                       div(style='height: .5vh'),
                       div(class='chart-title-buffer',
                           div(textOutput('sport_title_map'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                       ),
                       div(class='chart-subtitle-buffer',
                           div(textOutput('sport_subtitle_map'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                       )
                   ),
                   shinycssloaders::withSpinner(
                     highchartOutput('sport_map', height='72vh'),
                     color = "#d82222"
                   )
                 ),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_spacer(),
                 nav_item(
                   shinyjs::hidden(uiOutput("sport_previous_btn")),
                   bsTooltip(
                     id = "sport_previous_btn",
                     title = 'View previous "Sport" question',
                     placement = "bottom",
                     trigger = "hover"
                   )
                 ),
                 nav_item(
                   uiOutput("sport_next_btn"),
                   bsTooltip(
                     id = "sport_next_btn",
                     title = 'View next "Sport" question',
                     placement = "bottom",
                     trigger = "hover"
                   )
                 ),
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
                 ),
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
               style = "background-color:#d82222 !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
               div(class='text-ui',
                   div(style='height:2vh'),
                   div(id='countUp-ui-sport'),
                   #generate_countUp(reactiveVal__countFromTo$countTo, reactiveVal__countFromTo$countFrom, 'arts'),
                   htmlOutput("sport_region_headline_text"),
                   shinycssloaders::withSpinner(htmlOutput("sport_region_summary_text"),color = "#ffffff")#,
                   # shinyjs::hidden(
                   #   div(id='sport-text-drilldown',
                   #       icon('arrow-down-long'),
                   #       div(style='height:1.4vh'),
                   #       htmlOutput("sport_borough_text")
                   #   )
                   # )
               )
             )
           ),
           fluidRow(
             div(style='height:2.5vh;'),
             div(class='section_nav_panel',style='margin-left:1vw; margin-right:0vw;',
                 bslib::navset_bar(
                   nav_item(
                     #div(class='box headlines',
                     HTML(
                       '
                         <div>
                           <a href="#headlines_ui" style="text-decoration: none;">
                             <div class="box headlines">
                               <i class="fa-solid fa-chart-simple fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#arts_ui" style="text-decoration: none;">
                             <div class="box arts">
                               <i class="fa-solid fa-masks-theater fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                       
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#libraries_ui" style="text-decoration: none;">
                             <div class="box libraries">
                               <i class="fa-solid fa-book-open fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#heritage_ui" style="text-decoration: none;">
                             <div class="box heritage">
                               <i class="fa-solid fa-building-columns fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#sport_ui" style="text-decoration: none;">
                             <div class="box sport active">
                               <i class="fa-solid fa-person-biking fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   ),
                   nav_spacer(),
                   nav_item(
                     HTML(
                       '
                         <div>
                           <a href="#tourism_ui" style="text-decoration: none;">
                             <div class="box tourism">
                               <i class="fa-solid fa-mountain-sun fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                     )
                   )#,
                 )
             )
           )
    )
  )
  ),
  #=============================================================================
  # Tourism UI
  #=============================================================================
  div(id='tourism_ui',
      fluidRow(
        column(7,
               div(class='tab-panel-ui',
                   shinyjs::hidden(
                     pickerInput(
                       inputId="tourism_select",
                       label="Question",
                       choices = TOURISM_QUESTIONS,
                       selected = TOURISM_QUESTIONS[1],
                       multiple=F
                     )
                   ),
                   bslib::navset_bar(
                     id='tourism_tab',
                     nav_panel(
                       title='Chart view',
                       value='chart',
                       div(style='height:15vh',
                           div(style='height: .5vh'),
                           div(class='chart-title-buffer',
                               div(textOutput('tourism_title_chart'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                           ),
                           div(class='chart-subtitle-buffer',
                               div(textOutput('tourism_subtitle_chart'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                           ),
                           shinyWidgets::prettyCheckboxGroup(
                             inputId='tourism_compOps',
                             label='',
                             choices=c(
                               'England mean'='mean',
                               'England error (95% CI)'='error'
                             ),
                             selected=c('mean'),
                             inline=T
                           )
                       ),
                       shinycssloaders::withSpinner(
                         highchartOutput('tourism_chart', height='72vh'),
                         color = "#d82222"
                       )
                     ),
                     nav_panel(
                       title='Map view',
                       value='map',
                       div(style='height:16vh',
                           div(style='height: .5vh'),
                           div(class='chart-title-buffer',
                               div(textOutput('tourism_title_map'), style='font-size:3.8vh; line-height:3.8vh;  color: #353d42; font-family: Arial !important; font-weight: 450 !important ')
                           ),
                           div(class='chart-subtitle-buffer',
                               div(textOutput('tourism_subtitle_map'), style='font-size:2.4vh; color: #353d42; font-family: Arial !important; font-weight: 250 !important; font-style: italic !important ')
                           )
                       ),
                       shinycssloaders::withSpinner(
                         highchartOutput('tourism_map', height='72vh'),
                         color = "#eb861e"
                       )
                     ),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_spacer(),
                     nav_item(
                       shinyjs::hidden(uiOutput("tourism_previous_btn")),
                       bsTooltip(
                         id = "tourism_previous_btn",
                         title = 'View previous "Tourism" question',
                         placement = "bottom",
                         trigger = "hover"
                       )
                     ),
                     nav_item(
                       shinyjs::hidden(uiOutput("tourism_next_btn")),
                       bsTooltip(
                         id = "tourism_next_btn",
                         title = 'View next "Sport" question',
                         placement = "bottom",
                         trigger = "hover"
                       )
                     ),
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
                     ),
                     fluidRow(
                       column(3),
                       column(5)#,
                     )
                   )
               )
        ),
        column(5,
               fluidRow(
                 h2('Tourism', style='color:#eb861e !important; font-size: 16vh; margin-top:0vh; right: 2vw; position:absolute')
               ),
               fluidRow(
                 div(
                   style = "background-color:#eb861e !important; margin-left:0vw; margin-right:0vw; height:70vh; border-radius:10% 0% 10% 0%",
                   div(class='text-ui',
                       div(style='height:2vh'),
                       div(id='countUp-ui-tourism'),
                       #generate_countUp(reactiveVal__countFromTo$countTo, reactiveVal__countFromTo$countFrom, 'arts'),
                       htmlOutput("tourism_region_headline_text"),
                       shinycssloaders::withSpinner(htmlOutput("tourism_region_summary_text"),color = "#ffffff"),
                       shinyjs::hidden(
                         div(id='tourism-text-drilldown',
                             icon('arrow-down-long'),
                             div(style='height:1.4vh'),
                             htmlOutput("tourism_borough_text")
                         )
                       )
                   )
                 )
               ),
               fluidRow(
                 div(style='height:2.5vh;'),
                 div(class='section_nav_panel',style='margin-left:1vw; margin-right:0vw;',
                     bslib::navset_bar(
                       nav_item(
                         #div(class='box headlines',
                         HTML(
                           '
                         <div>
                           <a href="#headlines_ui" style="text-decoration: none;">
                             <div class="box headlines">
                               <i class="fa-solid fa-chart-simple fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#arts_ui" style="text-decoration: none;">
                             <div class="box arts">
                               <i class="fa-solid fa-masks-theater fa-1x"></i>
                             </div>
                           </a>
                         </div>
                         '
                         
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#libraries_ui" style="text-decoration: none;">
                             <div class="box libraries">
                               <i class="fa-solid fa-book-open fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#heritage_ui" style="text-decoration: none;">
                             <div class="box heritage">
                               <i class="fa-solid fa-building-columns fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#sport_ui" style="text-decoration: none;">
                             <div class="box sport">
                               <i class="fa-solid fa-person-biking fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       ),
                       nav_spacer(),
                       nav_item(
                         HTML(
                           '
                         <div>
                           <a href="#tourism_ui" style="text-decoration: none;">
                             <div class="box tourism active">
                               <i class="fa-solid fa-mountain-sun fa-1x"></i>
                             </div>
                           </a>
                         </div>

                         '
                         )
                       )#,
                     )
                 )
               )
        )
      )
  ),
  div(style='height:10vh'),
  #=============================================================================
  # Header UI
  #=============================================================================
  div(id='footer_ui',
      includeHTML('footer-new.html')
  ),
  
  # Set scroll reveal animation for each section - mwah!
  scroll_reveal(target = c('#header_ui', '#contents_ui', '#headlines_ui', "#arts_ui", "#libraries_ui", "#heritage_ui", "#sport_ui", "#tourism_ui", '#header_ui', '#footer_ui'), duration=4000, distance="0%", delay=200)
)



 
  
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
#    
# 
# 
# <h4 style="font-size:2.4vh"><i>The 2023/24 <i>Participation Survey</i> was commissioned by the Department for 
# Culture, Media and Sport (DCMS) to provide data on "adult participation in DCMS sectors across
#           England". The <i>Participation Survey</i> serves as a successor to the <i>Taking Part Survey</i>, 
# which ran for 16 continuous years, with its final publication in 2019/20. 
# Like its successor, the <i>Participation Survey</i> is designed to "deliver a nationally 
#           representative sample of adults (aged 16 years and over)". However, while the
# <i>Taking Part Survey</i> relied exclusively on data obtained from face-to-face interviews, 
# the <i>Participation Survey</i> is based on a "push to web" data collection model, where "[r]espondents take part either online or by completing a paper questionaire." The most 
# immediate benefit of this alternate approach to data collection is that DCMS is now able to obtain a boosted
# respondent sample, allowing for the collection of reliable data at the Local Authority (LA) level; going forward, DCMS plan to 
# obtain a boosted sample for the <i>Pariticipation Survey</i> every three years, starting with the 2023/24 publication.
# Overall, the key takeaway for the GLA is that the 2023/24 <i>Participation Survey</i> provides a uniquely rich source of information
# on adult participation at the level of both Region and Borough.</i></h4>'
   

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #=============================================================================
  # Background Server
  #=============================================================================
  #text-align: center;
  insertUI(
    selector="#background-info-tabs",
    where='beforeEnd',
    ui=
    tabsetPanel(type='pills',
      tabPanel(
        title='About the survey',
        div(style="height:3vh;"),
        HTML(
          '
          <div class="multicolumn-text">
            <h4 style="font-size:2vh; break-after:column; 
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);">
              The <i>Participation Survey 2023/24</i> was commissioned by the Department for 
              Culture, Media and Sport (DCMS) to provide data on "adult participation in DCMS sectors across
              England". The <i>Participation Survey</i> serves as a successor to the <i>Taking Part Survey</i>,
              which ran for 16 continuous years, with its final publication in 2019/20.
            </h4>
             <h4 style="font-size:2vh; break-after:column;
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);">
               Like its successor, the <i>Participation Survey</i> is designed to "deliver a nationally representative sample of adults (aged 16 years and over)". However, while the
               <i>Taking Part Survey</i> relied exclusively on data obtained from face-to-face interviews,
               the <i>Participation Survey</i> is based on a "push to web" data collection model, where "[r]espondents take part either online or by completing a paper questionaire".
             </h4>
             <h4 style="font-size:2vh; break-after:column;
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);">
               The most immediate benefit of this alternate approach to data collection is that DCMS is now able to obtain a boosted
               respondent sample, allowing for the collection of reliable data at the Local Authority (LA) level; going forward, DCMS plan to 
               obtain a boosted sample for the <i>Pariticipation Survey</i> every three years, starting with the 2023/24 publication.
             </h4>
             <h4 style="font-size:2vh; break-after:column;
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);">
               Overall, the key takeaway for the GLA is that the <i>Participation Survey 2023/24</i> provides a uniquely rich source of information
               on adult participation at the level of both Region and Borough.</i>
             </h4>
          </div>
          '
          # '
          # <table>
          #   <tr>
          #     <td>
          #       The 2023/24 <i>Participation Survey</i> was commissioned by the Department for 
          #       Culture, Media and Sport (DCMS) to provide data on "adult participation in DCMS sectors across
          #       England". The <i>Participation Survey</i> serves as a successor to the <i>Taking Part Survey</i>, 
          #       which ran for 16 continuous years, with its final publication in 2019/20. 
          #     </td>
          #     <td>
          #       The most immediate benefit of this alternate approach to data collection is that DCMS is now able to obtain a boosted
          # respondent sample, allowing for the collection of reliable data at the Local Authority (LA) level; going forward, DCMS plan to 
          # obtain a boosted sample for the <i>Pariticipation Survey</i> every three years, starting with the 2023/24 publication.
          #     </td>
          #   </tr>
          #   <tr>
          #     <td>
          #       Like its successor, the <i>Participation Survey</i> is designed to "deliver a nationally representative sample of adults (aged 16 years and over)". However, while the
          #       <i>Taking Part Survey</i> relied exclusively on data obtained from face-to-face interviews, 
          #       the <i>Participation Survey</i> is based on a "push to web" data collection model, where "[r]espondents take part either online or by completing a paper questionaire."
          #     </td>
          #     <td>
          #       Overall, the key takeaway for the GLA is that the 2023/24 <i>Participation Survey</i> provides a uniquely rich source of information
          #       on adult participation at the level of both Region and Borough.</i>
          #     </td>
          #   </tr>
          # </table>
          # '
        )
      ),
      tabPanel(
        title='About this tool',
        div(style="height:3vh;"),
        HTML(
          "
          <div class='multicolumn-text'>
            <h4 style='font-size:2vh; break-after:column; 
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);'>
  This tool provides a London-focused overview of the main findings from the <i>Participation Survey 2023/24</i> using the annual data tables, which can be accessed <a href='https://www.gov.uk/government/statistics/participation-survey-2023-24-annual-publication' target='_blank'>here</a>. The tool presents findings from across five DCMS sectors, namely: Arts, Libraries, Heritage, Sport and Tourism (Heritage contains the measures on Museums and Galleries).
            </h4>
             <h4 style='font-size:2vh; break-after:column;
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);'>
     The tool contains a dedicated page for each of these five sectors, which can be accessed sequentially by scrolling downward or alternatively you can jump to a specific page using the Contents panel below. All five sector pages also contain two 'views', which you can switch between via the navigation bar located at the top left of each page.
             </h4>
             <h4 style='font-size:2vh; break-after:column;
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);'>
     The default 'Chart view' presents findings using a simple bar chart. The alternate 'Map view' presents the same information using a choropleth map. You can also download an image of the chart or map in multiple formats, as well as export the underlying data to CSV. 
             </h4>
             <h4 style='font-size:2vh; break-after:column;
  position: relative;
  top: 50%;
  -ms-transform: translateY(-58%);
  -webkit-transform: translateY(-58%);
  transform: translateY(-58%);'>
    Finally, each sector page (except for Tourism) presents findings from multiple survey questions. You can switch between questions using the 'Next' and 'Previous' buttons.
             </h4>
          </div>
          "
        )
      )
    )
  )
  
  #=============================================================================
  # Headlines Server
  #=============================================================================
      
  output$headlines_chart <- renderHighchart({
    generate_headline_chart(df_headline, shinybrowser::get_height())
  })

  
  
  
  
  
  
  
                 
  
  
  
  
  
  

  #=============================================================================
  # Arts Server
  #=============================================================================
  
  output$arts_previous_btn <- renderUI({
    # tooltip(
    actionButton(
      "arts_previous", 
      "Previous question",
      icon=icon("backward")
    )
    #,
    #'A fucking message'
    #)
  })
  output$arts_next_btn <- renderUI({
    actionButton(
      "arts_next", 
      "Next question",
      icon=icon("forward")
    )
  })
  
  
  output$arts_title_chart <- renderText({
    print(paste(df_list[[as.numeric(input$arts_select)]][['region']][['title']]))
  })
  output$arts_title_map <- renderText({
    print(paste(df_list[[as.numeric(input$arts_select)]][['region']][['title']]))
  })
  outputOptions(output, "arts_title_map", suspendWhenHidden = FALSE)
  
  
  output$arts_subtitle_chart <- renderText({
    if (is.null(input$arts_currLevel)) {
      print(paste("Click to drilldown into London by Borough"))
    }
    else if (input$arts_currLevel==0) {
      print(paste("Click 'Back to Regions' to drillup" ))
    }
    else {
      print(paste("Click to drilldown into London by Borough"))
    }
  })
  output$arts_subtitle_map <- renderText({
    if (is.null(input$arts_currLevelMap)) {
      print(paste("Click to drilldown into London by Borough"))
    }
    else if (input$arts_currLevelMap==0) {
      print(paste("Click 'Back to Regions' to drillup" ))
    }
    else {
      print(paste("Click to drilldown into London by Borough"))
    }
  })
  outputOptions(output, "arts_subtitle_map", suspendWhenHidden = FALSE)
  
  
  
  output$arts_chart <- renderHighchart({
    generate_drilldown_chart(ARTS_QUESTIONS[1], df_list, 'arts', shinybrowser::get_height())
  })
  output$arts_map <- renderHighchart({
    generate_drilldown_map(ARTS_QUESTIONS[1], df_list, 'arts', QUESTION_LIST, bounds_region, bounds_borough)
  })
  outputOptions(output, "arts_map", suspendWhenHidden = FALSE)
  
  
  reactive__arts_select <- reactive({req(input$arts_select)})
  reactive__mouseOver_arts <- reactiveValues()
  observeEvent(c(input$arts_tab, input$arts_chart_mouseOver$name,input$arts_currLevel, input$arts_map_mouseOver,input$arts_currLevelMap, input$arts_select), {
    observeEvent(c(input$arts_tab, input$arts_select, input$arts_currLevel, input$arts_currLevelMap), {
      reg_name <- 'London'
      reg_val <-
        df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        select(region, prop_resp) %>%
        filter(region=='London') %>%
        select(prop_resp) %>%
        pull(1)
      eng_val <-
        df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
        # mutate(mean = round(mean(prop_resp),1)) %>%
        # filter(region=='London') %>%
        select(prop_resp) %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        pull(1)
      reg_eng_dif <- round(reg_val - eng_val ,1)
      reg_rank <-
        df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
        select(region, prop_resp, rank_direction) %>%
        mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
        filter(region=='London') %>%
        select(rank) %>%
        pull(1)
      
    }, ignoreInit=T, ignoreNULL=T)
    if (input$arts_tab=='chart') {
      if(is.null(input$arts_chart_mouseOver$name)) {
        #browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
        #browser()
      }
      # On mouseOver
      else {
        
        if (is.null(input$arts_currLevel)) {
          reg_name <- input$arts_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$arts_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$arts_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          #browser()
        }
        else if (input$arts_currLevel==1) {
          reg_name <- input$arts_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$arts_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
              select(prop_resp) %>%
              mutate(prop_resp=round(prop_resp,1)) %>%
              pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$arts_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          # TODO TRY HERE!!!!!!!!
          #browser()
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
              select(prop_resp) %>%
              mutate(prop_resp=round(prop_resp,1)) %>%
              pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    if (input$arts_tab=='map') {
      if(is.null(input$arts_map_mouseOver)) {
        # browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
      }
      # On mouseOver
      else {
        if (is.null(input$arts_currLevelMap)) {
          #browser()
          reg_name <- input$arts_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$arts_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
              select(prop_resp) %>%
              mutate(prop_resp=round(prop_resp,1)) %>%
              pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$arts_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else if (input$arts_currLevelMap==1) {
          #browser()
          reg_name <- input$arts_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$arts_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
              select(prop_resp) %>%
              mutate(prop_resp=round(prop_resp,1)) %>%
              pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$arts_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__arts_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    reactive__mouseOver_arts$reg_name <- reg_name
    reactive__mouseOver_arts$reg_val <- reg_val
    reactive__mouseOver_arts$eng_val <- eng_val
    reactive__mouseOver_arts$reg_eng_dif <- reg_eng_dif
    reactive__mouseOver_arts$reg_rank <- reg_rank
    # browser()
    # print('yo')
  }, ignoreNULL=F, ignoreInit=F
  )
  
  
  output$arts_region_headline_text <- renderUI({
    generate_region_headline_text(input$arts_select, df_list)
  })
  output$arts_region_summary_text <- renderUI({
    generate_region_summary_text(input$arts_select, df_list, input$arts_currLevel, reactive__mouseOver_arts$reg_name,  reactive__mouseOver_arts$reg_val, reactive__mouseOver_arts$eng_val, reactive__mouseOver_arts$reg_eng_dif, reactive__mouseOver_arts$reg_rank)
  })
  
  output$arts_borough_text <- renderUI({
    generate_borough_text(input$arts_select, df_list)
  })
  
  
  
  
  
  observeEvent(input$arts_select, {
    if (input$arts_select!=ARTS_QUESTIONS[1]) {
      shinyjs::show('arts_previous_btn')
    }
    if (input$arts_select==ARTS_QUESTIONS[1]) {
      shinyjs::hide('arts_previous_btn')
    }
    if (input$arts_select!=ARTS_QUESTIONS[length(ARTS_QUESTIONS)]) {
      shinyjs::show('arts_next_btn')
    }
    if (input$arts_select==ARTS_QUESTIONS[length(ARTS_QUESTIONS)])  { 
      shinyjs::hide('arts_next_btn')
    }
    
    
  }, ignoreInit=T)
  
  
  
  observeEvent(input$arts_previous, {
    current <- which(ARTS_QUESTIONS == input$arts_select)
    if(current > 1) {
      updateSelectInput(
        session, "arts_select",
        selected = ARTS_QUESTIONS[current - 1]
      )
      #update_drilldown_chart(input$arts_select, df_list, "arts_chart") 
      #update_drilldown_map(input$arts_select, df_list, "arts_map")
      # if (!is.null(input$arts_currLevel)) {
      # if (input$arts_currLevel==0) {
      #   #browser()
      # # output$arts_chart <- NULL
      # # output$arts_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$arts_select, df_list, 'arts', shinybrowser::get_height())
      # # })
      #   # highchartProxy('arts_chart') %>%
      #   # hcpxy_redraw()
      # }
      # }
      
      
      #shinyjs::html(id = 'arts_region_headline_text', html=generate_region_headline_text(input$arts_select, df_list))
      #shinyjs::html(id = 'arts_region_summary_text', html=generate_region_summary_text(input$arts_select, df_list, input$arts_currLevel, reactive__mouseOver_arts$reg_name,  reactive__mouseOver_arts$reg_val, reactive__mouseOver_arts$reg_eng_dif, reactive__mouseOver_arts$reg_rank))
      # TODO update instead of repetitive regeneration
      
      
      #shinyjs::html(id = 'arts_borough_text', html=generate_borough_text(input$arts_select, df_list))
      #shinyjs::html(id = 'arts_region_text', html =  generate_region_text(input$arts_select, df_list))
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
      #input$arts_currLevel <- 1
      #update_drilldown_chart(input$arts_select, df_list, "arts_chart") 
      #update_drilldown_map(input$arts_select, df_list, "arts_map")
      #browser()
      # if (!is.null(input$arts_currLevel)) {
      # if (input$arts_currLevel==0) {
      # #browser()
      # # output$arts_chart <- NULL
      # # output$arts_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$arts_select, df_list, 'arts', shinybrowser::get_height())
      # # })
      #   highchartProxy('arts_chart') %>%
      #   hcpxy_redraw()
      # }#
      # }
      
      
      #shinyjs::html(id = 'arts_region_headline_text', html=generate_region_headline_text(input$arts_select, df_list))
      #shinyjs::html(id = 'arts_region_summary_text', html=generate_region_summary_text(input$arts_select, df_list, input$arts_currLevel, reactive__mouseOver_arts$reg_name,  reactive__mouseOver_arts$reg_val, reactive__mouseOver_arts$reg_eng_dif, reactive__mouseOver_arts$reg_rank))
      
      
      #shinyjs::html(id = 'arts_borough_text', html=generate_borough_text(input$arts_select, df_list))
      
    }
  }, ignoreInit=T, ignoreNULL=T, priority=4
  )
  
  # 
  # observeEvent(c(input$arts_currLevel,input$arts_currLevelMap), {
  #   if (is.null(input$arts_currLevel)) {
  #     updateTextOutput(session, 'arts_subtitle_text')
  #   }
  #    if (is.null(input$arts_currLevelMap)) {
  #      updateTextOutput(session, 'arts_subtitle_text')
  #    }
  #   
  #   input$arts_currLevel
  #   
  # })
  
  observeEvent(input$arts_currLevel, {
    print('level change')
  })
  
  
  # Need to separate out
  
  
  
  
  observeEvent(input$arts_select, {
    if (!is.null(input$arts_currLevel)) { 
      if (input$arts_currLevel==0) {
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#arts_chart').highcharts();
            chart.drillUp();
            "
          )
        )
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#arts_map').highcharts();
            chart.drillUp();
            "
          )
        )
      }
    }
    if (!is.null(input$arts_currLevelMap)) { 
      if (input$arts_currLevelMap==0) {
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#arts_map').highcharts();
            chart.drillUp();
            "
          )
        )
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#arts_chart').highcharts();
            chart.drillUp();
            "
          )
        )
      }
    }
  }, priority=8)  
  

  
  
  
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  observeEvent(c(input$arts_select, input$arts_currLevel), { #input$arts_compOps, input$arts_currLevel,
    #browser()
    if (is.null(input$arts_currLevel)==T) {
      update_drilldown_chart(input$arts_select, df_list, "arts_chart", input$arts_compOps)
      update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps)
      update_drilldown_map(input$arts_select, df_list, "arts_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    #browser()
    else if (input$arts_currLevel==1) {
      update_drilldown_chart(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
      #browser()
      update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
      update_drilldown_map(input$arts_select, df_list, "arts_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    # else if (input$arts_currLevel==0) {
    #   #browser()
    #   
    # }
    else {
      update_drilldown_chart(input$arts_select, df_list, "arts_chart", input$arts_compOps)
      update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
    }
    # observeEvent(input$arts_currLevel, {
    #   #browser()
    #   update_drilldown_chart(input$arts_select, df_list, "arts_chart", input$arts_compOps, 1)
    #   update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps, 1)
    #   update_drilldown_map(input$arts_select, df_list, "arts_map", QUESTION_LIST, bounds_region, bounds_borough)
    # }, ignoreInit=T, ignoreNULL=T)
  }, ignoreInit=T, ignoreNULL=F, priority=0)
  
  
  observeEvent(c(input$arts_compOps), { #, input$arts_currLevel
    update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
  }, ignoreInit=F, ignoreNULL=F, priority=3)
  
  


  
  
  
  
  # observeEvent(c(input$arts_select), { #input$arts_compOps, input$arts_currLevel,
  #   update_drilldown_chart(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
  #   # update_drilldown_chart(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
  #   #update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
  #   update_drilldown_map(input$arts_select, df_list, "arts_map", QUESTION_LIST, bounds_region, bounds_borough)
  # }, ignoreInit=T, ignoreNULL=F, priority=0)
  # 
  # 
  # observeEvent(c(input$arts_currLevel, input$arts_compOps, input$arts_select), { #input$arts_compOps, input$arts_currLevel,
  #   update_drilldown_chart_summary(input$arts_select, df_list, "arts_chart", input$arts_compOps, input$arts_currLevel)
  # }, ignoreInit=T, ignoreNULL=F, priority=1)
  
  
  
  
  
  observeEvent(
    c(input$arts_currLevel), {
      
      #browser()
      req(input$arts_currLevel)
      selected <- input$arts_compOps
      if (input$arts_currLevel==0) {
        updatePrettyCheckboxGroup(
          session, "arts_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      else {
        updatePrettyCheckboxGroup(
          session, "arts_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      
      observeEvent(c(input$arts_select), {
        updatePrettyCheckboxGroup(
          session, "arts_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  
  observe({
    print(input$arts_tab)
  })
  
  observe({
    print(paste0('my level =', input$arts_currLevelMap))
  })
  
  
  observeEvent(c(input$arts_currLevel, input$arts_currLevelMap, input$arts_tab), {
    
    req(input$arts_tab) # still don't really understand req() but is required 
    #browser()
    if (input$arts_tab=='chart') {
      req(input$arts_currLevel)
      #browser()
      if (input$arts_currLevel==0) {
        shinyjs::show('arts-text-drilldown')
      }
      else  {
        shinyjs::hide('arts-text-drilldown')
      }
      observeEvent(c(input$arts_select, input$arts_tab), {
        shinyjs::hide('arts-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    else {
      #browser()
      #delay(5000,
      req(input$arts_currLevelMap)
      if (input$arts_currLevelMap==0) {
        shinyjs::show('arts-text-drilldown')
      }
      else  {
        shinyjs::hide('arts-text-drilldown')
      }
      observeEvent(c(input$arts_select, input$arts_tab), {
        shinyjs::hide('arts-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  reactive__countFromToArts <- reactive({
    question <- as.numeric(input$arts_select)
    df_region <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    countTo <- df_region$prop_resp[df_region$region=='London']
  })
  
  reactiveVal__countFromToArts <-  reactiveValues(countFrom=0, countTo=0)
  
  observeEvent( 
    reactive__countFromToArts(),{
      reactiveVal__countFromToArts$countFrom <- reactiveVal__countFromToArts$countTo; 
      #print(reactiveVal__countFromTo$countFrom)
      reactiveVal__countFromToArts$countTo <- reactive__countFromToArts()
      #print(reactiveVal__countFromTo$countTo)
    }
  )
  
  reactive__countToArts <- reactive({req(reactive__countFromToArts());  reactive__countFromToArts(); reactiveVal__countFromToArts$countTo})
  reactive__countFromArts <- reactive({req(reactive__countFromToArts()); reactive__countFromToArts(); reactiveVal__countFromToArts$countFrom})
  
  
  output$countTo <- renderPrint({reactive__countTo()})
  output$countFrom <- renderPrint({ reactive__countFrom()})
  
  
  observeEvent(
    input$arts_select,once=T, ignoreNULL=F, ignoreInit=F, {
      insertUI(
        selector = "#countUp-ui-arts",
        where = "afterEnd",
        ui = div(generate_countUp(reactiveVal__countFromToArts$countTo, reactiveVal__countFromToArts$countFrom, 'arts'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
      )
    }
  )
  
  observeEvent(reactiveVal__countFromToArts$countTo, {
    countupProxy("countUp-arts") %>% 
      countup_update(reactiveVal__countFromToArts$countTo)
  }, priority=8)
  
  #=============================================================================
  # Libraries Server
  #=============================================================================
  
  output$libraries_previous_btn <- renderUI({
    # tooltip(
    actionButton(
      "libraries_previous", 
      "Previous question",
      icon=icon("backward")
    )
    #,
    #'A fucking message'
    #)
  })
  output$libraries_next_btn <- renderUI({
    actionButton(
      "libraries_next", 
      "Next question",
      icon=icon("forward")
    )
  })
  
  
  output$libraries_title_chart <- renderText({
    print(paste(df_list[[as.numeric(input$libraries_select)]][['region']][['title']]))
  })
  output$libraries_title_map <- renderText({
    print(paste(df_list[[as.numeric(input$libraries_select)]][['region']][['title']]))
  })
  outputOptions(output, "libraries_title_map", suspendWhenHidden = FALSE)
  
  output$libraries_subtitle_chart <- renderText({
    if (is.null(input$libraries_currLevel)) {
      print(paste("Click to drilldown into London by Borough"))
    }
    else if (input$libraries_currLevel==0) {
      print(paste("Click 'Back to Regions' to drillup" ))
    }
    else {
      print(paste("Click to drilldown into London by Borough"))
    }
  })
  output$libraries_subtitle_map <- renderText({
    if (is.null(input$libraries_currLevelMap)) {
      print(paste("Click to drilldown into London by Borough"))
    }
    else if (input$libraries_currLevelMap==0) {
      print(paste("Click 'Back to Regions' to drillup" ))
    }
    else {
      print(paste("Click to drilldown into London by Borough"))
    }
  })
  outputOptions(output, "libraries_subtitle_map", suspendWhenHidden = FALSE)
  
  
  
  output$libraries_chart <- renderHighchart({
    generate_drilldown_chart(LIBRARIES_QUESTIONS[1], df_list, 'libraries', shinybrowser::get_height())
  })
  output$libraries_map <- renderHighchart({
    generate_drilldown_map(LIBRARIES_QUESTIONS[1], df_list, 'libraries', QUESTION_LIST, bounds_region, bounds_borough)
  })
  outputOptions(output, "libraries_map", suspendWhenHidden = FALSE)
  
  
  reactive__libraries_select <- reactive({req(input$libraries_select)})
  reactive__mouseOver_libraries <- reactiveValues()
  observeEvent(c(input$libraries_tab, input$libraries_chart_mouseOver$name,input$libraries_currLevel, input$libraries_map_mouseOver,input$libraries_currLevelMap, input$libraries_select), {
    observeEvent(c(input$libraries_tab, input$libraries_select, input$libraries_currLevel, input$libraries_currLevelMap), {
      reg_name <- 'London'
      reg_val <-
        df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        select(region, prop_resp) %>%
        filter(region=='London') %>%
        select(prop_resp) %>%
        pull(1)
      eng_val <-
        df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
        select(prop_resp) %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        pull(1)
      reg_eng_dif <- round(reg_val - eng_val ,1)
      reg_rank <-
        df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
        select(region, prop_resp, rank_direction) %>%
        mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
        filter(region=='London') %>%
        select(rank) %>%
        pull(1)

    }, ignoreInit=T, ignoreNULL=T)
    if (input$libraries_tab=='chart') {
      if(is.null(input$libraries_chart_mouseOver$name)) {
        #browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
        #browser()
      }
      # On mouseOver
      else {

        if (is.null(input$libraries_currLevel)) {
          reg_name <- input$libraries_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$libraries_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$libraries_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          #browser()
        }
        else if (input$libraries_currLevel==1) {
          reg_name <- input$libraries_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$libraries_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$libraries_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          # TODO TRY HERE!!!!!!!!
          #browser()
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    if (input$libraries_tab=='map') {
      if(is.null(input$libraries_map_mouseOver)) {
        # browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
      }
      # On mouseOver
      else {
        if (is.null(input$libraries_currLevelMap)) {
          #browser()
          reg_name <- input$libraries_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$libraries_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$libraries_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else if (input$libraries_currLevelMap==1) {
          #browser()
          reg_name <- input$libraries_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$libraries_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$libraries_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__libraries_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    reactive__mouseOver_libraries$reg_name <- reg_name
    reactive__mouseOver_libraries$reg_val <- reg_val
    reactive__mouseOver_libraries$eng_val <- eng_val
    reactive__mouseOver_libraries$reg_eng_dif <- reg_eng_dif
    reactive__mouseOver_libraries$reg_rank <- reg_rank
    # browser()
    # print('yo')
  }, ignoreNULL=F, ignoreInit=F
  )
  
  
  output$libraries_region_headline_text <- renderUI({
    generate_region_headline_text(input$libraries_select, df_list)
  })
  output$libraries_region_summary_text <- renderUI({
    generate_region_summary_text(input$libraries_select, df_list, input$libraries_currLevel, reactive__mouseOver_libraries$reg_name,  reactive__mouseOver_libraries$reg_val, reactive__mouseOver_libraries$eng_val, reactive__mouseOver_libraries$reg_eng_dif, reactive__mouseOver_libraries$reg_rank)
  })
  
  output$libraries_borough_text <- renderUI({
    generate_borough_text(input$libraries_select, df_list)
  })
  
  
  
  
  
  observeEvent(input$libraries_select, {
    if (input$libraries_select!=LIBRARIES_QUESTIONS[1]) {
      shinyjs::show('libraries_previous_btn')
    }
    if (input$libraries_select==LIBRARIES_QUESTIONS[1]) {
      shinyjs::hide('libraries_previous_btn')
    }
    if (input$libraries_select!=LIBRARIES_QUESTIONS[length(LIBRARIES_QUESTIONS)]) {
      shinyjs::show('libraries_next_btn')
    }
    if (input$libraries_select==LIBRARIES_QUESTIONS[length(LIBRARIES_QUESTIONS)])  { 
      shinyjs::hide('libraries_next_btn')
    }
    
    
  }, ignoreInit=T)
  
  
  
  observeEvent(input$libraries_previous, {
    current <- which(LIBRARIES_QUESTIONS == input$libraries_select)
    if(current > 1) {
      updateSelectInput(
        session, "libraries_select",
        selected = LIBRARIES_QUESTIONS[current - 1]
      )
      #update_drilldown_chart(input$libraries_select, df_list, "libraries_chart") 
      #update_drilldown_map(input$libraries_select, df_list, "libraries_map")
      # if (!is.null(input$libraries_currLevel)) {
      # if (input$libraries_currLevel==0) {
      #   #browser()
      # # output$libraries_chart <- NULL
      # # output$libraries_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$libraries_select, df_list, 'libraries', shinybrowser::get_height())
      # # })
      #   # highchartProxy('libraries_chart') %>%
      #   # hcpxy_redraw()
      # }
      
      # }
      
      #shinyjs::html(id = 'libraries_region_headline_text', html=generate_region_headline_text(input$libraries_select, df_list))
      #shinyjs::html(id = 'libraries_region_summary_text', html=generate_region_summary_text(input$libraries_select, df_list, input$libraries_currLevel, reactive__mouseOver_libraries$reg_name,  reactive__mouseOver_libraries$reg_val, reactive__mouseOver_libraries$reg_eng_dif, reactive__mouseOver_libraries$reg_rank))
      
      
      #shinyjs::html(id = 'libraries_borough_text', html=generate_borough_text(input$libraries_select, df_list))
      #shinyjs::html(id = 'libraries_region_text', html =  generate_region_text(input$libraries_select, df_list))
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
      #input$libraries_currLevel <- 1
      #update_drilldown_chart(input$libraries_select, df_list, "libraries_chart") 
      #update_drilldown_map(input$libraries_select, df_list, "libraries_map")
      #browser()
      # if (!is.null(input$libraries_currLevel)) {
      # if (input$libraries_currLevel==0) {
      # #browser()
      # # output$libraries_chart <- NULL
      # # output$libraries_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$libraries_select, df_list, 'libraries', shinybrowser::get_height())
      # # })
      #   highchartProxy('libraries_chart') %>%
      #   hcpxy_redraw()
      # }#
      # }
      
      
      #shinyjs::html(id = 'libraries_region_headline_text', html=generate_region_headline_text(input$libraries_select, df_list))
      #shinyjs::html(id = 'libraries_region_summary_text', html=generate_region_summary_text(input$libraries_select, df_list, input$libraries_currLevel, reactive__mouseOver_libraries$reg_name,  reactive__mouseOver_libraries$reg_val, reactive__mouseOver_libraries$reg_eng_dif, reactive__mouseOver_libraries$reg_rank))
      
      
      #shinyjs::html(id = 'libraries_borough_text', html=generate_borough_text(input$libraries_select, df_list))
      
    }
  }, ignoreInit=T, ignoreNULL=T, priority=4
  )
  
  # 
  # observeEvent(c(input$libraries_currLevel,input$libraries_currLevelMap), {
  #   if (is.null(input$libraries_currLevel)) {
  #     updateTextOutput(session, 'libraries_subtitle_text')
  #   }
  #    if (is.null(input$libraries_currLevelMap)) {
  #      updateTextOutput(session, 'libraries_subtitle_text')
  #    }
  #   
  #   input$libraries_currLevel
  #   
  # })
  
  observeEvent(input$libraries_currLevel, {
    print('level change')
  })
  
  # Need to separate out
  
  
  
  observeEvent(input$libraries_select, {
    if (!is.null(input$libraries_currLevel)) { 
      if (input$libraries_currLevel==0) {
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#libraries_chart').highcharts();
            chart.drillUp();
            "
          )
        )
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#libraries_map').highcharts();
            chart.drillUp();
            "
          )
        )
      }
    }
    if (!is.null(input$libraries_currLevelMap)) { 
      if (input$libraries_currLevelMap==0) {
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#libraries_map').highcharts();
            chart.drillUp();
            "
          )
        )
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#libraries_chart').highcharts();
            chart.drillUp();
            "
          )
        )
      }
    }
  }, priority=8)  
  
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  observeEvent(c(input$libraries_select, input$libraries_currLevel), { #input$libraries_compOps, input$libraries_currLevel,
    #browser()
    if (is.null(input$libraries_currLevel)==T) {
      update_drilldown_chart(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps)
      update_drilldown_chart_summary(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps)
      update_drilldown_map(input$libraries_select, df_list, "libraries_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    #browser()
    else if (input$libraries_currLevel==1) {
      update_drilldown_chart(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps, input$libraries_currLevel)
      #browser()
      update_drilldown_chart_summary(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps, input$libraries_currLevel)
      update_drilldown_map(input$libraries_select, df_list, "libraries_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    # else if (input$libraries_currLevel==0) {
    #   #browser()
    #   
    # }
    else {
      update_drilldown_chart(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps)
      update_drilldown_chart_summary(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps, input$libraries_currLevel)
    }
    # observeEvent(input$libraries_currLevel, {
    #   #browser()
    #   update_drilldown_chart(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps, 1)
    #   update_drilldown_chart_summary(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps, 1)
    #   update_drilldown_map(input$libraries_select, df_list, "libraries_map", QUESTION_LIST, bounds_region, bounds_borough)
    # }, ignoreInit=T, ignoreNULL=T)
  }, ignoreInit=T, ignoreNULL=F, priority=0)
  
  observeEvent(c(input$libraries_compOps), { #, input$libraries_currLevel
    update_drilldown_chart_summary(input$libraries_select, df_list, "libraries_chart", input$libraries_compOps, input$libraries_currLevel)
  }, ignoreInit=F, ignoreNULL=F, priority=3)
  
  
  
  
  
  observeEvent(
    c(input$libraries_currLevel), {
      
      #browser()
      req(input$libraries_currLevel)
      selected <- input$libraries_compOps
      if (input$libraries_currLevel==0) {
        updatePrettyCheckboxGroup(
          session, "libraries_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      else {
        updatePrettyCheckboxGroup(
          session, "libraries_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      
      observeEvent(c(input$libraries_select), {
        updatePrettyCheckboxGroup(
          session, "libraries_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  
  observe({
    print(input$libraries_tab)
  })
  
  observe({
    print(paste0('my level =', input$libraries_currLevelMap))
  })
  
  
  observeEvent(c(input$libraries_currLevel, input$libraries_currLevelMap, input$libraries_tab), {
    
    req(input$libraries_tab) # still don't really understand req() but is required 
    #browser()
    if (input$libraries_tab=='chart') {
      req(input$libraries_currLevel)
      #browser()
      if (input$libraries_currLevel==0) {
        shinyjs::show('libraries-text-drilldown')
      }
      else  {
        shinyjs::hide('libraries-text-drilldown')
      }
      observeEvent(c(input$libraries_select, input$libraries_tab), {
        shinyjs::hide('libraries-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    else {
      #browser()
      #delay(5000,
      req(input$libraries_currLevelMap)
      if (input$libraries_currLevelMap==0) {
        shinyjs::show('libraries-text-drilldown')
      }
      else  {
        shinyjs::hide('libraries-text-drilldown')
      }
      observeEvent(c(input$libraries_select, input$libraries_tab), {
        shinyjs::hide('libraries-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  reactive__countFromToLibraries <- reactive({
    question <- as.numeric(input$libraries_select)
    df_region <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    countTo <- df_region$prop_resp[df_region$region=='London']
  })
  
  reactiveVal__countFromToLibraries <-  reactiveValues(countFrom=0, countTo=0)
  
  observeEvent( 
    reactive__countFromToLibraries(),{
      reactiveVal__countFromToLibraries$countFrom <- reactiveVal__countFromToLibraries$countTo; 
      #print(reactiveVal__countFromTo$countFrom)
      reactiveVal__countFromToLibraries$countTo <- reactive__countFromToLibraries()
      #print(reactiveVal__countFromTo$countTo)
    }
  )
  
  reactive__countToLibraries <- reactive({req(reactive__countFromToLibraries());  reactive__countFromToLibraries(); reactiveVal__countFromToLibraries$countTo})
  reactive__countFromLibraries <- reactive({req(reactive__countFromToLibraries()); reactive__countFromToLibraries(); reactiveVal__countFromToLibraries$countFrom})
  
  
  output$countTo <- renderPrint({reactive__countTo()})
  output$countFrom <- renderPrint({ reactive__countFrom()})
  
  
  observeEvent(
    input$libraries_select,once=T, ignoreNULL=F, ignoreInit=F, {
      insertUI(
        selector = "#countUp-ui-libraries",
        where = "afterEnd",
        ui = div(generate_countUp(reactiveVal__countFromToLibraries$countTo, reactiveVal__countFromToLibraries$countFrom, 'libraries'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
      )
    }
  )
  
  observeEvent(reactiveVal__countFromToLibraries$countTo, {
    countupProxy("countUp-libraries") %>% 
      countup_update(reactiveVal__countFromToLibraries$countTo)
  }, priority=8)
  
  #=============================================================================
  # Heritage Server
  #=============================================================================
  
  output$heritage_previous_btn <- renderUI({
    # tooltip(
    actionButton(
      "heritage_previous", 
      "Previous question",
      icon=icon("backward")
    )
    #,
    #'A fucking message'
    #)
  })
  output$heritage_next_btn <- renderUI({
    actionButton(
      "heritage_next", 
      "Next question",
      icon=icon("forward")
    )
  })
  
  
  output$heritage_title_chart <- renderText({
    print(paste(df_list[[as.numeric(input$heritage_select)]][['region']][['title']]))
  })
  output$heritage_title_map <- renderText({
    print(paste(df_list[[as.numeric(input$heritage_select)]][['region']][['title']]))
  })
  outputOptions(output, "heritage_title_map", suspendWhenHidden = FALSE)
  
  output$heritage_subtitle_chart <- renderText({
    if (is.null(input$heritage_currLevel)) {
      print(paste("Click to drilldown into London by Borough"))
    }
    else if (input$heritage_currLevel==0) {
      print(paste("Click 'Back to Regions' to drillup" ))
    }
    else {
      print(paste("Click to drilldown into London by Borough"))
    }
  })
  output$heritage_subtitle_map <- renderText({
    if (is.null(input$heritage_currLevelMap)) {
      print(paste("Click to drilldown into London by Borough"))
    }
    else if (input$heritage_currLevelMap==0) {
      print(paste("Click 'Back to Regions' to drillup" ))
    }
    else {
      print(paste("Click to drilldown into London by Borough"))
    }
  })
  outputOptions(output, "heritage_subtitle_map", suspendWhenHidden = FALSE)
  
  
  
  output$heritage_chart <- renderHighchart({
    generate_drilldown_chart(HERITAGE_QUESTIONS[1], df_list, 'heritage', shinybrowser::get_height())
  })
  output$heritage_map <- renderHighchart({
    generate_drilldown_map(HERITAGE_QUESTIONS[1], df_list, 'heritage', QUESTION_LIST, bounds_region, bounds_borough)
  })
  outputOptions(output, "heritage_map", suspendWhenHidden = FALSE)
  
  
  reactive__heritage_select <- reactive({req(input$heritage_select)})
  reactive__mouseOver_heritage <- reactiveValues()
  observeEvent(c(input$heritage_tab, input$heritage_chart_mouseOver$name,input$heritage_currLevel, input$heritage_map_mouseOver,input$heritage_currLevelMap, input$heritage_select), {
    observeEvent(c(input$heritage_tab, input$heritage_select, input$heritage_currLevel, input$heritage_currLevelMap), {
      reg_name <- 'London'
      reg_val <-
        df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        select(region, prop_resp) %>%
        filter(region=='London') %>%
        select(prop_resp) %>%
        pull(1)
      eng_val <-
        df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
        select(prop_resp) %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        pull(1)
      reg_eng_dif <- round(reg_val - eng_val ,1)
      reg_rank <-
        df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
        select(region, prop_resp, rank_direction) %>%
        mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
        filter(region=='London') %>%
        select(rank) %>%
        pull(1)
      
    }, ignoreInit=T, ignoreNULL=T)
    if (input$heritage_tab=='chart') {
      if(is.null(input$heritage_chart_mouseOver$name)) {
        #browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
        #browser()
      }
      # On mouseOver
      else {
        
        if (is.null(input$heritage_currLevel)) {
          reg_name <- input$heritage_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$heritage_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$heritage_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          #browser()
        }
        else if (input$heritage_currLevel==1) {
          reg_name <- input$heritage_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$heritage_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$heritage_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          # TODO TRY HERE!!!!!!!!
          #browser()
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    if (input$heritage_tab=='map') {
      if(is.null(input$heritage_map_mouseOver)) {
        # browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
      }
      # On mouseOver
      else {
        if (is.null(input$heritage_currLevelMap)) {
          #browser()
          reg_name <- input$heritage_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$heritage_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$heritage_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else if (input$heritage_currLevelMap==1) {
          #browser()
          reg_name <- input$heritage_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$heritage_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$heritage_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    reactive__mouseOver_heritage$reg_name <- reg_name
    reactive__mouseOver_heritage$reg_val <- reg_val
    reactive__mouseOver_heritage$eng_val <- eng_val
    reactive__mouseOver_heritage$reg_eng_dif <- reg_eng_dif
    reactive__mouseOver_heritage$reg_rank <- reg_rank
    # browser()
    # print('yo')
  }, ignoreNULL=F, ignoreInit=F
  )
  
  
  output$heritage_region_headline_text <- renderUI({
    generate_region_headline_text(input$heritage_select, df_list)
  })
  output$heritage_region_summary_text <- renderUI({
    generate_region_summary_text(input$heritage_select, df_list, input$heritage_currLevel, reactive__mouseOver_heritage$reg_name,  reactive__mouseOver_heritage$reg_val, reactive__mouseOver_heritage$eng_val, reactive__mouseOver_heritage$reg_eng_dif, reactive__mouseOver_heritage$reg_rank)
  })
  
  output$heritage_borough_text <- renderUI({
    generate_borough_text(input$heritage_select, df_list)
  })
  
  
  
  
  
  observeEvent(input$heritage_select, {
    if (input$heritage_select!=HERITAGE_QUESTIONS[1]) {
      shinyjs::show('heritage_previous_btn')
    }
    if (input$heritage_select==HERITAGE_QUESTIONS[1]) {
      shinyjs::hide('heritage_previous_btn')
    }
    if (input$heritage_select!=HERITAGE_QUESTIONS[length(HERITAGE_QUESTIONS)]) {
      shinyjs::show('heritage_next_btn')
    }
    if (input$heritage_select==HERITAGE_QUESTIONS[length(HERITAGE_QUESTIONS)])  { 
      shinyjs::hide('heritage_next_btn')
    }
    
    
  }, ignoreInit=T)
  
  
  
  observeEvent(input$heritage_previous, {
    current <- which(HERITAGE_QUESTIONS == input$heritage_select)
    if(current > 1) {
      updateSelectInput(
        session, "heritage_select",
        selected = HERITAGE_QUESTIONS[current - 1]
      )
      #update_drilldown_chart(input$heritage_select, df_list, "heritage_chart") 
      #update_drilldown_map(input$heritage_select, df_list, "heritage_map")
      # if (!is.null(input$heritage_currLevel)) {
      # if (input$heritage_currLevel==0) {
      #   #browser()
      # # output$heritage_chart <- NULL
      # # output$heritage_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$heritage_select, df_list, 'heritage', shinybrowser::get_height())
      # # })
      #   # highchartProxy('heritage_chart') %>%
      #   # hcpxy_redraw()
      # }
      # }
      
      
      #shinyjs::html(id = 'heritage_region_headline_text', html=generate_region_headline_text(input$heritage_select, df_list))
      #shinyjs::html(id = 'heritage_region_summary_text', html=generate_region_summary_text(input$heritage_select, df_list, input$heritage_currLevel, reactive__mouseOver_heritage$reg_name,  reactive__mouseOver_heritage$reg_val, reactive__mouseOver_heritage$reg_eng_dif, reactive__mouseOver_heritage$reg_rank))
      
      
      #shinyjs::html(id = 'heritage_borough_text', html=generate_borough_text(input$heritage_select, df_list))
      #shinyjs::html(id = 'heritage_region_text', html =  generate_region_text(input$heritage_select, df_list))
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
      #input$heritage_currLevel <- 1
      #update_drilldown_chart(input$heritage_select, df_list, "heritage_chart") 
      #update_drilldown_map(input$heritage_select, df_list, "heritage_map")
      #browser()
      # if (!is.null(input$heritage_currLevel)) {
      # if (input$heritage_currLevel==0) {
      # #browser()
      # # output$heritage_chart <- NULL
      # # output$heritage_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$heritage_select, df_list, 'heritage', shinybrowser::get_height())
      # # })
      #   highchartProxy('heritage_chart') %>%
      #   hcpxy_redraw()
      # }#
      # }
      
      
      #shinyjs::html(id = 'heritage_region_headline_text', html=generate_region_headline_text(input$heritage_select, df_list))
      #shinyjs::html(id = 'heritage_region_summary_text', html=generate_region_summary_text(input$heritage_select, df_list, input$heritage_currLevel, reactive__mouseOver_heritage$reg_name,  reactive__mouseOver_heritage$reg_val, reactive__mouseOver_heritage$reg_eng_dif, reactive__mouseOver_heritage$reg_rank))
      
      
      
      #shinyjs::html(id = 'heritage_borough_text', html=generate_borough_text(input$heritage_select, df_list))
      
    }
  }, ignoreInit=T, ignoreNULL=T, priority=4
  )
  
  # 
  # observeEvent(c(input$heritage_currLevel,input$heritage_currLevelMap), {
  #   if (is.null(input$heritage_currLevel)) {
  #     updateTextOutput(session, 'heritage_subtitle_text')
  #   }
  #    if (is.null(input$heritage_currLevelMap)) {
  #      updateTextOutput(session, 'heritage_subtitle_text')
  #    }
  #   
  #   input$heritage_currLevel
  #   
  # })
  
  observeEvent(input$heritage_currLevel, {
    print('level change')
  })
  
  
  # Need to separate out
  
  
  
  observeEvent(input$heritage_select, {
    if (!is.null(input$heritage_currLevel)) { 
      if (input$heritage_currLevel==0) {
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#heritage_chart').highcharts();
            chart.drillUp();
            "
          )
        )
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#heritage_map').highcharts();
            chart.drillUp();
            "
          )
        )
      }
    }
    if (!is.null(input$heritage_currLevelMap)) { 
      if (input$heritage_currLevelMap==0) {
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#heritage_map').highcharts();
            chart.drillUp();
            "
          )
        )
        shinyjs::runjs(
          paste0(
            "
            var chart = $('#heritage_chart').highcharts();
            chart.drillUp();
            "
          )
        )
      }
    }
  }, priority=8)  
  
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  observeEvent(c(input$heritage_select, input$heritage_currLevel), { #input$heritage_compOps, input$heritage_currLevel,
    #browser()
    if (is.null(input$heritage_currLevel)==T) {
      update_drilldown_chart(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps)
      update_drilldown_chart_summary(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps)
      update_drilldown_map(input$heritage_select, df_list, "heritage_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    #browser()
    else if (input$heritage_currLevel==1) {
      update_drilldown_chart(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps, input$heritage_currLevel)
      #browser()
      update_drilldown_chart_summary(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps, input$heritage_currLevel)
      update_drilldown_map(input$heritage_select, df_list, "heritage_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    # else if (input$heritage_currLevel==0) {
    #   #browser()
    #   
    # }
    else {
      update_drilldown_chart(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps)
      update_drilldown_chart_summary(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps, input$heritage_currLevel)
    }
    # observeEvent(input$heritage_currLevel, {
    #   #browser()
    #   update_drilldown_chart(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps, 1)
    #   update_drilldown_chart_summary(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps, 1)
    #   update_drilldown_map(input$heritage_select, df_list, "heritage_map", QUESTION_LIST, bounds_region, bounds_borough)
    # }, ignoreInit=T, ignoreNULL=T)
  }, ignoreInit=T, ignoreNULL=F, priority=0)
  
  
  observeEvent(c(input$heritage_compOps), { #, input$heritage_currLevel
    update_drilldown_chart_summary(input$heritage_select, df_list, "heritage_chart", input$heritage_compOps, input$heritage_currLevel)
  }, ignoreInit=F, ignoreNULL=F, priority=3)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(
    c(input$heritage_currLevel), {
      
      #browser()
      req(input$heritage_currLevel)
      selected <- input$heritage_compOps
      if (input$heritage_currLevel==0) {
        updatePrettyCheckboxGroup(
          session, "heritage_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      else {
        updatePrettyCheckboxGroup(
          session, "heritage_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      
      observeEvent(c(input$heritage_select), {
        updatePrettyCheckboxGroup(
          session, "heritage_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  
  observe({
    print(input$heritage_tab)
  })
  
  observe({
    print(paste0('my level =', input$heritage_currLevelMap))
  })
  
  
  observeEvent(c(input$heritage_currLevel, input$heritage_currLevelMap, input$heritage_tab), {
    
    req(input$heritage_tab) # still don't really understand req() but is required 
    #browser()
    if (input$heritage_tab=='chart') {
      req(input$heritage_currLevel)
      #browser()
      if (input$heritage_currLevel==0) {
        shinyjs::show('heritage-text-drilldown')
      }
      else  {
        shinyjs::hide('heritage-text-drilldown')
      }
      observeEvent(c(input$heritage_select, input$heritage_tab), {
        shinyjs::hide('heritage-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    else {
      #browser()
      #delay(5000,
      req(input$heritage_currLevelMap)
      if (input$heritage_currLevelMap==0) {
        shinyjs::show('heritage-text-drilldown')
      }
      else  {
        shinyjs::hide('heritage-text-drilldown')
      }
      observeEvent(c(input$heritage_select, input$heritage_tab), {
        shinyjs::hide('heritage-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  reactive__countFromToHeritage <- reactive({
    question <- as.numeric(input$heritage_select)
    df_region <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    countTo <- df_region$prop_resp[df_region$region=='London']
  })
  
  reactiveVal__countFromToHeritage <-  reactiveValues(countFrom=0, countTo=0)
  
  observeEvent( 
    reactive__countFromToHeritage(),{
      reactiveVal__countFromToHeritage$countFrom <- reactiveVal__countFromToHeritage$countTo; 
      #print(reactiveVal__countFromTo$countFrom)
      reactiveVal__countFromToHeritage$countTo <- reactive__countFromToHeritage()
      #print(reactiveVal__countFromTo$countTo)
    }
  )
  
  reactive__countToHeritage <- reactive({req(reactive__countFromToHeritage());  reactive__countFromToHeritage(); reactiveVal__countFromToHeritage$countTo})
  reactive__countFromHeritage <- reactive({req(reactive__countFromToHeritage()); reactive__countFromToHeritage(); reactiveVal__countFromToHeritage$countFrom})
  
  
  output$countTo <- renderPrint({reactive__countTo()})
  output$countFrom <- renderPrint({ reactive__countFrom()})
  
  
  observeEvent(
    input$heritage_select,once=T, ignoreNULL=F, ignoreInit=F, {
      insertUI(
        selector = "#countUp-ui-heritage",
        where = "afterEnd",
        ui = div(generate_countUp(reactiveVal__countFromToHeritage$countTo, reactiveVal__countFromToHeritage$countFrom, 'heritage'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
      )
    }
  )
  
  observeEvent(reactiveVal__countFromToHeritage$countTo, {
    countupProxy("countUp-heritage") %>% 
      countup_update(reactiveVal__countFromToHeritage$countTo)
  }, priority=8)
  
  
  #' output$heritage_previous_btn <- renderUI({
  #'   # tooltip(
  #'   actionButton(
  #'     "heritage_previous", 
  #'     "Previous question",
  #'     icon=icon("backward")
  #'   )
  #'   #,
  #'   #'A fucking message'
  #'   #)
  #' })
  #' output$heritage_next_btn <- renderUI({
  #'   actionButton(
  #'     "heritage_next", 
  #'     "Next question",
  #'     icon=icon("forward")
  #'   )
  #' })
  #' 
  #' 
  #' output$heritage_title_chart <- renderText({
  #'   print(paste(df_list[[as.numeric(input$heritage_select)]][['region']][['title']]))
  #' })
  #' output$heritage_title_map <- renderText({
  #'   print(paste(df_list[[as.numeric(input$heritage_select)]][['region']][['title']]))
  #' })
  #' output$heritage_subtitle_chart <- renderText({
  #'   if (is.null(input$heritage_currLevel)) {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #'   else if (input$heritage_currLevel==0) {
  #'     print(paste("Click 'Back to Regions' to drillup" ))
  #'   }
  #'   else {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #' })
  #' output$heritage_subtitle_map <- renderText({
  #'   if (is.null(input$heritage_currLevelMap)) {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #'   else if (input$heritage_currLevelMap==0) {
  #'     print(paste("Click 'Back to Regions' to drillup" ))
  #'   }
  #'   else {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #' })
  #' 
  #' 
  #' 
  #' output$heritage_chart <- renderHighchart({
  #'   generate_drilldown_chart(input$heritage_select, df_list, 'heritage', shinybrowser::get_height())
  #' })
  #' output$heritage_map <- renderHighchart({
  #'   generate_drilldown_map(input$heritage_select, df_list, 'heritage', QUESTION_LIST, bounds_region, bounds_borough)
  #' })
  #' 
  #' 
  #' reactive__heritage_select <- reactive({req(input$heritage_select)})
  #' reactive__mouseOver_heritage <- reactiveValues()
  #' observeEvent(c(input$heritage_tab, input$heritage_chart_mouseOver$name,input$heritage_currLevel, input$heritage_map_mouseOver,input$heritage_currLevelMap, input$heritage_select), {
  #'   observeEvent(c(input$heritage_tab, input$heritage_select, input$heritage_currLevel, input$heritage_currLevelMap), {
  #'     reg_name <- 'London'
  #'     reg_val <-
  #'       df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'       mutate(prop_resp=round(prop_resp,1)) %>%
  #'       select(region, prop_resp) %>%
  #'       filter(region=='London') %>%
  #'       select(prop_resp) %>%
  #'       pull(1)
  #'     eng_val <-
  #'       df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'       mutate(mean = round(mean(prop_resp),1)) %>%
  #'       filter(region=='London') %>%
  #'       select(mean) %>%
  #'       pull(1)
  #'     reg_eng_dif <- round(reg_val - eng_val ,1)
  #'     reg_rank <-
  #'       df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'       select(region, prop_resp, rank_direction) %>%
  #'       mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'       filter(region=='London') %>%
  #'       select(rank) %>%
  #'       pull(1)
  #'     
  #'   }, ignoreInit=T, ignoreNULL=T)
  #'   if (input$heritage_tab=='chart') {
  #'     if(is.null(input$heritage_chart_mouseOver$name)) {
  #'       #browser()
  #'       reg_name <- 'London'
  #'       reg_val <-
  #'         df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'         mutate(prop_resp=round(prop_resp,1)) %>%
  #'         select(region, prop_resp) %>%
  #'         filter(region=='London') %>%
  #'         select(prop_resp) %>%
  #'         pull(1)
  #'       eng_val <-
  #'         df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'         mutate(mean = round(mean(prop_resp),1)) %>%
  #'         filter(region=='London') %>%
  #'         select(mean) %>%
  #'         pull(1)
  #'       reg_eng_dif <- round(reg_val - eng_val ,1)
  #'       reg_rank <-
  #'         df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'         select(region, prop_resp, rank_direction) %>%
  #'         mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'         filter(region=='London') %>%
  #'         select(rank) %>%
  #'         pull(1)
  #'       #browser()
  #'     }
  #'     # On mouseOver
  #'     else {
  #'       
  #'       if (is.null(input$heritage_currLevel)) {
  #'         reg_name <- input$heritage_chart_mouseOver$name
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$heritage_chart_mouseOver$name) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$heritage_chart_mouseOver$name) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$heritage_chart_mouseOver$name) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'         #browser()
  #'       }
  #'       else if (input$heritage_currLevel==1) {
  #'         reg_name <- input$heritage_chart_mouseOver$name
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$heritage_chart_mouseOver$name) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$heritage_chart_mouseOver$name) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$heritage_chart_mouseOver$name) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'         # TODO TRY HERE!!!!!!!!
  #'         #browser()
  #'       }
  #'       else {
  #'         reg_name <- 'London'
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region=='London') %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region=='London') %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region=='London') %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'     }
  #'   }
  #'   if (input$heritage_tab=='map') {
  #'     if(is.null(input$heritage_map_mouseOver)) {
  #'       # browser()
  #'       reg_name <- 'London'
  #'       reg_val <-
  #'         df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'         mutate(prop_resp=round(prop_resp,1)) %>%
  #'         select(region, prop_resp) %>%
  #'         filter(region=='London') %>%
  #'         select(prop_resp) %>%
  #'         pull(1)
  #'       eng_val <-
  #'         df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'         mutate(mean = round(mean(prop_resp),1)) %>%
  #'         filter(region=='London') %>%
  #'         select(mean) %>%
  #'         pull(1)
  #'       reg_eng_dif <- round(reg_val - eng_val ,1)
  #'       reg_rank <-
  #'         df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'         select(region, prop_resp, rank_direction) %>%
  #'         mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'         filter(region=='London') %>%
  #'         select(rank) %>%
  #'         pull(1)
  #'     }
  #'     # On mouseOver
  #'     else {
  #'       if (is.null(input$heritage_currLevelMap)) {
  #'         #browser()
  #'         reg_name <- input$heritage_map_mouseOver
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$heritage_map_mouseOver) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$heritage_map_mouseOver) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$heritage_map_mouseOver) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'       else if (input$heritage_currLevelMap==1) {
  #'         #browser()
  #'         reg_name <- input$heritage_map_mouseOver
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$heritage_map_mouseOver) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$heritage_map_mouseOver) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$heritage_map_mouseOver) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'       else {
  #'         reg_name <- 'London'
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region=='London') %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region=='London') %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__heritage_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region=='London') %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'     }
  #'   }
  #'   reactive__mouseOver_heritage$reg_name <- reg_name
  #'   reactive__mouseOver_heritage$reg_val <- reg_val
  #'   reactive__mouseOver_heritage$reg_eng_dif <- reg_eng_dif
  #'   reactive__mouseOver_heritage$reg_rank <- reg_rank
  #'   # browser()
  #'   # print('yo')
  #' }, ignoreNULL=F, ignoreInit=F
  #' )
  #' 
  #' 
  #' output$heritage_region_headline_text <- renderUI({
  #'   generate_region_headline_text(input$heritage_select, df_list)
  #' })
  #' output$heritage_region_summary_text <- renderUI({
  #'   generate_region_summary_text(input$heritage_select, df_list, input$heritage_currLevel, reactive__mouseOver_heritage$reg_name,  reactive__mouseOver_heritage$reg_val, reactive__mouseOver_heritage$reg_eng_dif, reactive__mouseOver_heritage$reg_rank)
  #' })
  #' 
  #' output$heritage_borough_text <- renderUI({
  #'   generate_borough_text(input$heritage_select, df_list)
  #' })
  #' 
  #' 
  #' 
  #' 
  #' 
  #' observeEvent(input$heritage_select, {
  #'   if (input$heritage_select!=HERITAGE_QUESTIONS[1]) {
  #'     shinyjs::show('heritage_previous_btn')
  #'   }
  #'   if (input$heritage_select==HERITAGE_QUESTIONS[1]) {
  #'     shinyjs::hide('heritage_previous_btn')
  #'   }
  #'   if (input$heritage_select!=HERITAGE_QUESTIONS[length(HERITAGE_QUESTIONS)]) {
  #'     shinyjs::show('heritage_next_btn')
  #'   }
  #'   if (input$heritage_select==HERITAGE_QUESTIONS[length(HERITAGE_QUESTIONS)])  { 
  #'     shinyjs::hide('heritage_next_btn')
  #'   }
  #'   
  #'   
  #' }, ignoreInit=T)
  #' 
  #' 
  #' 
  #' observeEvent(input$heritage_previous, {
  #'   current <- which(HERITAGE_QUESTIONS == input$heritage_select)
  #'   if(current > 1) {
  #'     updateSelectInput(
  #'       session, "heritage_select",
  #'       selected = HERITAGE_QUESTIONS[current - 1]
  #'     )
  #'     #update_drilldown_chart(input$heritage_select, df_list, "heritage_chart") 
  #'     update_drilldown_map(input$heritage_select, df_list, "heritage_map")
  #'     generate_region_text(input$heritage_select, df_list)
  #'     shinyjs::html(id = 'heritage_region_text', html =  generate_region_text(input$heritage_select, df_list))
  #'   }
  #' }, ignoreInit=T, ignoreNULL=T, priority=2
  #' )
  #' # 
  #' observeEvent(input$heritage_next, {
  #'   current <- which(HERITAGE_QUESTIONS == input$heritage_select)
  #'   if(current < length(HERITAGE_QUESTIONS)){
  #'     updateSelectInput(
  #'       session, "heritage_select",
  #'       selected = HERITAGE_QUESTIONS[current + 1]
  #'     )
  #'     #update_drilldown_chart(input$heritage_select, df_list, "heritage_chart") 
  #'     update_drilldown_map(input$heritage_select, df_list, "heritage_map")
  #'     shinyjs::html(id = 'heritage_region_text', html = generate_region_text(input$heritage_select, df_list))
  #'   }
  #' }, ignoreInit=T, ignoreNULL=T, priority=2
  #' )
  #' 
  #' # 
  #' # observeEvent(c(input$heritage_currLevel,input$heritage_currLevelMap), {
  #' #   if (is.null(input$heritage_currLevel)) {
  #' #     updateTextOutput(session, 'heritage_subtitle_text')
  #' #   }
  #' #    if (is.null(input$heritage_currLevelMap)) {
  #' #      updateTextOutput(session, 'heritage_subtitle_text')
  #' #    }
  #' #   
  #' #   input$heritage_currLevel
  #' #   
  #' # })
  #' 
  #' observeEvent(input$heritage_currLevel, {
  #'   print('level change')
  #' })
  #' 
  #' 
  #' observeEvent(c(input$heritage_compOps, input$heritage_currLevel, input$heritage_select), {
  #'   
  #'   # drillup event bug!!!
  #'   # https://github.com/blacklabel/custom_events/issues/139
  #'   
  #'   
  #'   
  #'   print(paste0('Current drilldown level: ',input$heritage_currLevel))
  #'   # print(input$heritage_currLevel)
  #'   question <- as.numeric(input$heritage_select)
  #'   # print(question)
  #'   df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp, num_resp, color, drilldown_central)
  #'   df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp_lb, num_resp, prop_resp_ub, drilldown_error)
  #'   df_borough <- df_list[[question ]][['borough']][['dataframe']]
  #'   if ('error'%in%input$heritage_compOps & 'mean'%ni%input$heritage_compOps) { #delay(110)
  #'     #browser()
  #'     if (is.null(input$heritage_currLevel)) {
  #'       #browser()
  #'       #browser()
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'England',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$heritage_currLevel==0) { # drill level
  #'       #browser()
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_borough$prop_resp_lb),",
  #'                     to:",mean(df_borough$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'London',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'            
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00'',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else { # top level
  #'       #browser()
  #'       
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'England',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'   }
  #'   else if ('mean'%in%input$heritage_compOps & 'error'%ni%input$heritage_compOps) {
  #'     #browser()
  #'     if (is.null(input$heritage_currLevel)) {
  #'       #browser()
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       # delay(0,
  #'       #       shinyjs::runjs(
  #'       #         paste0(
  #'       #           "
  #'       #         var chart = $('#heritage_chart').highcharts();
  #'       #           chart.yAxis[0].update({plotLines:
  #'       #             [{
  #'       #               value:",mean(df_region_central$prop_resp),",
  #'       #               color:'#d82222',
  #'       #               zIndex:99,
  #'       #               label: {
  #'       #                 text: 'England',
  #'       #                 verticalAlign: 'top',
  #'       #                 textAlign: 'center',
  #'       #                 rotation:0,
  #'       #                 y:-4,
  #'       #                 style: {
  #'       #                     color:'#d82222',
  #'       #                     fontWeight: 'normal',
  #'       #                     fontSize: '1.35vh'
  #'       #                 }
  #'       #               }
  #'       #             }]
  #'       #           });
  #'       #         console.log(chart);
  #'       #      "
  #'       #         )
  #'       #       )
  #'       # )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$heritage_currLevel==0) { # Borough level
  #'       #browser()
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_borough$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'London',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$heritage_currLevel==1) { # top level
  #'       #browser()
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else {
  #'       #   update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       #   delay(300,
  #'       #         shinyjs::runjs(
  #'       #           paste0(
  #'       #             "
  #'       #           var chart = $('#heritage_chart').highcharts();
  #'       #             chart.yAxis[0].update({plotLines:
  #'       #               [{
  #'       #                 value:",mean(df_region_central$prop_resp),",
  #'       #                 color:'#d82222',
  #'       #                 zIndex:99,
  #'       #                 label: {
  #'       #                   text: 'England',
  #'       #           verticalAlign: 'top',
  #'       #           textAlign: 'center',
  #'       #           rotation:0,
  #'       #           y:-4,
  #'       #           style: {
  #'       #               color:'#d82222',
  #'       #               fontWeight: 'normal'
  #'       #           }
  #'       #           
  #'       #                 }
  #'       #               }]
  #'       #             });
  #'       #           console.log(chart);
  #'       #        "
  #'       #           )
  #'       #         )
  #'       #   )
  #'       #   delay(300,
  #'       #         shinyjs::runjs(
  #'       #           paste0(
  #'       #             "
  #'       #           var chart = $('#heritage_chart').highcharts();
  #'       #             chart.yAxis[0].update({plotBands:
  #'       #               [{
  #'       #                 from:0,
  #'       #                 to:0,
  #'       #                 color:'#d822221F',
  #'       #                 zIndex:98
  #'       # 
  #'       #               }]
  #'       #             });
  #'       #           console.log(chart);
  #'       #        "
  #'       #           )
  #'       #         )
  #'       #   )
  #'     }
  #'     
  #'   }
  #'   
  #'   else if ('mean'%in%input$heritage_compOps & 'error'%in%input$heritage_compOps) {
  #'     if (is.null(input$heritage_currLevel)) {
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$heritage_currLevel==0) {
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_borough$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'London',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_borough$prop_resp_lb),",
  #'                     to:",mean(df_borough$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       
  #'     }
  #'     else{
  #'       update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'   }
  #'   else {
  #'     update_drilldown_chart(input$heritage_select, df_list, "heritage_chart")
  #'     delay(300,
  #'           shinyjs::runjs(
  #'             "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'           )
  #'     )
  #'     delay(300,
  #'           shinyjs::runjs(
  #'             "
  #'               var chart = $('#heritage_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'           )
  #'     )
  #'     
  #'     #browser()
  #'   }
  #' }, ignoreInit=F, ignoreNULL=F, priority=0)
  #' 
  #' 
  #' 
  #' observeEvent(
  #'   c(input$heritage_currLevel), {
  #'     
  #'     #browser()
  #'     req(input$heritage_currLevel)
  #'     selected <- input$heritage_compOps
  #'     if (input$heritage_currLevel==0) {
  #'       updatePrettyCheckboxGroup(
  #'         session, "heritage_compOps",
  #'         choices=c(
  #'           'London mean'='mean',
  #'           'London error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'     }
  #'     else {
  #'       updatePrettyCheckboxGroup(
  #'         session, "heritage_compOps",
  #'         choices=c(
  #'           'England mean'='mean',
  #'           'England error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'     }
  #'     
  #'     observeEvent(c(input$heritage_select), {
  #'       updatePrettyCheckboxGroup(
  #'         session, "heritage_compOps",
  #'         choices=c(
  #'           'England mean'='mean',
  #'           'England error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'       
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-4)
  #'   }, ignoreInit=T, ignoreNULL=F, priority=-3
  #' )
  #' 
  #' observe({
  #'   print(input$heritage_tab)
  #' })
  #' 
  #' observe({
  #'   print(paste0('my level =', input$heritage_currLevelMap))
  #' })
  #' 
  #' 
  #' observeEvent(c(input$heritage_currLevel, input$heritage_currLevelMap, input$heritage_tab), {
  #'   
  #'   req(input$heritage_tab) # still don't really understand req() but is required 
  #'   #browser()
  #'   if (input$heritage_tab=='chart') {
  #'     req(input$heritage_currLevel)
  #'     #browser()
  #'     if (input$heritage_currLevel==0) {
  #'       shinyjs::show('heritage-text-drilldown')
  #'     }
  #'     else  {
  #'       shinyjs::hide('heritage-text-drilldown')
  #'     }
  #'     observeEvent(c(input$heritage_select, input$heritage_tab), {
  #'       shinyjs::hide('heritage-text-drilldown')
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-2)
  #'   }
  #'   
  #'   else {
  #'     #browser()
  #'     #delay(5000,
  #'     req(input$heritage_currLevelMap)
  #'     if (input$heritage_currLevelMap==0) {
  #'       shinyjs::show('heritage-text-drilldown')
  #'     }
  #'     else  {
  #'       shinyjs::hide('heritage-text-drilldown')
  #'     }
  #'     observeEvent(c(input$heritage_select, input$heritage_tab), {
  #'       shinyjs::hide('heritage-text-drilldown')
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-2)
  #'   }
  #'   
  #'   
  #' }, ignoreInit=T, ignoreNULL=T, priority=-1
  #' )
  #' 
  #' 
  #' reactive__countFromToHeritage <- reactive({
  #'   question <- as.numeric(input$heritage_select)
  #'   df_region <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp, color, drilldown_central)
  #'   countTo <- df_region$prop_resp[df_region$region=='London']
  #' })
  #' 
  #' reactiveVal__countFromToHeritage <-  reactiveValues(countFrom=0, countTo=0)
  #' 
  #' observeEvent( 
  #'   reactive__countFromToHeritage(),{
  #'     reactiveVal__countFromToHeritage$countFrom <- reactiveVal__countFromToHeritage$countTo; 
  #'     #print(reactiveVal__countFromTo$countFrom)
  #'     reactiveVal__countFromToHeritage$countTo <- reactive__countFromToHeritage()
  #'     #print(reactiveVal__countFromTo$countTo)
  #'   }
  #' )
  #' 
  #' reactive__countToHeritage <- reactive({req(reactive__countFromToHeritage());  reactive__countFromToHeritage(); reactiveVal__countFromToHeritage$countTo})
  #' reactive__countFromHeritage <- reactive({req(reactive__countFromToHeritage()); reactive__countFromToHeritage(); reactiveVal__countFromToHeritage$countFrom})
  #' 
  #' 
  #' output$countTo <- renderPrint({reactive__countTo()})
  #' output$countFrom <- renderPrint({ reactive__countFrom()})
  #' 
  #' 
  #' observeEvent(
  #'   input$heritage_select,once=T, ignoreNULL=F, ignoreInit=F, {
  #'     insertUI(
  #'       selector = "#countUp-ui-heritage",
  #'       where = "afterEnd",
  #'       ui = div(generate_countUp(reactiveVal__countFromToHeritage$countTo, reactiveVal__countFromToHeritage$countFrom, 'heritage'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
  #'     )
  #'   }
  #' )
  #' 
  #' observeEvent(reactiveVal__countFromToHeritage$countTo, {
  #'   countupProxy("countUp-heritage") %>% 
  #'     countup_update(reactiveVal__countFromToHeritage$countTo)
  #' })
  #' 
  #' 
  
  #=============================================================================
  # Sport Server
  #=============================================================================
  
  
  output$sport_previous_btn <- renderUI({
    # tooltip(
    actionButton(
      "sport_previous", 
      "Previous question",
      icon=icon("backward")
    )
    #,
    #'A fucking message'
    #)
  })
  output$sport_next_btn <- renderUI({
    actionButton(
      "sport_next", 
      "Next question",
      icon=icon("forward")
    )
  })
  
  
  output$sport_title_chart <- renderText({
    print(paste(df_list[[as.numeric(input$sport_select)]][['region']][['title']]))
  })
  output$sport_title_map <- renderText({
    print(paste(df_list[[as.numeric(input$sport_select)]][['region']][['title']]))
  })
  outputOptions(output, "sport_title_map", suspendWhenHidden = FALSE)
  
  output$sport_subtitle_chart <- renderText({
    if (is.null(input$sport_currLevel)) {
      print(paste("Drilldown functionality is not available since Borough-level data has not been published"))
    }
  })
  output$sport_subtitle_map <- renderText({
    if (is.null(input$sport_currLevel)) {
      print(paste("Drilldown functionality is not available since Borough-level data has not been published"))
    }
  })
  # output$sport_subtitle_map <- renderText({
  #   if (is.null(input$sport_currLevelMap)) {
  #     print(paste("Click to drilldown into London by Borough"))
  #   }
  #   else if (input$sport_currLevelMap==0) {
  #     print(paste("Click 'Back to Regions' to drillup" ))
  #   }
  #   else {
  #     print(paste("Click to drilldown into London by Borough"))
  #   }
  # })
  outputOptions(output, "sport_subtitle_map", suspendWhenHidden = FALSE)
  
  
  
  output$sport_chart <- renderHighchart({
    generate_drilldown_chart(SPORT_QUESTIONS[1], df_list, 'sport', shinybrowser::get_height())
  })
  output$sport_map <- renderHighchart({
    generate_drilldown_map(SPORT_QUESTIONS[1], df_list, 'sport', QUESTION_LIST, bounds_region, bounds_borough)
  })
  outputOptions(output, "sport_map", suspendWhenHidden = FALSE)
  
  
  reactive__sport_select <- reactive({req(input$sport_select)})
  reactive__mouseOver_sport <- reactiveValues()
  observeEvent(c(input$sport_tab, input$sport_chart_mouseOver$name,input$sport_currLevel, input$sport_map_mouseOver,input$sport_currLevelMap, input$sport_select), {
    observeEvent(c(input$sport_tab, input$sport_select, input$sport_currLevel, input$sport_currLevelMap), {
      reg_name <- 'London'
      reg_val <-
        df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        select(region, prop_resp) %>%
        filter(region=='London') %>%
        select(prop_resp) %>%
        pull(1)
      eng_val <-
        df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
        select(prop_resp) %>%
        mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
        pull(1)
      reg_eng_dif <- round(reg_val - eng_val ,1)
      reg_rank <-
        df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
        select(region, prop_resp, rank_direction) %>%
        mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
        filter(region=='London') %>%
        select(rank) %>%
        pull(1)
      
    }, ignoreInit=T, ignoreNULL=T)
    if (input$sport_tab=='chart') {
      if(is.null(input$sport_chart_mouseOver$name)) {
        #browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
        #browser()
      }
      # On mouseOver
      else {
        
        if (is.null(input$sport_currLevel)) {
          reg_name <- input$sport_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$sport_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$sport_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          #browser()
        }
        else if (input$sport_currLevel==1) {
          reg_name <- input$sport_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$sport_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$sport_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          # TODO TRY HERE!!!!!!!!
          #browser()
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    if (input$sport_tab=='map') {
      if(is.null(input$sport_map_mouseOver)) {
        # browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
      }
      # On mouseOver
      else {
        if (is.null(input$sport_currLevelMap)) {
          #browser()
          reg_name <- input$sport_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$sport_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$sport_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else if (input$sport_currLevelMap==1) {
          #browser()
          reg_name <- input$sport_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$sport_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$sport_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    reactive__mouseOver_sport$reg_name <- reg_name
    reactive__mouseOver_sport$reg_val <- reg_val
    reactive__mouseOver_sport$eng_val <- eng_val
    reactive__mouseOver_sport$reg_eng_dif <- reg_eng_dif
    reactive__mouseOver_sport$reg_rank <- reg_rank
    # browser()
    # print('yo')
  }, ignoreNULL=F, ignoreInit=F
  )
  
  
  output$sport_region_headline_text <- renderUI({
    generate_region_headline_text(input$sport_select, df_list)
  })
  output$sport_region_summary_text <- renderUI({
    generate_region_summary_text(input$sport_select, df_list, input$sport_currLevel, reactive__mouseOver_sport$reg_name,  reactive__mouseOver_sport$reg_val, reactive__mouseOver_sport$eng_val, reactive__mouseOver_sport$reg_eng_dif, reactive__mouseOver_sport$reg_rank)
  })
  
  # output$sport_borough_text <- renderUI({
  #   generate_borough_text(input$sport_select, df_list)
  # })
  # 
  
  
  
  
  observeEvent(input$sport_select, {
    if (input$sport_select!=SPORT_QUESTIONS[1]) {
      shinyjs::show('sport_previous_btn')
    }
    if (input$sport_select==SPORT_QUESTIONS[1]) {
      shinyjs::hide('sport_previous_btn')
    }
    if (input$sport_select!=SPORT_QUESTIONS[length(SPORT_QUESTIONS)]) {
      shinyjs::show('sport_next_btn')
    }
    if (input$sport_select==SPORT_QUESTIONS[length(SPORT_QUESTIONS)])  { 
      shinyjs::hide('sport_next_btn')
    }
    
    
  }, ignoreInit=T)
  
  
  
  observeEvent(input$sport_previous, {
    current <- which(SPORT_QUESTIONS == input$sport_select)
    if(current > 1) {
      updateSelectInput(
        session, "sport_select",
        selected = SPORT_QUESTIONS[current - 1]
      )
      #update_drilldown_chart(input$sport_select, df_list, "sport_chart") 
      #update_drilldown_map(input$sport_select, df_list, "sport_map")
      # if (!is.null(input$sport_currLevel)) {
      # if (input$sport_currLevel==0) {
      #   #browser()
      # # output$sport_chart <- NULL
      # # output$sport_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$sport_select, df_list, 'sport', shinybrowser::get_height())
      # # })
      #   # highchartProxy('sport_chart') %>%
      #   # hcpxy_redraw()
      # }
      # }
      
      
      
      #shinyjs::html(id = 'sport_region_headline_text', html=generate_region_headline_text(input$sport_select, df_list))
      #shinyjs::html(id = 'sport_region_summary_text', html=generate_region_summary_text(input$sport_select, df_list, input$sport_currLevel, reactive__mouseOver_sport$reg_name,  reactive__mouseOver_sport$reg_val, reactive__mouseOver_sport$reg_eng_dif, reactive__mouseOver_sport$reg_rank))
      
      
      
      #shinyjs::html(id = 'sport_borough_text', html=generate_borough_text(input$sport_select, df_list))
      #shinyjs::html(id = 'sport_region_text', html =  generate_region_text(input$sport_select, df_list))
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
      #input$sport_currLevel <- 1
      #update_drilldown_chart(input$sport_select, df_list, "sport_chart") 
      #update_drilldown_map(input$sport_select, df_list, "sport_map")
      #browser()
      # if (!is.null(input$sport_currLevel)) {
      # if (input$sport_currLevel==0) {
      # #browser()
      # # output$sport_chart <- NULL
      # # output$sport_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$sport_select, df_list, 'sport', shinybrowser::get_height())
      # # })
      #   highchartProxy('sport_chart') %>%
      #   hcpxy_redraw()
      # }#
      # }
      
      
      #shinyjs::html(id = 'sport_region_headline_text', html=generate_region_headline_text(input$sport_select, df_list))
      #shinyjs::html(id = 'sport_region_summary_text', html=generate_region_summary_text(input$sport_select, df_list, input$sport_currLevel, reactive__mouseOver_sport$reg_name,  reactive__mouseOver_sport$reg_val, reactive__mouseOver_sport$reg_eng_dif, reactive__mouseOver_sport$reg_rank))
      
      
      
      #shinyjs::html(id = 'sport_borough_text', html=generate_borough_text(input$sport_select, df_list))
      
    }
  }, ignoreInit=T, ignoreNULL=T, priority=4
  )
  
  # 
  # observeEvent(c(input$sport_currLevel,input$sport_currLevelMap), {
  #   if (is.null(input$sport_currLevel)) {
  #     updateTextOutput(session, 'sport_subtitle_text')
  #   }
  #    if (is.null(input$sport_currLevelMap)) {
  #      updateTextOutput(session, 'sport_subtitle_text')
  #    }
  #   
  #   input$sport_currLevel
  #   
  # })
  
  observeEvent(input$sport_currLevel, {
    print('level change')
  })
  
  
  # Need to separate out
  
  
  
  # observeEvent(input$sport_select, {
  #   if (!is.null(input$sport_currLevel)) { 
  #     if (input$sport_currLevel==0) {
  #       shinyjs::runjs(
  #         paste0(
  #           "
  #           var chart = $('#sport_chart').highcharts();
  #           chart.drillUp();
  #           "
  #         )
  #       )
  #     }
  #   }
  #   if (!is.null(input$sport_currLevelMap)) { 
  #     if (input$sport_currLevelMap==0) {
  #       shinyjs::runjs(
  #         paste0(
  #           "
  #           var chart = $('#sport_map').highcharts();
  #           chart.drillUp();
  #           "
  #         )
  #       )
  #     }
  #   }
  #   
  # }, priority=2)  
  
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  observeEvent(c(input$sport_select, input$sport_currLevel), { #input$sport_compOps, input$sport_currLevel,
    #browser()
    if (is.null(input$sport_currLevel)==T) {
      update_drilldown_chart(input$sport_select, df_list, "sport_chart", input$sport_compOps)
      update_drilldown_chart_summary(input$sport_select, df_list, "sport_chart", input$sport_compOps)
      update_drilldown_map(input$sport_select, df_list, "sport_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    #browser()
    else if (input$sport_currLevel==1) {
      update_drilldown_chart(input$sport_select, df_list, "sport_chart", input$sport_compOps, input$sport_currLevel)
      #browser()
      update_drilldown_chart_summary(input$sport_select, df_list, "sport_chart", input$sport_compOps, input$sport_currLevel)
      update_drilldown_map(input$sport_select, df_list, "sport_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    # else if (input$sport_currLevel==0) {
    #   #browser()
    #   
    # }
    else {
      update_drilldown_chart(input$sport_select, df_list, "sport_chart", input$sport_compOps)
      update_drilldown_chart_summary(input$sport_select, df_list, "sport_chart", input$sport_compOps, input$sport_currLevel)
    }
    # observeEvent(input$sport_currLevel, {
    #   #browser()
    #   update_drilldown_chart(input$sport_select, df_list, "sport_chart", input$sport_compOps, 1)
    #   update_drilldown_chart_summary(input$sport_select, df_list, "sport_chart", input$sport_compOps, 1)
    #   update_drilldown_map(input$sport_select, df_list, "sport_map", QUESTION_LIST, bounds_region, bounds_borough)
    # }, ignoreInit=T, ignoreNULL=T)
  }, ignoreInit=T, ignoreNULL=F, priority=0)
  
  
  observeEvent(c(input$sport_compOps, input$sport_currLevel), { #input$sport_compOps, input$sport_currLevel,
    update_drilldown_chart_summary(input$sport_select, df_list, "sport_chart", input$sport_compOps, input$sport_currLevel)
  }, ignoreInit=F, ignoreNULL=F, priority=3)
  
  
  
  
  observeEvent(
    c(input$sport_currLevel), {
      
      #browser()
      req(input$sport_currLevel)
      selected <- input$sport_compOps
      if (input$sport_currLevel==0) {
        updatePrettyCheckboxGroup(
          session, "sport_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      else {
        updatePrettyCheckboxGroup(
          session, "sport_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      
      observeEvent(c(input$sport_select), {
        updatePrettyCheckboxGroup(
          session, "sport_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  
  observe({
    print(input$sport_tab)
  })
  
  observe({
    print(paste0('my level =', input$sport_currLevelMap))
  })
  
  
  observeEvent(c(input$sport_currLevel, input$sport_currLevelMap, input$sport_tab), {
    
    req(input$sport_tab) # still don't really understand req() but is required 
    #browser()
    if (input$sport_tab=='chart') {
      req(input$sport_currLevel)
      #browser()
      if (input$sport_currLevel==0) {
        shinyjs::show('sport-text-drilldown')
      }
      else  {
        shinyjs::hide('sport-text-drilldown')
      }
      observeEvent(c(input$sport_select, input$sport_tab), {
        shinyjs::hide('sport-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    else {
      #browser()
      #delay(5000,
      req(input$sport_currLevelMap)
      if (input$sport_currLevelMap==0) {
        shinyjs::show('sport-text-drilldown')
      }
      else  {
        shinyjs::hide('sport-text-drilldown')
      }
      observeEvent(c(input$sport_select, input$sport_tab), {
        shinyjs::hide('sport-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  reactive__countFromToSport <- reactive({
    question <- as.numeric(input$sport_select)
    df_region <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    countTo <- df_region$prop_resp[df_region$region=='London']
  })
  
  reactiveVal__countFromToSport <-  reactiveValues(countFrom=0, countTo=0)
  
  observeEvent( 
    reactive__countFromToSport(),{
      reactiveVal__countFromToSport$countFrom <- reactiveVal__countFromToSport$countTo; 
      #print(reactiveVal__countFromTo$countFrom)
      reactiveVal__countFromToSport$countTo <- reactive__countFromToSport()
      #print(reactiveVal__countFromTo$countTo)
    }
  )
  
  reactive__countToSport <- reactive({req(reactive__countFromToSport());  reactive__countFromToSport(); reactiveVal__countFromToSport$countTo})
  reactive__countFromSport <- reactive({req(reactive__countFromToSport()); reactive__countFromToSport(); reactiveVal__countFromToSport$countFrom})
  
  
  output$countTo <- renderPrint({reactive__countTo()})
  output$countFrom <- renderPrint({ reactive__countFrom()})
  
  
  observeEvent(
    input$sport_select,once=T, ignoreNULL=F, ignoreInit=F, {
      insertUI(
        selector = "#countUp-ui-sport",
        where = "afterEnd",
        ui = div(generate_countUp(reactiveVal__countFromToSport$countTo, reactiveVal__countFromToSport$countFrom, 'sport'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
      )
    }
  )
  
  observeEvent(reactiveVal__countFromToSport$countTo, {
    countupProxy("countUp-sport") %>% 
      countup_update(reactiveVal__countFromToSport$countTo)
  }, priority=8)
  
  #' output$sport_previous_btn <- renderUI({
  #'   # tooltip(
  #'   actionButton(
  #'     "sport_previous", 
  #'     "Previous question",
  #'     icon=icon("backward")
  #'   )
  #'   #,
  #'   #'A fucking message'
  #'   #)
  #' })
  #' output$sport_next_btn <- renderUI({
  #'   actionButton(
  #'     "sport_next", 
  #'     "Next question",
  #'     icon=icon("forward")
  #'   )
  #' })
  #' 
  #' 
  #' output$sport_title_chart <- renderText({
  #'   print(paste(df_list[[as.numeric(input$sport_select)]][['region']][['title']]))
  #' })
  #' output$sport_title_map <- renderText({
  #'   print(paste(df_list[[as.numeric(input$sport_select)]][['region']][['title']]))
  #' })
  #' output$sport_subtitle_chart <- renderText({
  #'   if (is.null(input$sport_currLevel)) {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #'   else if (input$sport_currLevel==0) {
  #'     print(paste("Click 'Back to Regions' to drillup" ))
  #'   }
  #'   else {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #' })
  #' output$sport_subtitle_map <- renderText({
  #'   if (is.null(input$sport_currLevelMap)) {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #'   else if (input$sport_currLevelMap==0) {
  #'     print(paste("Click 'Back to Regions' to drillup" ))
  #'   }
  #'   else {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #' })
  #' 
  #' 
  #' 
  #' output$sport_chart <- renderHighchart({
  #'   generate_drilldown_chart(input$sport_select, df_list, 'sport', shinybrowser::get_height())
  #' })
  #' output$sport_map <- renderHighchart({
  #'   generate_drilldown_map(input$sport_select, df_list, 'sport', QUESTION_LIST, bounds_region, bounds_borough)
  #' })
  #' 
  #' 
  #' reactive__sport_select <- reactive({req(input$sport_select)})
  #' reactive__mouseOver_sport <- reactiveValues()
  #' observeEvent(c(input$sport_tab, input$sport_chart_mouseOver$name,input$sport_currLevel, input$sport_map_mouseOver,input$sport_currLevelMap, input$sport_select), {
  #'   observeEvent(c(input$sport_tab, input$sport_select, input$sport_currLevel, input$sport_currLevelMap), {
  #'     reg_name <- 'London'
  #'     reg_val <-
  #'       df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'       mutate(prop_resp=round(prop_resp,1)) %>%
  #'       select(region, prop_resp) %>%
  #'       filter(region=='London') %>%
  #'       select(prop_resp) %>%
  #'       pull(1)
  #'     eng_val <-
  #'       df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'       mutate(mean = round(mean(prop_resp),1)) %>%
  #'       filter(region=='London') %>%
  #'       select(mean) %>%
  #'       pull(1)
  #'     reg_eng_dif <- round(reg_val - eng_val ,1)
  #'     reg_rank <-
  #'       df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'       select(region, prop_resp, rank_direction) %>%
  #'       mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'       filter(region=='London') %>%
  #'       select(rank) %>%
  #'       pull(1)
  #'     
  #'   }, ignoreInit=T, ignoreNULL=T)
  #'   if (input$sport_tab=='chart') {
  #'     if(is.null(input$sport_chart_mouseOver$name)) {
  #'       #browser()
  #'       reg_name <- 'London'
  #'       reg_val <-
  #'         df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'         mutate(prop_resp=round(prop_resp,1)) %>%
  #'         select(region, prop_resp) %>%
  #'         filter(region=='London') %>%
  #'         select(prop_resp) %>%
  #'         pull(1)
  #'       eng_val <-
  #'         df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'         mutate(mean = round(mean(prop_resp),1)) %>%
  #'         filter(region=='London') %>%
  #'         select(mean) %>%
  #'         pull(1)
  #'       reg_eng_dif <- round(reg_val - eng_val ,1)
  #'       reg_rank <-
  #'         df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'         select(region, prop_resp, rank_direction) %>%
  #'         mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'         filter(region=='London') %>%
  #'         select(rank) %>%
  #'         pull(1)
  #'       #browser()
  #'     }
  #'     # On mouseOver
  #'     else {
  #'       
  #'       if (is.null(input$sport_currLevel)) {
  #'         reg_name <- input$sport_chart_mouseOver$name
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$sport_chart_mouseOver$name) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$sport_chart_mouseOver$name) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$sport_chart_mouseOver$name) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'         #browser()
  #'       }
  #'       else if (input$sport_currLevel==1) {
  #'         reg_name <- input$sport_chart_mouseOver$name
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$sport_chart_mouseOver$name) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$sport_chart_mouseOver$name) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$sport_chart_mouseOver$name) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'         # TODO TRY HERE!!!!!!!!
  #'         #browser()
  #'       }
  #'       else {
  #'         reg_name <- 'London'
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region=='London') %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region=='London') %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region=='London') %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'     }
  #'   }
  #'   if (input$sport_tab=='map') {
  #'     if(is.null(input$sport_map_mouseOver)) {
  #'       # browser()
  #'       reg_name <- 'London'
  #'       reg_val <-
  #'         df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'         mutate(prop_resp=round(prop_resp,1)) %>%
  #'         select(region, prop_resp) %>%
  #'         filter(region=='London') %>%
  #'         select(prop_resp) %>%
  #'         pull(1)
  #'       eng_val <-
  #'         df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'         mutate(mean = round(mean(prop_resp),1)) %>%
  #'         filter(region=='London') %>%
  #'         select(mean) %>%
  #'         pull(1)
  #'       reg_eng_dif <- round(reg_val - eng_val ,1)
  #'       reg_rank <-
  #'         df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'         select(region, prop_resp, rank_direction) %>%
  #'         mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'         filter(region=='London') %>%
  #'         select(rank) %>%
  #'         pull(1)
  #'     }
  #'     # On mouseOver
  #'     else {
  #'       if (is.null(input$sport_currLevelMap)) {
  #'         #browser()
  #'         reg_name <- input$sport_map_mouseOver
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$sport_map_mouseOver) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$sport_map_mouseOver) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$sport_map_mouseOver) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'       else if (input$sport_currLevelMap==1) {
  #'         #browser()
  #'         reg_name <- input$sport_map_mouseOver
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$sport_map_mouseOver) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$sport_map_mouseOver) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$sport_map_mouseOver) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'       else {
  #'         reg_name <- 'London'
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region=='London') %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region=='London') %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__sport_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region=='London') %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'     }
  #'   }
  #'   reactive__mouseOver_sport$reg_name <- reg_name
  #'   reactive__mouseOver_sport$reg_val <- reg_val
  #'   reactive__mouseOver_sport$reg_eng_dif <- reg_eng_dif
  #'   reactive__mouseOver_sport$reg_rank <- reg_rank
  #'   # browser()
  #'   # print('yo')
  #' }, ignoreNULL=F, ignoreInit=F
  #' )
  #' 
  #' 
  #' output$sport_region_headline_text <- renderUI({
  #'   generate_region_headline_text(input$sport_select, df_list)
  #' })
  #' output$sport_region_summary_text <- renderUI({
  #'   generate_region_summary_text(input$sport_select, df_list, input$sport_currLevel, reactive__mouseOver_sport$reg_name,  reactive__mouseOver_sport$reg_val, reactive__mouseOver_sport$reg_eng_dif, reactive__mouseOver_sport$reg_rank)
  #' })
  #' 
  #' output$sport_borough_text <- renderUI({
  #'   generate_borough_text(input$sport_select, df_list)
  #' })
  #' 
  #' 
  #' 
  #' 
  #' 
  #' observeEvent(input$sport_select, {
  #'   if (input$sport_select!=SPORT_QUESTIONS[1]) {
  #'     shinyjs::show('sport_previous_btn')
  #'   }
  #'   if (input$sport_select==SPORT_QUESTIONS[1]) {
  #'     shinyjs::hide('sport_previous_btn')
  #'   }
  #'   if (input$sport_select!=SPORT_QUESTIONS[length(SPORT_QUESTIONS)]) {
  #'     shinyjs::show('sport_next_btn')
  #'   }
  #'   if (input$sport_select==SPORT_QUESTIONS[length(SPORT_QUESTIONS)])  { 
  #'     shinyjs::hide('sport_next_btn')
  #'   }
  #'   
  #'   
  #' }, ignoreInit=T)
  #' 
  #' 
  #' 
  #' observeEvent(input$sport_previous, {
  #'   current <- which(SPORT_QUESTIONS == input$sport_select)
  #'   if(current > 1) {
  #'     updateSelectInput(
  #'       session, "sport_select",
  #'       selected = SPORT_QUESTIONS[current - 1]
  #'     )
  #'     #update_drilldown_chart(input$sport_select, df_list, "sport_chart") 
  #'     #update_drilldown_map(input$sport_select, df_list, "sport_map")
  #'     generate_region_text(input$sport_select, df_list)
  #'     shinyjs::html(id = 'sport_region_text', html =  generate_region_text(input$sport_select, df_list))
  #'   }
  #' }, ignoreInit=T, ignoreNULL=T, priority=2
  #' )
  #' # 
  #' observeEvent(input$sport_next, {
  #'   current <- which(SPORT_QUESTIONS == input$sport_select)
  #'   if(current < length(SPORT_QUESTIONS)){
  #'     updateSelectInput(
  #'       session, "sport_select",
  #'       selected = SPORT_QUESTIONS[current + 1]
  #'     )
  #'     #update_drilldown_chart(input$sport_select, df_list, "sport_chart") 
  #'     #update_drilldown_map(input$sport_select, df_list, "sport_map")
  #'     shinyjs::html(id = 'sport_region_text', html = generate_region_text(input$sport_select, df_list))
  #'   }
  #' }, ignoreInit=T, ignoreNULL=T, priority=2
  #' )
  #' 
  #' # 
  #' # observeEvent(c(input$sport_currLevel,input$sport_currLevelMap), {
  #' #   if (is.null(input$sport_currLevel)) {
  #' #     updateTextOutput(session, 'sport_subtitle_text')
  #' #   }
  #' #    if (is.null(input$sport_currLevelMap)) {
  #' #      updateTextOutput(session, 'sport_subtitle_text')
  #' #    }
  #' #   
  #' #   input$sport_currLevel
  #' #   
  #' # })
  #' 
  #' observeEvent(input$sport_currLevel, {
  #'   print('level change')
  #' })
  #' 
  #' 
  #' observeEvent(c(input$sport_compOps, input$sport_currLevel, input$sport_select), {
  #'   
  #'   # drillup event bug!!!
  #'   # https://github.com/blacklabel/custom_events/issues/139
  #'   
  #'   
  #'   
  #'   print(paste0('Current drilldown level: ',input$sport_currLevel))
  #'   # print(input$sport_currLevel)
  #'   question <- as.numeric(input$sport_select)
  #'   # print(question)
  #'   df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp, num_resp, color, drilldown_central)
  #'   df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp_lb, num_resp, prop_resp_ub, drilldown_error)
  #'   df_borough <- df_list[[question ]][['borough']][['dataframe']]
  #'   if ('error'%in%input$sport_compOps & 'mean'%ni%input$sport_compOps) { #delay(110)
  #'     #browser()
  #'     if (is.null(input$sport_currLevel)) {
  #'       #browser()
  #'       #browser()
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'England',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$sport_currLevel==0) { # drill level
  #'       #browser()
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_borough$prop_resp_lb),",
  #'                     to:",mean(df_borough$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'London',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'            
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00'',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else { # top level
  #'       #browser()
  #'       
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'England',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'   }
  #'   else if ('mean'%in%input$sport_compOps & 'error'%ni%input$sport_compOps) {
  #'     #browser()
  #'     if (is.null(input$sport_currLevel)) {
  #'       #browser()
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       # delay(0,
  #'       #       shinyjs::runjs(
  #'       #         paste0(
  #'       #           "
  #'       #         var chart = $('#sport_chart').highcharts();
  #'       #           chart.yAxis[0].update({plotLines:
  #'       #             [{
  #'       #               value:",mean(df_region_central$prop_resp),",
  #'       #               color:'#d82222',
  #'       #               zIndex:99,
  #'       #               label: {
  #'       #                 text: 'England',
  #'       #                 verticalAlign: 'top',
  #'       #                 textAlign: 'center',
  #'       #                 rotation:0,
  #'       #                 y:-4,
  #'       #                 style: {
  #'       #                     color:'#d82222',
  #'       #                     fontWeight: 'normal',
  #'       #                     fontSize: '1.35vh'
  #'       #                 }
  #'       #               }
  #'       #             }]
  #'       #           });
  #'       #         console.log(chart);
  #'       #      "
  #'       #         )
  #'       #       )
  #'       # )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$sport_currLevel==0) { # Borough level
  #'       #browser()
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_borough$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'London',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$sport_currLevel==1) { # top level
  #'       #browser()
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else {
  #'       #   update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       #   delay(300,
  #'       #         shinyjs::runjs(
  #'       #           paste0(
  #'       #             "
  #'       #           var chart = $('#sport_chart').highcharts();
  #'       #             chart.yAxis[0].update({plotLines:
  #'       #               [{
  #'       #                 value:",mean(df_region_central$prop_resp),",
  #'       #                 color:'#d82222',
  #'       #                 zIndex:99,
  #'       #                 label: {
  #'       #                   text: 'England',
  #'       #           verticalAlign: 'top',
  #'       #           textAlign: 'center',
  #'       #           rotation:0,
  #'       #           y:-4,
  #'       #           style: {
  #'       #               color:'#d82222',
  #'       #               fontWeight: 'normal'
  #'       #           }
  #'       #           
  #'       #                 }
  #'       #               }]
  #'       #             });
  #'       #           console.log(chart);
  #'       #        "
  #'       #           )
  #'       #         )
  #'       #   )
  #'       #   delay(300,
  #'       #         shinyjs::runjs(
  #'       #           paste0(
  #'       #             "
  #'       #           var chart = $('#sport_chart').highcharts();
  #'       #             chart.yAxis[0].update({plotBands:
  #'       #               [{
  #'       #                 from:0,
  #'       #                 to:0,
  #'       #                 color:'#d822221F',
  #'       #                 zIndex:98
  #'       # 
  #'       #               }]
  #'       #             });
  #'       #           console.log(chart);
  #'       #        "
  #'       #           )
  #'       #         )
  #'       #   )
  #'     }
  #'     
  #'   }
  #'   
  #'   else if ('mean'%in%input$sport_compOps & 'error'%in%input$sport_compOps) {
  #'     if (is.null(input$sport_currLevel)) {
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$sport_currLevel==0) {
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_borough$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'London',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_borough$prop_resp_lb),",
  #'                     to:",mean(df_borough$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       
  #'     }
  #'     else{
  #'       update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'   }
  #'   else {
  #'     update_drilldown_chart(input$sport_select, df_list, "sport_chart")
  #'     delay(300,
  #'           shinyjs::runjs(
  #'             "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'           )
  #'     )
  #'     delay(300,
  #'           shinyjs::runjs(
  #'             "
  #'               var chart = $('#sport_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'           )
  #'     )
  #'     
  #'     #browser()
  #'   }
  #' }, ignoreInit=F, ignoreNULL=F, priority=0)
  #' 
  #' 
  #' 
  #' observeEvent(
  #'   c(input$sport_currLevel), {
  #'     
  #'     #browser()
  #'     req(input$sport_currLevel)
  #'     selected <- input$sport_compOps
  #'     if (input$sport_currLevel==0) {
  #'       updatePrettyCheckboxGroup(
  #'         session, "sport_compOps",
  #'         choices=c(
  #'           'London mean'='mean',
  #'           'London error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'     }
  #'     else {
  #'       updatePrettyCheckboxGroup(
  #'         session, "sport_compOps",
  #'         choices=c(
  #'           'England mean'='mean',
  #'           'England error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'     }
  #'     
  #'     observeEvent(c(input$sport_select), {
  #'       updatePrettyCheckboxGroup(
  #'         session, "sport_compOps",
  #'         choices=c(
  #'           'England mean'='mean',
  #'           'England error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'       
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-4)
  #'   }, ignoreInit=T, ignoreNULL=F, priority=-3
  #' )
  #' 
  #' observe({
  #'   print(input$sport_tab)
  #' })
  #' 
  #' observe({
  #'   print(paste0('my level =', input$sport_currLevelMap))
  #' })
  #' 
  #' 
  #' observeEvent(c(input$sport_currLevel, input$sport_currLevelMap, input$sport_tab), {
  #'   
  #'   req(input$sport_tab) # still don't really understand req() but is required 
  #'   #browser()
  #'   if (input$sport_tab=='chart') {
  #'     req(input$sport_currLevel)
  #'     #browser()
  #'     if (input$sport_currLevel==0) {
  #'       shinyjs::show('sport-text-drilldown')
  #'     }
  #'     else  {
  #'       shinyjs::hide('sport-text-drilldown')
  #'     }
  #'     observeEvent(c(input$sport_select, input$sport_tab), {
  #'       shinyjs::hide('sport-text-drilldown')
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-2)
  #'   }
  #'   
  #'   else {
  #'     #browser()
  #'     #delay(5000,
  #'     req(input$sport_currLevelMap)
  #'     if (input$sport_currLevelMap==0) {
  #'       shinyjs::show('sport-text-drilldown')
  #'     }
  #'     else  {
  #'       shinyjs::hide('sport-text-drilldown')
  #'     }
  #'     observeEvent(c(input$sport_select, input$sport_tab), {
  #'       shinyjs::hide('sport-text-drilldown')
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-2)
  #'   }
  #'   
  #'   
  #' }, ignoreInit=T, ignoreNULL=T, priority=-1
  #' )
  #' 
  #' 
  #' reactive__countFromToSport <- reactive({
  #'   question <- as.numeric(input$sport_select)
  #'   df_region <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp, color, drilldown_central)
  #'   countTo <- df_region$prop_resp[df_region$region=='London']
  #' })
  #' 
  #' reactiveVal__countFromToSport <-  reactiveValues(countFrom=0, countTo=0)
  #' 
  #' observeEvent( 
  #'   reactive__countFromToSport(),{
  #'     reactiveVal__countFromToSport$countFrom <- reactiveVal__countFromToSport$countTo; 
  #'     #print(reactiveVal__countFromTo$countFrom)
  #'     reactiveVal__countFromToSport$countTo <- reactive__countFromToSport()
  #'     #print(reactiveVal__countFromTo$countTo)
  #'   }
  #' )
  #' 
  #' reactive__countToSport <- reactive({req(reactive__countFromToSport());  reactive__countFromToSport(); reactiveVal__countFromToSport$countTo})
  #' reactive__countFromSport <- reactive({req(reactive__countFromToSport()); reactive__countFromToSport(); reactiveVal__countFromToSport$countFrom})
  #' 
  #' 
  #' output$countTo <- renderPrint({reactive__countTo()})
  #' output$countFrom <- renderPrint({ reactive__countFrom()})
  #' 
  #' 
  #' observeEvent(
  #'   input$sport_select,once=T, ignoreNULL=F, ignoreInit=F, {
  #'     insertUI(
  #'       selector = "#countUp-ui-sport",
  #'       where = "afterEnd",
  #'       ui = div(generate_countUp(reactiveVal__countFromToSport$countTo, reactiveVal__countFromToSport$countFrom, 'sport'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
  #'     )
  #'   }
  #' )
  #' 
  #' observeEvent(reactiveVal__countFromToSport$countTo, {
  #'   countupProxy("countUp-sport") %>% 
  #'     countup_update(reactiveVal__countFromToSport$countTo)
  #' })
  #' 
  #' 
  
  #=============================================================================
  # Tourism Server
  #=============================================================================
  
  
  
  output$tourism_previous_btn <- renderUI({
    # tooltip(
    actionButton(
      "tourism_previous", 
      "Previous question",
      icon=icon("backward")
    )
    #,
    #'A fucking message'
    #)
  })
  output$tourism_next_btn <- renderUI({
    actionButton(
      "tourism_next", 
      "Next question",
      icon=icon("forward")
    )
  })
  
  
  output$tourism_title_chart <- renderText({
    print(paste(df_list[[as.numeric(input$tourism_select)]][['region']][['title']]))
  })
  output$tourism_title_map <- renderText({
    print(paste(df_list[[as.numeric(input$tourism_select)]][['region']][['title']]))
  })
  outputOptions(output, "tourism_title_map", suspendWhenHidden = FALSE)
  
  output$tourism_subtitle_chart <- renderText({
    if (is.null(input$tourism_currLevel)) {
      print(paste("Drilldown functionality is not available since Borough-level data has not been published"))
    }
  })
  
  output$tourism_subtitle_map <- renderText({
    if (is.null(input$tourism_currLevel)) {
      print(paste("Drilldown functionality is not available since Borough-level data has not been published"))
    }
  })
  # output$tourism_subtitle_map <- renderText({
  #   if (is.null(input$tourism_currLevelMap)) {
  #     print(paste("Click to drilldown into London by Borough"))
  #   }
  #   else if (input$tourism_currLevelMap==0) {
  #     print(paste("Click 'Back to Regions' to drillup" ))
  #   }
  #   else {
  #     print(paste("Click to drilldown into London by Borough"))
  #   }
  # })
  outputOptions(output, "tourism_subtitle_map", suspendWhenHidden = FALSE)
  
  
  
  output$tourism_chart <- renderHighchart({
    generate_drilldown_chart(TOURISM_QUESTIONS[1], df_list, 'tourism', shinybrowser::get_height())
  })
  output$tourism_map <- renderHighchart({
    generate_drilldown_map(TOURISM_QUESTIONS[1], df_list, 'tourism', QUESTION_LIST, bounds_region, bounds_borough)
  })
  outputOptions(output, "tourism_map", suspendWhenHidden = FALSE)
  
  
  reactive__tourism_select <- reactive({req(input$tourism_select)})
  reactive__mouseOver_tourism <- reactiveValues()
  observeEvent(c(input$tourism_tab, input$tourism_chart_mouseOver$name,input$tourism_currLevel, input$tourism_map_mouseOver,input$tourism_currLevelMap, input$tourism_select), {
    observeEvent(c(input$tourism_tab, input$tourism_select, input$tourism_currLevel, input$tourism_currLevelMap), {
      reg_name <- 'London'
      reg_val <-
        df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
        mutate(prop_resp=round(prop_resp,1)) %>%
        select(region, prop_resp) %>%
        filter(region=='London') %>%
        select(prop_resp) %>%
        pull(1)
      eng_val <-
        df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
        select(prop_resp) %>%
        mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
        pull(1)
      reg_eng_dif <- round(reg_val - eng_val ,1)
      reg_rank <-
        df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
        select(region, prop_resp, rank_direction) %>%
        mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
        filter(region=='London') %>%
        select(rank) %>%
        pull(1)
      
    }, ignoreInit=T, ignoreNULL=T)
    if (input$tourism_tab=='chart') {
      if(is.null(input$tourism_chart_mouseOver$name)) {
        #browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
        #browser()
      }
      # On mouseOver
      else {
        
        if (is.null(input$tourism_currLevel)) {
          reg_name <- input$tourism_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$tourism_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$tourism_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          #browser()
        }
        else if (input$tourism_currLevel==1) {
          reg_name <- input$tourism_chart_mouseOver$name
          reg_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$tourism_chart_mouseOver$name) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$tourism_chart_mouseOver$name) %>%
            select(rank) %>%
            pull(1)
          # TODO TRY HERE!!!!!!!!
          #browser()
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    if (input$tourism_tab=='map') {
      if(is.null(input$tourism_map_mouseOver)) {
        # browser()
        reg_name <- 'London'
        reg_val <-
          df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
          mutate(prop_resp=round(prop_resp,1)) %>%
          select(region, prop_resp) %>%
          filter(region=='London') %>%
          select(prop_resp) %>%
          pull(1)
        eng_val <-
          df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
          select(prop_resp) %>%
          mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
          pull(1)
        reg_eng_dif <- round(reg_val - eng_val ,1)
        reg_rank <-
          df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
          select(region, prop_resp, rank_direction) %>%
          mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
          filter(region=='London') %>%
          select(rank) %>%
          pull(1)
      }
      # On mouseOver
      else {
        if (is.null(input$tourism_currLevelMap)) {
          #browser()
          reg_name <- input$tourism_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$tourism_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$tourism_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else if (input$tourism_currLevelMap==1) {
          #browser()
          reg_name <- input$tourism_map_mouseOver
          reg_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region==input$tourism_map_mouseOver) %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          #browser()
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region==input$tourism_map_mouseOver) %>%
            select(rank) %>%
            pull(1)
        }
        else {
          reg_name <- 'London'
          reg_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            mutate(prop_resp=round(prop_resp,1)) %>%
            select(region, prop_resp) %>%
            filter(region=='London') %>%
            select(prop_resp) %>%
            pull(1)
          eng_val <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['summary']] %>%
            select(prop_resp) %>%
            mutate(prop_resp=round(as.numeric(as.character(prop_resp)),1)) %>%
            pull(1)
          reg_eng_dif <- round(reg_val - eng_val ,1)
          reg_rank <-
            df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
            select(region, prop_resp, rank_direction) %>%
            mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
            filter(region=='London') %>%
            select(rank) %>%
            pull(1)
        }
      }
    }
    reactive__mouseOver_tourism$reg_name <- reg_name
    reactive__mouseOver_tourism$reg_val <- reg_val
    reactive__mouseOver_tourism$eng_val <- eng_val
    reactive__mouseOver_tourism$reg_eng_dif <- reg_eng_dif
    reactive__mouseOver_tourism$reg_rank <- reg_rank
    # browser()
    # print('yo')
  }, ignoreNULL=F, ignoreInit=F
  )
  
  
  output$tourism_region_headline_text <- renderUI({
    generate_region_headline_text(input$tourism_select, df_list)
  })
  output$tourism_region_summary_text <- renderUI({
    generate_region_summary_text(input$tourism_select, df_list, input$tourism_currLevel, reactive__mouseOver_tourism$reg_name,  reactive__mouseOver_tourism$reg_val, reactive__mouseOver_tourism$eng_val, reactive__mouseOver_tourism$reg_eng_dif, reactive__mouseOver_tourism$reg_rank)
  })
  
  # output$tourism_borough_text <- renderUI({
  #   generate_borough_text(input$tourism_select, df_list)
  # })
  # 
  
  
  
  
  observeEvent(input$tourism_select, {
    if (input$tourism_select!=TOURISM_QUESTIONS[1]) {
      shinyjs::show('tourism_previous_btn')
    }
    if (input$tourism_select==TOURISM_QUESTIONS[1]) {
      shinyjs::hide('tourism_previous_btn')
    }
    if (input$tourism_select!=TOURISM_QUESTIONS[length(TOURISM_QUESTIONS)]) {
      shinyjs::show('tourism_next_btn')
    }
    if (input$tourism_select==TOURISM_QUESTIONS[length(TOURISM_QUESTIONS)])  { 
      shinyjs::hide('tourism_next_btn')
    }
    
    
  }, ignoreInit=T)
  
  
  
  observeEvent(input$tourism_previous, {
    current <- which(TOURISM_QUESTIONS == input$tourism_select)
    if(current > 1) {
      updateSelectInput(
        session, "tourism_select",
        selected = TOURISM_QUESTIONS[current - 1]
      )
      #update_drilldown_chart(input$tourism_select, df_list, "tourism_chart") 
      #update_drilldown_map(input$tourism_select, df_list, "tourism_map")
      # if (!is.null(input$tourism_currLevel)) {
      # if (input$tourism_currLevel==0) {
      #   #browser()
      # # output$tourism_chart <- NULL
      # # output$tourism_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$tourism_select, df_list, 'tourism', shinybrowser::get_height())
      # # })
      #   # highchartProxy('tourism_chart') %>%
      #   # hcpxy_redraw()
      # }
      # }
      
      
      #shinyjs::html(id = 'tourism_region_headline_text', html=generate_region_headline_text(input$tourism_select, df_list))
      #shinyjs::html(id = 'tourism_region_summary_text', html=generate_region_summary_text(input$tourism_select, df_list, input$tourism_currLevel, reactive__mouseOver_tourism$reg_name,  reactive__mouseOver_tourism$reg_val, reactive__mouseOver_tourism$reg_eng_dif, reactive__mouseOver_tourism$reg_rank))
      
      
      
      #shinyjs::html(id = 'tourism_borough_text', html=generate_borough_text(input$tourism_select, df_list))
      #shinyjs::html(id = 'tourism_region_text', html =  generate_region_text(input$tourism_select, df_list))
    }
  }, ignoreInit=T, ignoreNULL=T, priority=2
  )
  # 
  observeEvent(input$tourism_next, {
    current <- which(TOURISM_QUESTIONS == input$tourism_select)
    if(current < length(TOURISM_QUESTIONS)){
      updateSelectInput(
        session, "tourism_select",
        selected = TOURISM_QUESTIONS[current + 1]
      )
      #input$tourism_currLevel <- 1
      #update_drilldown_chart(input$tourism_select, df_list, "tourism_chart") 
      #update_drilldown_map(input$tourism_select, df_list, "tourism_map")
      #browser()
      # if (!is.null(input$tourism_currLevel)) {
      # if (input$tourism_currLevel==0) {
      # #browser()
      # # output$tourism_chart <- NULL
      # # output$tourism_chart <- renderHighchart({
      # #   generate_drilldown_chart(input$tourism_select, df_list, 'tourism', shinybrowser::get_height())
      # # })
      #   highchartProxy('tourism_chart') %>%
      #   hcpxy_redraw()
      # }#
      # }
      
      
      
      #shinyjs::html(id = 'tourism_region_headline_text', html=generate_region_headline_text(input$tourism_select, df_list))
      #shinyjs::html(id = 'tourism_region_summary_text', html=generate_region_summary_text(input$tourism_select, df_list, input$tourism_currLevel, reactive__mouseOver_tourism$reg_name,  reactive__mouseOver_tourism$reg_val, reactive__mouseOver_tourism$reg_eng_dif, reactive__mouseOver_tourism$reg_rank))
      
      
      
      
      #shinyjs::html(id = 'tourism_borough_text', html=generate_borough_text(input$tourism_select, df_list))
      
    }
  }, ignoreInit=T, ignoreNULL=T, priority=4
  )
  
  # 
  # observeEvent(c(input$tourism_currLevel,input$tourism_currLevelMap), {
  #   if (is.null(input$tourism_currLevel)) {
  #     updateTextOutput(session, 'tourism_subtitle_text')
  #   }
  #    if (is.null(input$tourism_currLevelMap)) {
  #      updateTextOutput(session, 'tourism_subtitle_text')
  #    }
  #   
  #   input$tourism_currLevel
  #   
  # })
  
  observeEvent(input$tourism_currLevel, {
    print('level change')
  })
  
  
  # Need to separate out
  
  
  
  # observeEvent(input$tourism_select, {
  #   if (!is.null(input$tourism_currLevel)) { 
  #     if (input$tourism_currLevel==0) {
  #       shinyjs::runjs(
  #         paste0(
  #           "
  #           var chart = $('#tourism_chart').highcharts();
  #           chart.drillUp();
  #           "
  #         )
  #       )
  #     }
  #   }
  #   if (!is.null(input$tourism_currLevelMap)) { 
  #     if (input$tourism_currLevelMap==0) {
  #       shinyjs::runjs(
  #         paste0(
  #           "
  #           var chart = $('#tourism_map').highcharts();
  #           chart.drillUp();
  #           "
  #         )
  #       )
  #     }
  #   }
  #   
  # }, priority=2)  
  
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  # Can remove  basically all the conditional ifs, since the logic is now stored infctuoin
  # Plus have the drill up jumper above
  observeEvent(c(input$tourism_select, input$tourism_currLevel), { #input$tourism_compOps, input$tourism_currLevel,
    #browser()
    if (is.null(input$tourism_currLevel)==T) {
      update_drilldown_chart(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps)
      update_drilldown_chart_summary(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps)
      update_drilldown_map(input$tourism_select, df_list, "tourism_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    #browser()
    else if (input$tourism_currLevel==1) {
      update_drilldown_chart(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps, input$tourism_currLevel)
      #browser()
      update_drilldown_chart_summary(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps, input$tourism_currLevel)
      update_drilldown_map(input$tourism_select, df_list, "tourism_map", QUESTION_LIST, bounds_region, bounds_borough)
    }
    # else if (input$tourism_currLevel==0) {
    #   #browser()
    #   
    # }
    else {
      update_drilldown_chart(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps)
      update_drilldown_chart_summary(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps, input$tourism_currLevel)
    }
    # observeEvent(input$tourism_currLevel, {
    #   #browser()
    #   update_drilldown_chart(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps, 1)
    #   update_drilldown_chart_summary(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps, 1)
    #   update_drilldown_map(input$tourism_select, df_list, "tourism_map", QUESTION_LIST, bounds_region, bounds_borough)
    # }, ignoreInit=T, ignoreNULL=T)
  }, ignoreInit=T, ignoreNULL=F, priority=0)
  
  observeEvent(c(input$tourism_compOps, input$tourism_currLevel), { #input$tourism_compOps, input$tourism_currLevel,
    update_drilldown_chart_summary(input$tourism_select, df_list, "tourism_chart", input$tourism_compOps, input$tourism_currLevel)
  }, ignoreInit=F, ignoreNULL=F, priority=3)
  
  
  observeEvent(
    c(input$tourism_currLevel), {
      
      #browser()
      req(input$tourism_currLevel)
      selected <- input$tourism_compOps
      if (input$tourism_currLevel==0) {
        updatePrettyCheckboxGroup(
          session, "tourism_compOps",
          choices=c(
            'London mean'='mean',
            'London error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      else {
        updatePrettyCheckboxGroup(
          session, "tourism_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
      }
      
      observeEvent(c(input$tourism_select), {
        updatePrettyCheckboxGroup(
          session, "tourism_compOps",
          choices=c(
            'England mean'='mean',
            'England error (95% CI)'='error'
          ),
          selected=selected,
          inline=T
        )
        
      }, ignoreInit=T, ignoreNULL=T, priority=-4)
    }, ignoreInit=T, ignoreNULL=F, priority=-3
  )
  
  observe({
    print(input$tourism_tab)
  })
  
  observe({
    print(paste0('my level =', input$tourism_currLevelMap))
  })
  
  
  observeEvent(c(input$tourism_currLevel, input$tourism_currLevelMap, input$tourism_tab), {
    
    req(input$tourism_tab) # still don't really understand req() but is required 
    #browser()
    if (input$tourism_tab=='chart') {
      req(input$tourism_currLevel)
      #browser()
      if (input$tourism_currLevel==0) {
        shinyjs::show('tourism-text-drilldown')
      }
      else  {
        shinyjs::hide('tourism-text-drilldown')
      }
      observeEvent(c(input$tourism_select, input$tourism_tab), {
        shinyjs::hide('tourism-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    else {
      #browser()
      #delay(5000,
      req(input$tourism_currLevelMap)
      if (input$tourism_currLevelMap==0) {
        shinyjs::show('tourism-text-drilldown')
      }
      else  {
        shinyjs::hide('tourism-text-drilldown')
      }
      observeEvent(c(input$tourism_select, input$tourism_tab), {
        shinyjs::hide('tourism-text-drilldown')
      }, ignoreInit=T, ignoreNULL=T, priority=-2)
    }
    
    
  }, ignoreInit=T, ignoreNULL=T, priority=-1
  )
  
  
  reactive__countFromToTourism <- reactive({
    question <- as.numeric(input$tourism_select)
    df_region <- df_list[[question ]][['region']][['dataframe']] %>%
      select(region, prop_resp, color, drilldown_central)
    countTo <- df_region$prop_resp[df_region$region=='London']
  })
  
  reactiveVal__countFromToTourism <-  reactiveValues(countFrom=0, countTo=0)
  
  observeEvent( 
    reactive__countFromToTourism(),{
      reactiveVal__countFromToTourism$countFrom <- reactiveVal__countFromToTourism$countTo; 
      #print(reactiveVal__countFromTo$countFrom)
      reactiveVal__countFromToTourism$countTo <- reactive__countFromToTourism()
      #print(reactiveVal__countFromTo$countTo)
    }
  )
  
  reactive__countToTourism <- reactive({req(reactive__countFromToTourism());  reactive__countFromToTourism(); reactiveVal__countFromToTourism$countTo})
  reactive__countFromTourism <- reactive({req(reactive__countFromToTourism()); reactive__countFromToTourism(); reactiveVal__countFromToTourism$countFrom})
  
  
  output$countTo <- renderPrint({reactive__countTo()})
  output$countFrom <- renderPrint({ reactive__countFrom()})
  
  
  observeEvent(
    input$tourism_select,once=T, ignoreNULL=F, ignoreInit=F, {
      insertUI(
        selector = "#countUp-ui-tourism",
        where = "afterEnd",
        ui = div(generate_countUp(reactiveVal__countFromToTourism$countTo, reactiveVal__countFromToTourism$countFrom, 'tourism'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
      )
    }
  )
  
  observeEvent(reactiveVal__countFromToTourism$countTo, {
    countupProxy("countUp-tourism") %>% 
      countup_update(reactiveVal__countFromToTourism$countTo)
  }, priority=8)
  
  #' output$tourism_previous_btn <- renderUI({
  #'   # tooltip(
  #'   actionButton(
  #'     "tourism_previous", 
  #'     "Previous question",
  #'     icon=icon("backward")
  #'   )
  #'   #,
  #'   #'A fucking message'
  #'   #)
  #' })
  #' output$tourism_next_btn <- renderUI({
  #'   actionButton(
  #'     "tourism_next", 
  #'     "Next question",
  #'     icon=icon("forward")
  #'   )
  #' })
  #' 
  #' 
  #' output$tourism_title_chart <- renderText({
  #'   print(paste(df_list[[as.numeric(input$tourism_select)]][['region']][['title']]))
  #' })
  #' output$tourism_title_map <- renderText({
  #'   print(paste(df_list[[as.numeric(input$tourism_select)]][['region']][['title']]))
  #' })
  #' output$tourism_subtitle_chart <- renderText({
  #'   if (is.null(input$tourism_currLevel)) {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #'   else if (input$tourism_currLevel==0) {
  #'     print(paste("Click 'Back to Regions' to drillup" ))
  #'   }
  #'   else {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #' })
  #' output$tourism_subtitle_map <- renderText({
  #'   if (is.null(input$tourism_currLevelMap)) {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #'   else if (input$tourism_currLevelMap==0) {
  #'     print(paste("Click 'Back to Regions' to drillup" ))
  #'   }
  #'   else {
  #'     print(paste("Click to drilldown into London by Borough"))
  #'   }
  #' })
  #' 
  #' 
  #' 
  #' output$tourism_chart <- renderHighchart({
  #'   generate_drilldown_chart(input$tourism_select, df_list, 'tourism', shinybrowser::get_height())
  #' })
  #' output$tourism_map <- renderHighchart({
  #'   generate_drilldown_map(input$tourism_select, df_list, 'tourism', QUESTION_LIST, bounds_region, bounds_borough)
  #' })
  #' 
  #' 
  #' reactive__tourism_select <- reactive({req(input$tourism_select)})
  #' reactive__mouseOver_tourism <- reactiveValues()
  #' observeEvent(c(input$tourism_tab, input$tourism_chart_mouseOver$name,input$tourism_currLevel, input$tourism_map_mouseOver,input$tourism_currLevelMap, input$tourism_select), {
  #'   observeEvent(c(input$tourism_tab, input$tourism_select, input$tourism_currLevel, input$tourism_currLevelMap), {
  #'     reg_name <- 'London'
  #'     reg_val <-
  #'       df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'       mutate(prop_resp=round(prop_resp,1)) %>%
  #'       select(region, prop_resp) %>%
  #'       filter(region=='London') %>%
  #'       select(prop_resp) %>%
  #'       pull(1)
  #'     eng_val <-
  #'       df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'       mutate(mean = round(mean(prop_resp),1)) %>%
  #'       filter(region=='London') %>%
  #'       select(mean) %>%
  #'       pull(1)
  #'     reg_eng_dif <- round(reg_val - eng_val ,1)
  #'     reg_rank <-
  #'       df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'       select(region, prop_resp, rank_direction) %>%
  #'       mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'       filter(region=='London') %>%
  #'       select(rank) %>%
  #'       pull(1)
  #'     
  #'   }, ignoreInit=T, ignoreNULL=T)
  #'   if (input$tourism_tab=='chart') {
  #'     if(is.null(input$tourism_chart_mouseOver$name)) {
  #'       #browser()
  #'       reg_name <- 'London'
  #'       reg_val <-
  #'         df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'         mutate(prop_resp=round(prop_resp,1)) %>%
  #'         select(region, prop_resp) %>%
  #'         filter(region=='London') %>%
  #'         select(prop_resp) %>%
  #'         pull(1)
  #'       eng_val <-
  #'         df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'         mutate(mean = round(mean(prop_resp),1)) %>%
  #'         filter(region=='London') %>%
  #'         select(mean) %>%
  #'         pull(1)
  #'       reg_eng_dif <- round(reg_val - eng_val ,1)
  #'       reg_rank <-
  #'         df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'         select(region, prop_resp, rank_direction) %>%
  #'         mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'         filter(region=='London') %>%
  #'         select(rank) %>%
  #'         pull(1)
  #'       #browser()
  #'     }
  #'     # On mouseOver
  #'     else {
  #'       
  #'       if (is.null(input$tourism_currLevel)) {
  #'         reg_name <- input$tourism_chart_mouseOver$name
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$tourism_chart_mouseOver$name) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$tourism_chart_mouseOver$name) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$tourism_chart_mouseOver$name) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'         #browser()
  #'       }
  #'       else if (input$tourism_currLevel==1) {
  #'         reg_name <- input$tourism_chart_mouseOver$name
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$tourism_chart_mouseOver$name) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$tourism_chart_mouseOver$name) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$tourism_chart_mouseOver$name) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'         # TODO TRY HERE!!!!!!!!
  #'         #browser()
  #'       }
  #'       else {
  #'         reg_name <- 'London'
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region=='London') %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region=='London') %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region=='London') %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'     }
  #'   }
  #'   if (input$tourism_tab=='map') {
  #'     if(is.null(input$tourism_map_mouseOver)) {
  #'       # browser()
  #'       reg_name <- 'London'
  #'       reg_val <-
  #'         df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'         mutate(prop_resp=round(prop_resp,1)) %>%
  #'         select(region, prop_resp) %>%
  #'         filter(region=='London') %>%
  #'         select(prop_resp) %>%
  #'         pull(1)
  #'       eng_val <-
  #'         df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'         mutate(mean = round(mean(prop_resp),1)) %>%
  #'         filter(region=='London') %>%
  #'         select(mean) %>%
  #'         pull(1)
  #'       reg_eng_dif <- round(reg_val - eng_val ,1)
  #'       reg_rank <-
  #'         df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'         select(region, prop_resp, rank_direction) %>%
  #'         mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'         filter(region=='London') %>%
  #'         select(rank) %>%
  #'         pull(1)
  #'     }
  #'     # On mouseOver
  #'     else {
  #'       if (is.null(input$tourism_currLevelMap)) {
  #'         #browser()
  #'         reg_name <- input$tourism_map_mouseOver
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$tourism_map_mouseOver) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$tourism_map_mouseOver) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$tourism_map_mouseOver) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'       else if (input$tourism_currLevelMap==1) {
  #'         #browser()
  #'         reg_name <- input$tourism_map_mouseOver
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region==input$tourism_map_mouseOver) %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region==input$tourism_map_mouseOver) %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         #browser()
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region==input$tourism_map_mouseOver) %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'       else {
  #'         reg_name <- 'London'
  #'         reg_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(prop_resp=round(prop_resp,1)) %>%
  #'           select(region, prop_resp) %>%
  #'           filter(region=='London') %>%
  #'           select(prop_resp) %>%
  #'           pull(1)
  #'         eng_val <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           mutate(mean = round(mean(prop_resp),1)) %>%
  #'           filter(region=='London') %>%
  #'           select(mean) %>%
  #'           pull(1)
  #'         reg_eng_dif <- round(reg_val - eng_val ,1)
  #'         reg_rank <-
  #'           df_list[[as.numeric(reactive__tourism_select())]][['region']][['dataframe']] %>%
  #'           select(region, prop_resp, rank_direction) %>%
  #'           mutate(rank = case_when(rank_direction=='D'~scales::ordinal(rank(desc(round(prop_resp,1)), ties.method='min')), T~scales::ordinal(rank(round(prop_resp,1), ties.method='min')))) %>%
  #'           filter(region=='London') %>%
  #'           select(rank) %>%
  #'           pull(1)
  #'       }
  #'     }
  #'   }
  #'   reactive__mouseOver_tourism$reg_name <- reg_name
  #'   reactive__mouseOver_tourism$reg_val <- reg_val
  #'   reactive__mouseOver_tourism$reg_eng_dif <- reg_eng_dif
  #'   reactive__mouseOver_tourism$reg_rank <- reg_rank
  #'   # browser()
  #'   # print('yo')
  #' }, ignoreNULL=F, ignoreInit=F
  #' )
  #' 
  #' 
  #' output$tourism_region_headline_text <- renderUI({
  #'   generate_region_headline_text(input$tourism_select, df_list)
  #' })
  #' output$tourism_region_summary_text <- renderUI({
  #'   generate_region_summary_text(input$tourism_select, df_list, input$tourism_currLevel, reactive__mouseOver_tourism$reg_name,  reactive__mouseOver_tourism$reg_val, reactive__mouseOver_tourism$reg_eng_dif, reactive__mouseOver_tourism$reg_rank)
  #' })
  #' 
  #' output$tourism_borough_text <- renderUI({
  #'   generate_borough_text(input$tourism_select, df_list)
  #' })
  #' 
  #' 
  #' 
  #' 
  #' 
  #' observeEvent(input$tourism_select, {
  #'   if (input$tourism_select!=TOURISM_QUESTIONS[1]) {
  #'     shinyjs::show('tourism_previous_btn')
  #'   }
  #'   if (input$tourism_select==TOURISM_QUESTIONS[1]) {
  #'     shinyjs::hide('tourism_previous_btn')
  #'   }
  #'   if (input$tourism_select!=TOURISM_QUESTIONS[length(TOURISM_QUESTIONS)]) {
  #'     shinyjs::show('tourism_next_btn')
  #'   }
  #'   if (input$tourism_select==TOURISM_QUESTIONS[length(TOURISM_QUESTIONS)])  { 
  #'     shinyjs::hide('tourism_next_btn')
  #'   }
  #'   
  #'   
  #' }, ignoreInit=T)
  #' 
  #' 
  #' 
  #' observeEvent(input$tourism_previous, {
  #'   current <- which(TOURISM_QUESTIONS == input$tourism_select)
  #'   if(current > 1) {
  #'     updateSelectInput(
  #'       session, "tourism_select",
  #'       selected = TOURISM_QUESTIONS[current - 1]
  #'     )
  #'     #update_drilldown_chart(input$tourism_select, df_list, "tourism_chart") 
  #'     update_drilldown_map(input$tourism_select, df_list, "tourism_map")
  #'     generate_region_text(input$tourism_select, df_list)
  #'     shinyjs::html(id = 'tourism_region_text', html =  generate_region_text(input$tourism_select, df_list))
  #'   }
  #' }, ignoreInit=T, ignoreNULL=T, priority=2
  #' )
  #' # 
  #' observeEvent(input$tourism_next, {
  #'   current <- which(TOURISM_QUESTIONS == input$tourism_select)
  #'   if(current < length(TOURISM_QUESTIONS)){
  #'     updateSelectInput(
  #'       session, "tourism_select",
  #'       selected = TOURISM_QUESTIONS[current + 1]
  #'     )
  #'     #update_drilldown_chart(input$tourism_select, df_list, "tourism_chart") 
  #'     update_drilldown_map(input$tourism_select, df_list, "tourism_map")
  #'     shinyjs::html(id = 'tourism_region_text', html = generate_region_text(input$tourism_select, df_list))
  #'   }
  #' }, ignoreInit=T, ignoreNULL=T, priority=2
  #' )
  #' 
  #' # 
  #' # observeEvent(c(input$tourism_currLevel,input$tourism_currLevelMap), {
  #' #   if (is.null(input$tourism_currLevel)) {
  #' #     updateTextOutput(session, 'tourism_subtitle_text')
  #' #   }
  #' #    if (is.null(input$tourism_currLevelMap)) {
  #' #      updateTextOutput(session, 'tourism_subtitle_text')
  #' #    }
  #' #   
  #' #   input$tourism_currLevel
  #' #   
  #' # })
  #' 
  #' observeEvent(input$tourism_currLevel, {
  #'   print('level change')
  #' })
  #' 
  #' 
  #' observeEvent(c(input$tourism_compOps, input$tourism_currLevel, input$tourism_select), {
  #'   
  #'   # drillup event bug!!!
  #'   # https://github.com/blacklabel/custom_events/issues/139
  #'   
  #'   
  #'   
  #'   print(paste0('Current drilldown level: ',input$tourism_currLevel))
  #'   # print(input$tourism_currLevel)
  #'   question <- as.numeric(input$tourism_select)
  #'   # print(question)
  #'   df_region_central <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp, num_resp, color, drilldown_central)
  #'   df_region_error <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp_lb, num_resp, prop_resp_ub, drilldown_error)
  #'   df_borough <- df_list[[question ]][['borough']][['dataframe']]
  #'   if ('error'%in%input$tourism_compOps & 'mean'%ni%input$tourism_compOps) { #delay(110)
  #'     #browser()
  #'     if (is.null(input$tourism_currLevel)) {
  #'       #browser()
  #'       #browser()
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'England',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$tourism_currLevel==0) { # drill level
  #'       #browser()
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_borough$prop_resp_lb),",
  #'                     to:",mean(df_borough$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'London',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'            
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00'',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else { # top level
  #'       #browser()
  #'       
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98,
  #'                     label: {
  #'                       text: 'England',
  #'                       verticalAlign: 'top',
  #'                       textAlign: 'center',
  #'                       rotation:0,
  #'                       y:-4,
  #'                       style: {
  #'                           color:'#d82222',
  #'                           fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'                       }
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'   }
  #'   else if ('mean'%in%input$tourism_compOps & 'error'%ni%input$tourism_compOps) {
  #'     #browser()
  #'     if (is.null(input$tourism_currLevel)) {
  #'       #browser()
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       # delay(0,
  #'       #       shinyjs::runjs(
  #'       #         paste0(
  #'       #           "
  #'       #         var chart = $('#tourism_chart').highcharts();
  #'       #           chart.yAxis[0].update({plotLines:
  #'       #             [{
  #'       #               value:",mean(df_region_central$prop_resp),",
  #'       #               color:'#d82222',
  #'       #               zIndex:99,
  #'       #               label: {
  #'       #                 text: 'England',
  #'       #                 verticalAlign: 'top',
  #'       #                 textAlign: 'center',
  #'       #                 rotation:0,
  #'       #                 y:-4,
  #'       #                 style: {
  #'       #                     color:'#d82222',
  #'       #                     fontWeight: 'normal',
  #'       #                     fontSize: '1.35vh'
  #'       #                 }
  #'       #               }
  #'       #             }]
  #'       #           });
  #'       #         console.log(chart);
  #'       #      "
  #'       #         )
  #'       #       )
  #'       # )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$tourism_currLevel==0) { # Borough level
  #'       #browser()
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_borough$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'London',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$tourism_currLevel==1) { # top level
  #'       #browser()
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else {
  #'       #   update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       #   delay(300,
  #'       #         shinyjs::runjs(
  #'       #           paste0(
  #'       #             "
  #'       #           var chart = $('#tourism_chart').highcharts();
  #'       #             chart.yAxis[0].update({plotLines:
  #'       #               [{
  #'       #                 value:",mean(df_region_central$prop_resp),",
  #'       #                 color:'#d82222',
  #'       #                 zIndex:99,
  #'       #                 label: {
  #'       #                   text: 'England',
  #'       #           verticalAlign: 'top',
  #'       #           textAlign: 'center',
  #'       #           rotation:0,
  #'       #           y:-4,
  #'       #           style: {
  #'       #               color:'#d82222',
  #'       #               fontWeight: 'normal'
  #'       #           }
  #'       #           
  #'       #                 }
  #'       #               }]
  #'       #             });
  #'       #           console.log(chart);
  #'       #        "
  #'       #           )
  #'       #         )
  #'       #   )
  #'       #   delay(300,
  #'       #         shinyjs::runjs(
  #'       #           paste0(
  #'       #             "
  #'       #           var chart = $('#tourism_chart').highcharts();
  #'       #             chart.yAxis[0].update({plotBands:
  #'       #               [{
  #'       #                 from:0,
  #'       #                 to:0,
  #'       #                 color:'#d822221F',
  #'       #                 zIndex:98
  #'       # 
  #'       #               }]
  #'       #             });
  #'       #           console.log(chart);
  #'       #        "
  #'       #           )
  #'       #         )
  #'       #   )
  #'     }
  #'     
  #'   }
  #'   
  #'   else if ('mean'%in%input$tourism_compOps & 'error'%in%input$tourism_compOps) {
  #'     if (is.null(input$tourism_currLevel)) {
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'     else if (input$tourism_currLevel==0) {
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_borough$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'London',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_borough$prop_resp_lb),",
  #'                     to:",mean(df_borough$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       
  #'     }
  #'     else{
  #'       update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:",mean(df_region_central$prop_resp),",
  #'                     color:'#d82222',
  #'                     zIndex:99,
  #'                     label: {
  #'                       text: 'England',
  #'               verticalAlign: 'top',
  #'               textAlign: 'center',
  #'               rotation:0,
  #'               y:-4,
  #'               style: {
  #'                   color:'#d82222',
  #'                   fontWeight: 'normal',
  #'                           fontSize: '1.35vh'
  #'               }
  #'               
  #'                     }
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'       delay(300,
  #'             shinyjs::runjs(
  #'               paste0(
  #'                 "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:",mean(df_region_error$prop_resp_lb),",
  #'                     to:",mean(df_region_error$prop_resp_ub),",
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'               )
  #'             )
  #'       )
  #'     }
  #'   }
  #'   else {
  #'     update_drilldown_chart(input$tourism_select, df_list, "tourism_chart")
  #'     delay(300,
  #'           shinyjs::runjs(
  #'             "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotBands:
  #'                   [{
  #'                     from:0,
  #'                     to:0,
  #'                     color:'#d822221F',
  #'                     zIndex:98
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'           )
  #'     )
  #'     delay(300,
  #'           shinyjs::runjs(
  #'             "
  #'               var chart = $('#tourism_chart').highcharts();
  #'                 chart.yAxis[0].update({plotLines:
  #'                   [{
  #'                     value:0,
  #'                     color:'#ffffff00',
  #'                     zIndex:99
  #' 
  #'                   }]
  #'                 });
  #'               console.log(chart);
  #'            "
  #'           )
  #'     )
  #'     
  #'     #browser()
  #'   }
  #' }, ignoreInit=F, ignoreNULL=F, priority=0)
  #' 
  #' 
  #' 
  #' observeEvent(
  #'   c(input$tourism_currLevel), {
  #'     
  #'     #browser()
  #'     req(input$tourism_currLevel)
  #'     selected <- input$tourism_compOps
  #'     if (input$tourism_currLevel==0) {
  #'       updatePrettyCheckboxGroup(
  #'         session, "tourism_compOps",
  #'         choices=c(
  #'           'London mean'='mean',
  #'           'London error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'     }
  #'     else {
  #'       updatePrettyCheckboxGroup(
  #'         session, "tourism_compOps",
  #'         choices=c(
  #'           'England mean'='mean',
  #'           'England error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'     }
  #'     
  #'     observeEvent(c(input$tourism_select), {
  #'       updatePrettyCheckboxGroup(
  #'         session, "tourism_compOps",
  #'         choices=c(
  #'           'England mean'='mean',
  #'           'England error (95% CI)'='error'
  #'         ),
  #'         selected=selected,
  #'         inline=T
  #'       )
  #'       
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-4)
  #'   }, ignoreInit=T, ignoreNULL=F, priority=-3
  #' )
  #' 
  #' observe({
  #'   print(input$tourism_tab)
  #' })
  #' 
  #' observe({
  #'   print(paste0('my level =', input$tourism_currLevelMap))
  #' })
  #' 
  #' 
  #' observeEvent(c(input$tourism_currLevel, input$tourism_currLevelMap, input$tourism_tab), {
  #'   
  #'   req(input$tourism_tab) # still don't really understand req() but is required 
  #'   #browser()
  #'   if (input$tourism_tab=='chart') {
  #'     req(input$tourism_currLevel)
  #'     #browser()
  #'     if (input$tourism_currLevel==0) {
  #'       shinyjs::show('tourism-text-drilldown')
  #'     }
  #'     else  {
  #'       shinyjs::hide('tourism-text-drilldown')
  #'     }
  #'     observeEvent(c(input$tourism_select, input$tourism_tab), {
  #'       shinyjs::hide('tourism-text-drilldown')
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-2)
  #'   }
  #'   
  #'   else {
  #'     #browser()
  #'     #delay(5000,
  #'     req(input$tourism_currLevelMap)
  #'     if (input$tourism_currLevelMap==0) {
  #'       shinyjs::show('tourism-text-drilldown')
  #'     }
  #'     else  {
  #'       shinyjs::hide('tourism-text-drilldown')
  #'     }
  #'     observeEvent(c(input$tourism_select, input$tourism_tab), {
  #'       shinyjs::hide('tourism-text-drilldown')
  #'     }, ignoreInit=T, ignoreNULL=T, priority=-2)
  #'   }
  #'   
  #'   
  #' }, ignoreInit=T, ignoreNULL=T, priority=-1
  #' )
  #' 
  #' 
  #' reactive__countFromToTourism <- reactive({
  #'   question <- as.numeric(input$tourism_select)
  #'   df_region <- df_list[[question ]][['region']][['dataframe']] %>%
  #'     select(region, prop_resp, color, drilldown_central)
  #'   countTo <- df_region$prop_resp[df_region$region=='London']
  #' })
  #' 
  #' reactiveVal__countFromToTourism <-  reactiveValues(countFrom=0, countTo=0)
  #' 
  #' observeEvent( 
  #'   reactive__countFromToTourism(),{
  #'     reactiveVal__countFromToTourism$countFrom <- reactiveVal__countFromToTourism$countTo; 
  #'     #print(reactiveVal__countFromTo$countFrom)
  #'     reactiveVal__countFromToTourism$countTo <- reactive__countFromToTourism()
  #'     #print(reactiveVal__countFromTo$countTo)
  #'   }
  #' )
  #' 
  #' reactive__countToTourism <- reactive({req(reactive__countFromToTourism());  reactive__countFromToTourism(); reactiveVal__countFromToTourism$countTo})
  #' reactive__countFromTourism <- reactive({req(reactive__countFromToTourism()); reactive__countFromToTourism(); reactiveVal__countFromToTourism$countFrom})
  #' 
  #' 
  #' output$countTo <- renderPrint({reactive__countTo()})
  #' output$countFrom <- renderPrint({ reactive__countFrom()})
  #' 
  #' 
  #' observeEvent(
  #'   input$tourism_select,once=T, ignoreNULL=F, ignoreInit=F, {
  #'     insertUI(
  #'       selector = "#countUp-ui-tourism",
  #'       where = "afterEnd",
  #'       ui = div(generate_countUp(reactiveVal__countFromToTourism$countTo, reactiveVal__countFromToTourism$countFrom, 'tourism'),style="color:#ffffff; font-size:4.8vw; line-height:4.8vw;")
  #'     )
  #'   }
  #' )
  #' 
  #' observeEvent(reactiveVal__countFromToTourism$countTo, {
  #'   countupProxy("countUp-tourism") %>% 
  #'     countup_update(reactiveVal__countFromToTourism$countTo)
  #' })
  #' 
   
}

# Run the application 
shinyApp(ui = ui, server = server)
