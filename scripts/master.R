#'[Script]#'*process-2_2_2_6.R*
#'[Project]#'*participation-survey-gla (https://github.com/mawtee/participation-survey-gla)*
#'[Author]#'*M. Tibbles*
#'[Last Update]#'*16/10/2024*
#'[Description]#'*This blah *
#'[Libraries]
rm(list=ls())
gc()
library(tidyverse)
library(rvest)
library(readxl)
library(readODS)
library(tidyverse)
library(geojsonio)
library(gglaplot)
library(highcharter)
library(here)
library(shinyjs)


#'[Source Paths]
source('scripts/funs.R')
#'[Global Options]
#'
DOWNLOAD_LATEST_DATA <- F

SURVEY_PATH <- "data/Final_Revised_DCMS_Participation_Survey_annual_23-24_data_tables__Oct_2024_.ods"
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

#'[____________________________________________________________________________]

if (DOWNLOAD_LATEST_DATA==T) {
  
}

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


df_list <- lapply(
  1:length(QUESTION_LIST[['region']][['code']]), function(q) {
    
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
      df_region$drilldown_error <- ''
      return(list('region'=df_region))
    }
  }
)


# 
# 
# test <- df_list[[1]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~''))
# 
# 
# 
# 
# 
# highchart() %>%
#   hc_chart(spacingBottom= 50) %>%
#   hc_add_series(
#     id='central',
#     type='bar', 
#     name='Regions',
#     showInLegend=F,
#     data=df_plot, 
#     hcaes(name=region, x = factor(region), y = round(prop_resp,1), color=color)
#   ) %>%
#   hc_add_series(
#     id='confidence',
#     type='errorbar',
#     name='Lower-Upper estimate',
#     data=df_plot,
#     hcaes(x=region, low=prop_resp_lb, high=prop_resp_ub)
#   ) %>%
#   hc_xAxis(
#     type='category',
#     title=list(enabled=F),
#     labels = list(
#       align='left',
#       reserveSpace=T,
#       style=list(
#         fontSize='2vh',
#         color='#9b9b9b',
#         fontFamily = "Arial",
#         fontWeight='300'
#       )
#     )
#   ) %>%
#   hc_yAxis(
#     title =list(enabled=F),
#     gridLineWidth=0,
#     tickInterval=10,
#     labels=list(
#       format="{value}%",
#       style=list(
#         fontSize='2vh',
#         color='#9b9b9b',
#         fontFamily = "Arial",
#         fontWeight='300'
#       )
#     )
#   ) %>%
#   hc_title(
#     text=title,
#     align='left',
#     style = list(
#       fontSize ="3.2vh",color = "#333333", 
#       fontFamily = "Arial", fontWeight = "600"
#     )
#   ) %>%
#   hc_subtitle(
#     text=subtitle,
#     align='left',
#     style = list(
#       fontSize ="2.4vh",color = "#333333", 
#       fontFamily = "Arial", fontWeight = "350"
#     )
#   ) %>%
#   hc_credits(
#     enabled=T,
#     useHTML=T,
#     text='Chart: <a href="https://data.london.gov.uk/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence</a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
#     position=list(
#       align='left',
#       x=10,
#       y=-10 
#     ),
#     style =list(
#       fontSize='1.7vh',
#       color='#9b9b9b'
#     )
#   ) %>%
#   hc_plotOptions(  #48-45
#     bar = list(
#       pointWidth=52
#     ),
#     errorbar=list(
#       stemWidth= 1,
#       whiskerWidth=1,
#       whiskerLength= 50
#     )
#   ) %>%
#   hc_tooltip(
#     valueSuffix= '%',
#     borderWidth=2.6,
#     style=list(fontSize='1.35vh'),
#     shape='callout',
#     shared=T,
#     useHTML = TRUE#,
#     #headerFormat = ""#,
#     #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
#   ) %>%
#   hc_drilldown(
#     allowPointDrilldown = TRUE,
#     series = list(
#       list(
#         name ='London',
#         id='london',
#         data = list_parse2(
#           data.frame(
#             name = c("b1", "b2", "b3", "b4", "b5"),
#             value = c(4, 3, 1, 2, 1)
#           )
#         ),
#         type = 'bar',
#         keys = list('name', 'value', 'drilldown')
#       )
#     )
#   )
# 
# 
# 
# 
# 






