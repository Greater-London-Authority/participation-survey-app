#'[Script]#'*process-2_2_2_6.R*
#'[Project]#'*participation-survey-gla (https://github.com/mawtee/participation-survey-gla)*
#'[Author]#'*M. Tibbles*
#'[Last Update]#'*16/10/2024*
#'[Description]#'*This blah *
#'[Libraries]
rm(list=ls())
gc()
library(shiny)
if(!require("shiny"))
  install.packages("shiny")
if(!require("tidyverse"))
  install.packages("tidyverse")
if(!require("rvest"))
  install.packages("rvest")
if(!require("readxl"))
  install.packages("readxl")
if(!require("readODS"))
  install.packages("readODS")
if(!require("tidyverse"))
  install.packages("tidyverse")
if(!require("geojsonio"))
  install.packages("geojsonio")
if(!require("gglaplot"))
  install.packages("gglaplot")
if(!require("highcharter"))
  install.packages("highcharter")
if(!require("here"))
  install.packages("here")
if(!require("shinyjs"))
  install.packages("shinyjs")
if(!require("bslib"))
  install.packages("bslib")
if(!require("shinyWidgets"))
  install.packages("shinyWidgets")
if(!require("shinycssloaders"))
  install.packages("shinycssloaders")
if(!require("reticulate"))
  install.packages("reticulate")

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

# if (DOWNLOAD_LATEST_DATA==T) {
#   
# }

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
bounds_region <- geojsonio::geojson_read(BOUNDS_REGION_PATH, what = "list")
bounds_borough <- geojsonio::geojson_read(BOUNDS_BOROUGH_PATH, what = "list")
