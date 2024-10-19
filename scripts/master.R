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
library(shinyglide)

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





#'[____________________________________________________________________________]

if (DOWNLOAD_LATEST_DATA==T) {
  
}

output_list <- lapply(
  1:length(REGION_QUESTION_LIST[[1]]), function(q) {
    #print(q)
    generate_regional_plot(
      SURVEY_PATH, RELEASE_YEAR,
      list('code'=REGION_QUESTION_LIST[[1]][q],'theme'=REGION_QUESTION_LIST[[2]][q], 'color'=REGION_QUESTION_LIST[[3]][q])
    )
  }
)







