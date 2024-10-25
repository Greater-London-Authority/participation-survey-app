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
setwd("C:/Users/Matt/Documents/participation-survey-gla")
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
      df_borough <- generate_borough_frame(
        SURVEY_PATH, 
        RELEASE_YEAR,
        list('code'=QUESTION_LIST[['borough']][['code']][[q]],'theme'=QUESTION_LIST[['borough']][['theme']][q], 'color'=QUESTION_LIST[['borough']][['color']][q])
      )
      return(list('region'=df_region, 'borough'=df_borough))
    }
    else {
      return(list('region'=df_region))
    }
  }
)
################################################################################



ui <- fluidPage(
  radioButtons(
    "radio", "Options",
    c("Original" = "og",
      "Update with Borough" = "wboro",
      "Update no borough" = "nboro"
      
    )
  ),
  highchartOutput('plot', height='75vh')
)



server <- function(input, output) {
  
 output$plot <- renderHighchart({
    df_region <- df_list[[1]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~'')) 
    subtitle <-  df_list[[1]][['region']][['title']]
    df_borough <- df_list[[1]][['borough']][['dataframe']] 
    
    
    list1 <- toString(
      lapply(
        1:nrow(df_borough), function(n) {
          paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp'],"]")
        }
      )
    )
    list2 <- toString(
      lapply(
        1:nrow(df_borough), function(n) {
          paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp_lb'],",",df_borough[n,'prop_resp_ub'],"]")
        }
      )
    )
    
    highchart() %>%
      hc_chart(spacingBottom= 50) %>%
      hc_add_series(
        id='central',
        type='bar', 
        name='Regions',
        showInLegend=F,
        data=df_region, 
        hcaes(x = factor(region), y = round(prop_resp,1), color=color)
      ) %>%
      hc_add_series(
        id='confidence',
        type='errorbar',
        name='Lower-Upper estimate',
        data=df_region,
        hcaes(x=region, low=prop_resp_lb, high=prop_resp_ub)
      ) %>%
      hc_xAxis(
        type='category',
        title=list(enabled=F),
        labels = list(
          align='left',
          reserveSpace=T,
          style=list(
            fontSize='2vh',
            color='#9b9b9b',
            fontFamily = "Arial",
            fontWeight='300'
          )
        )
      ) %>%
      hc_yAxis(
        title =list(enabled=F),
        gridLineWidth=0,
        tickInterval=10,
        labels=list(
          format="{value}%",
          style=list(
            fontSize='2vh',
            color='#9b9b9b',
            fontFamily = "Arial",
            fontWeight='300'
          )
        )
      ) %>%
      hc_title(
        text='',
        align='left',
        style = list(
          fontSize ="3.2vh",color = "#333333", 
          fontFamily = "Arial", fontWeight = "600"
        )
      ) %>%
      hc_subtitle(
        text=subtitle,
        align='left',
        style = list(
          fontSize ="2.4vh",color = "#333333", 
          fontFamily = "Arial", fontWeight = "350"
        )
      ) %>%
      hc_credits(
        enabled=T,
        useHTML=T,
        text='Chart: <a href="https://data.london.gov.uk/" style="color:#9b9b9b; text-decoration: underline;" >GLA City Intelligence</a>&#x2022 Source: <a href="https://www.gov.uk/government/collections/participation-survey-statistical-releases/" style="color:#9b9b9b; text-decoration: underline;">Participation Survey 2023/24</a>&#x2022',
        position=list(
          align='left',
          x=10,
          y=-10 
        ),
        style =list(
          fontSize='1.7vh',
          color='#9b9b9b'
        )
      ) %>%
      hc_plotOptions(  #48-45
        bar = list(
          pointWidth=52
        ),
        errorbar=list(
          stemWidth= 1,
          whiskerWidth=1,
          whiskerLength= 50
        )
      ) %>%
      hc_tooltip(
        valueSuffix= '%',
        borderWidth=2.6,
        style=list(fontSize='1.35vh'),
        shape='callout',
        shared=T,
        useHTML = TRUE#,
        #headerFormat = ""#,
        #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
      ) %>%
      # hc_drilldown(
      #   allowPointDrilldown = TRUE,
      #   series = list(
      #       list(
      #       name ='London',
      #       id='london',
      #       data = list_parse2(
      #         data.frame(
      #           'x'=factor(df_borough$borough),
      #           'y'=df_borough$prop_resp,
      #           'color'='#6da7de',
    #           'low'=df_borough$prop_resp_lb,
    #           'high'=df_borough$prop_resp_ub
    #           
    #         )
    #       ),
    #       type = 'bar'
    #     )
    #     
    #   )
    # ) %>%
    hc_chart(events = list(
      drilldown = JS(paste0(
        "function(e) {
        if(!e.seriesOptions){
        var chart=this,
        drilldowns={
          'London':{
            name:'London',
            id: 'london',
            data:[",list1,"],
            type:'bar',
            pointWidth:20
          },
          'London2':{
            name:'London2',
            id: 'london2',
            color: '#000000',
            data:[",list2,"],
            type:'errorbar'
          }
      },

      series=[drilldowns[e.point.name],drilldowns[e.point.name+'2']];
      chart.addSingleSeriesAsDrilldown(e.point,series[0]);
      chart.addSingleSeriesAsDrilldown(e.point,series[1]);
      chart.applyDrilldown()
      }}"
      )
      )
    ))
  })
 
 
 observeEvent(input$radio, {
   
   if (input$radio=='wboro') {
     #browser()
     df_region <- df_list[[2]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~'')) 
     subtitle <-  df_list[[2]][['region']][['title']]
     df_borough <- df_list[[2]][['borough']][['dataframe']] 
     # list1new <- toString(
     #   lapply(
     #     1:nrow(df_borough), function(n) {
     #       paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp'],"]")
     #     }
     #   )
     # )
     # list2new <- toString(
     #   lapply(
     #     1:nrow(df_borough), function(n) {
     #       paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp_lb'],",",df_borough[n,'prop_resp_ub'],"]")
     #     }
     #   )
     # )
     highchartProxy("plot") %>%
       hcpxy_update_series(
         id = "central",
         data=df_region$prop_resp
       ) %>%
       hcpxy_update_series(
         id = "confidence",
         data=list_parse2(
           data.frame(
             low=df_region$prop_resp_lb,
             high=df_region$prop_resp_ub
           )
         )
       ) %>%
       hcpxy_update_series(
         id='london',
         data=df_borough$prop_resp
       ) %>%
       hcpxy_update_series(
         id='london2',
         data=list_parse2(
           data.frame(
             low=df_region$prop_resp_lb,
             high=df_region$prop_resp_ub
           )
         )
       ) 
   }
   
   
   if (input$radio=='nboro') {
     #browser()
     df_region <- df_list[[11]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~'')) 
     subtitle <-  df_list[[11]][['region']][['title']]
     df_borough <- df_list[[11]][['borough']][['dataframe']] 
     # list1new <- toString(
     #   lapply(
     #     1:nrow(df_borough), function(n) {
     #       paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp'],"]")
     #     }
     #   )
     # )
     # list2new <- toString(
     #   lapply(
     #     1:nrow(df_borough), function(n) {
     #       paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp_lb'],",",df_borough[n,'prop_resp_ub'],"]")
     #     }
     #   )
     # )
     highchartProxy("plot") %>%
       hcpxy_update_series(
         id = "central",
         data=df_region$prop_resp
       ) %>%
       hcpxy_update_series(
         id = "confidence",
         data=list_parse2(
           data.frame(
             low=df_region$prop_resp_lb,
             high=df_region$prop_resp_ub
           )
         )
       ) #%>%
       hcpxy_update_series(
         id='london',
         data=rep(0, 33)
       ) %>%
       hcpxy_update_series(
         id='london2',
         data= data=list_parse2(
           data.frame(
             low=rep(0,33),
             high=rep(0,33)
           )
         )
       )
   }
   
   if (input$radio=='og') {
     df_region <- df_list[[1]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~'')) 
     subtitle <-  df_list[[1]][['region']][['title']]
     df_borough <- df_list[[1]][['borough']][['dataframe']] 
     
     highchartProxy("plot") %>%
       hcpxy_update_series(
         id = "central",
         data=df_region$prop_resp
       ) %>%
       hcpxy_update_series(
         id = "confidence",
         data=list_parse2(
           data.frame(
             low=df_region$prop_resp_lb,
             high=df_region$prop_resp_ub
           )
         )
       ) %>%
       hcpxy_update_series(
         id='london',
         data=df_borough$prop_resp
       ) %>%
       hcpxy_update_series(
         id='london2',
         data=list_parse2(
           data.frame(
             low=df_region$prop_resp_lb,
             high=df_region$prop_resp_ub
           )
         )
       ) 
   }
   
 }, ignoreInit=T, once=F)
  
 
 
 
 
 
 
 
 
 
 
 
 
 
}

shinyApp(ui, server)