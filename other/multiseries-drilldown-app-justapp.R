

ui <- fluidPage(
  radioButtons(
    "radio", "Options",
    c("Original" = "og",
      "Update with Borough" = "wboro",
      "Update no borough" = "nboro"
      
    )
  ),
  highchartOutput('plot', height='75vh')#,
  #textOutput('text')
  
)



server <- function(input, output) {
  
  output$plot <- renderHighchart({
    
    df_region_central <- df_list[[1]][['region']][['dataframe']] %>% 
      mutate(drilldown=case_when(region=='London'~'london-central',T~'')) %>%
      select(region, prop_resp, color, drilldown)
    df_region_error <- df_list[[1]][['region']][['dataframe']] %>% 
      mutate(drilldown=case_when(region=='London'~'london-error',T~'')) %>%
      select(region, prop_resp_lb, prop_resp_ub, drilldown)
    subtitle <-  df_list[[1]][['region']][['title']]
    df_borough <- df_list[[1]][['borough']][['dataframe']] 
    
    highchart() %>%
      hc_chart(spacingBottom= 50) %>%
      hc_add_series(
        name='Central estimate',
        id='region-central',
        type='bar', 
        showInLegend=F,
        data=df_region_central, 
        hcaes(x = factor(region), y = round(prop_resp,1), color=color, drilldown=drilldown)
      ) %>%
      hc_add_series(
        name='Lower-Upper estimate',
        id='region-error',
        type='errorbar',
        
        data=df_region_error,
        hcaes(x=factor(region), low=prop_resp_lb, high=prop_resp_ub, drilldown=drilldown)
      ) %>%
      hc_drilldown(
        allowPointDrilldown=F,
        series = list(
          list(
            id='london-central',
            name='Central estimate',
            type='bar',
            data = list_parse2(
              data.frame(
                borough=df_borough$borough,
                prop_resp=df_borough$prop_resp
              )
            )
          ),
          list(
            id='london-error',
            mame='Lower-Upper estimate',
            type='errorbar',
            data=list_parse2(
              data.frame(
                borough=df_borough$borough,
                prop_resp_lb=df_borough$prop_resp_lb,
                prop_resp_ub=df_borough$prop_resp_ub
              )
              
            )
          )
        )
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
      hc_tooltip(
        valueSuffix= '%',
        borderWidth=2.6,
        style=list(fontSize='1.35vh'),
        shape='callout',
        shared=T,
        useHTML = TRUE
        #headerFormat = ""#,
        #pointFormat = "<span style='font-size:1.6vh; font-weight: normal;'><span style='color:{point.color}'>\u25CF</span> {point.name}</span><br>Central estimate: <b>{point.prop_resp}%</b><br>Lower-Upper estimate: <b>{point.prop_resp_lb}% - {point.prop_resp_ub}%</b>"
      )
    
  })
  
  #https://api.highcharts.com/highcharts/drilldown.breadcrumbs
  #https://api.highcharts.com/class-reference/Highcharts#.BreadcrumbsClickCallbackFunction
  #https://jsfiddle.net/gh/get/library/pure/highcharts/highcharts/tree/master/samples/highcharts/breadcrumbs/single-button
  # try and edit the breadcrumbs
  
  
  observeEvent(input$radio, {
    
    #https://stackoverflow.com/questions/57057061/highcharter-plot-updates-only-after-second-click-r-shiny
    #https://stackoverflow.com/questions/37208989/how-to-know-information-about-the-clicked-bar-in-highchart-column-r-shiny-plot
    
    if (input$radio=='wboro') {
      df_region_central <- df_list[[2]][['region']][['dataframe']] %>% 
        mutate(drilldown=case_when(region=='London'~'london-central',T~'')) %>%
        select(region, prop_resp, color, drilldown)
      df_region_error <- df_list[[2]][['region']][['dataframe']] %>% 
        mutate(drilldown=case_when(region=='London'~'london-error',T~'')) %>%
        select(region, prop_resp_lb, prop_resp_ub, drilldown)
      subtitle <-  df_list[[2]][['region']][['title']]
      df_borough <- df_list[[2]][['borough']][['dataframe']] 

      highchartProxy("plot") %>%
      hcpxy_update_series(
        id='region-central',
        data = df_region_central$prop_resp
      ) %>%
      hcpxy_update_series(
        id='region-error',
        data=list_parse2(
          data.frame(
            prop_resp_lb=df_region_error$prop_resp_lb,
            prop_resp_ub=df_region_error$prop_resp_ub
          )
        )
      ) %>%
      hcpxy_update(
        drilldown=list(
          series=list(
            list(
              id='london-central',
                type='column',
                data=list_parse2(
                  data.frame(
                    borough=df_borough$borough,
                    prop_resp=df_borough$prop_resp
                  )
                )
              ),
              list(
                id='london-error',
                type='errorbar',
                data=list_parse2(
                  data.frame(
                    borough=df_borough$borough,
                    prop_resp_lb=df_borough$prop_resp_lb,
                    prop_resp_ub=df_borough$prop_resp_ub
                    
                  )
                )
                
              )
            )
          )
        )
    }
      
      
      #%>%
        # hcpxy_update_series(
        #   id = "central",
        #   data=df_region_wb$prop_resp
        # ) %>%
        # hcpxy_update_series(
        #   id = "confidence",
        #   data=list_parse2(
        #     data.frame(
        #       low=df_region_wb$prop_resp_lb,
        #       high=df_region_wb$prop_resp_ub
        #     )
        #   )
        # ) %>%
      
      
        # hcpxy_update(
        #   drilldown=list(
        #     series=list(
        #       list(
        #       id='london',
        #       name='A new series',
        #       data=list_parse2(
        #         data.frame(
        #         x=df_borough_wb$borough,
        #         y=df_borough_wb$prop_resp
        #         )
        #       ),
        #       type='bar'
        #       ),
        #       list(
        #         id='london3',
        #         name='A new series2',
        #         data=list_parse2(
        #           data.frame(
        #             x=df_borough_wb$borough,
        #             low=df_borough_wb$prop_resp_lb,
        #             high=df_borough_wb$prop_resp_ub
        #           )
        #         ),
        #         type='errorbar'
        #       )
        #       
        #     )
        #   )
        # ) 
        
        
        
        #%>%
        # hcpxy_update(
        #   drilldown=list(
        #     series=list(
        #       list(
        #         id='london2',
        #         data=list_parse2(
        #           data.frame(
        #             x=df_borough_wb$borough,
        #             low=df_borough_wb$prop_resp_lb,
        #             high=df_borough_wb$prop_resp_ub
        #           )
        #         ),
        #         type='errorbar'
        #       )
        #     )
        #   )
        # ) 
     
        # hcpxy_update(
        #   drilldown = list(
        #     series=list(
        #       id='london',
        #       data=list_parse2(
        #         data.frame(
        #           y=df_borough_wb$prop_resp
        #         )
        #       )
        #         
        #     )
        #   )
        #  ) #%>%
      #   hcpxy_update_series(
      #     id='london2',
      #     data=list_parse2(
      #       data.frame(
      #         low=df_borough_wb$prop_resp_lb,
      #         high=df_borough_wb$prop_resp_ub
      #       )
      #     )
      #   ) %>%
      # hcpxy_redraw()

        #hcpxy_update_series(
        #   id='london',
        #   data=df_borough_wb$prop_resp
        # ) %>%
        # hcpxy_update_series(
        #   id='london2',
        #   data=list_parse2(
        #     data.frame(
        #       low=df_borough_wb$prop_resp_lb,
        #       high=df_borough_wb$prop_resp_ub
        #     )
        #   )
        # )

    
  #   
  #   if (input$radio=='nboro') {
  #     #browser()
  #     df_region_nb <- df_list[[11]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~'')) 
  #     subtitle <-  df_list[[11]][['region']][['title']]
  #     df_borough_nb <- df_list[[11]][['borough']][['dataframe']] 
  #     # list1new <- toString(
  #     #   lapply(
  #     #     1:nrow(df_borough), function(n) {
  #     #       paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp'],"]")
  #     #     }
  #     #   )
  #     # )
  #     # list2new <- toString(
  #     #   lapply(
  #     #     1:nrow(df_borough), function(n) {
  #     #       paste0("['",paste0('b',df_borough[n,'borough']),"',",df_borough[n,'prop_resp_lb'],",",df_borough[n,'prop_resp_ub'],"]")
  #     #     }
  #     #   )
  #     # )
  #     highchartProxy("plot") %>%
  #     
  #       hcpxy_update_series(
  #         id = "central",
  #         data=df_region$prop_resp
  #       ) %>%
  #       hcpxy_update_series(
  #         id = "confidence",
  #         data=list_parse2(
  #           data.frame(
  #             low=df_region_nb$prop_resp_lb,
  #             high=df_region_nb$prop_resp_ub
  #           )
  #         )
  #       ) %>%
  #       hcpxy_update_series(
  #         id='london',
  #         data=rep(0, 33)
  #       ) %>%
  #       hcpxy_update_series(
  #         id='london2',
  #         data=list_parse2(
  #           data.frame(
  #             low=rep(0,33),
  #             high=rep(0,33)
  #           )
  #         )
  #       ) 
  #       # hcpxy_update_series(
  #       #   id = "central",
  #       #   data=df_region$prop_resp
  #       # ) %>%
  #       # hcpxy_update_series(
  #       #   id = "confidence",
  #       #   data=list_parse2(
  #       #     data.frame(
  #       #       low=df_region_nb$prop_resp_lb,
  #       #       high=df_region_nb$prop_resp_ub
  #       #     )
  #       #   )
  #       # ) %>%
  #       # hcpxy_update_series(
  #       #   id='london',
  #       #   data=rep(0, 33)
  #       # ) %>%
  #       # hcpxy_update_series(
  #       #   id='london2',
  #       #   data=list_parse2(
  #       #     data.frame(
  #       #       low=rep(0,33),
  #       #       high=rep(0,33)
  #       #     )
  #       #   )
  #       # ) 
  #       
  #   }
  #   
  #   if (input$radio=='og') {
  #     df_region_og <- df_list[[1]][['region']][['dataframe']] %>% mutate(drilldown=case_when(region=='London'~'london',T~'')) 
  #     subtitle <-  df_list[[1]][['region']][['title']]
  #     df_borough_og <- df_list[[1]][['borough']][['dataframe']] 
  #     
  #     highchartProxy("plot") %>%
  #       hcpxy_update_series(
  #         id='london',
  #         data=df_borough_og$prop_resp
  #       ) %>%
  #       hcpxy_update_series(
  #         id='london2',
  #         data=list_parse2(
  #           data.frame(
  #             low=df_borough_og$prop_resp_lb,
  #             high=df_borough_og$prop_resp_ub
  #           )
  #         )
  #       ) %>%
  #       hcpxy_update_series(
  #         id = "central",
  #         data=df_region_og$prop_resp
  #       ) %>%
  #       hcpxy_update_series(
  #         id = "confidence",
  #         data=list_parse2(
  #           data.frame(
  #             low=df_region_og$prop_resp_lb,
  #             high=df_region_og$prop_resp_ub
  #           )
  #         )
  #       ) 
  #       
  #   }
  #   
  # }, ignoreInit=T, ignoreNULL=F, once=F)
  # 
  # 
  # 
  # makeReactiveBinding("outputText")
  # 
  # observeEvent(input$drillupReturn, {
  #   outputText <<- paste0("You clicked on series ", input$drillupReturn[1], " and the bar you clicked was from category ") 
  # })
  # 
  # 
  # output$text <- renderText({
  #   outputText      
  # })
})
  
  
}

shinyApp(ui, server)