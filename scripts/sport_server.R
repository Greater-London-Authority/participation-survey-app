

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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
    delay(100,
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
    delay(100,
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
