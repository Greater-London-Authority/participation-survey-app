

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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
    delay(100,
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
    delay(100,
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
