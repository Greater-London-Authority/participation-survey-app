

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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
    delay(100,
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
    delay(100,
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
