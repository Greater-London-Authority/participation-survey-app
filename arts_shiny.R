fluidPage(
  includeCSS("FORMATTING\\GLAstyle.css"),
  useShinyjs(rmd=T),
  fluidRow(
    column(7,#style = "background-color: #fafafa !important;",
           div(class='tab-panel-ui',
               shinyjs::hidden(
                 pickerInput(
                   inputId="arts_select",
                   label="Question",
                   choices = ARTS_QUESTIONS,
                   selected=1,
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
  )
)



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
      delay(100,
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
      delay(100,
            shinyjs::runjs(
              paste0(
                "
                var chart = $('#arts_chart').highcharts();
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
    else if (input$arts_currLevel==0) { # drill level
      #browser()
      delay(100,
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
      delay(100,
            shinyjs::runjs(
              paste0(
                "
                var chart = $('#arts_chart').highcharts();
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
      delay(100,
            shinyjs::runjs(
              paste0(
                "
                var chart = $('#arts_chart').highcharts();
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
  else if ('mean'%in%input$arts_compOps & 'error'%ni%input$arts_compOps) {
    #browser()
    if (is.null(input$arts_currLevel)) {
      #browser()
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
      
      delay(100,
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
      delay(100,
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
      delay(100,
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
      delay(100,
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
    delay(100,
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
    delay(100,
          shinyjs::runjs(
            "
                var chart = $('#arts_chart').highcharts();
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