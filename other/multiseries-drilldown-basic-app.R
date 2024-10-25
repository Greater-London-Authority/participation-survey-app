ui <- fluidPage(
  actionButton(
    "update", "Update"
  ),
  highchartOutput('plot', height='75vh'),
)


server <- function(input, output) {
  
  output$plot <- renderHighchart({
    highchart() %>%
      hc_chart(
        type = "column",
        events = list(
          drilldown = JS(
            "function(e) {
          if (!e.seriesOptions) {
            var chart = this,
              drilldowns1 = {
                'Animals': {
                  name: 'Animals',
                  id: 'animals',
                  data: [
                    ['Cows', 2],
                    ['Sheep', 3]
                  ]
                },
                'Fruits': {
                  name: 'Fruits',
                  id: 'fruits',
                  data: [
                    ['Apples', 5],
                    ['Oranges', 7],
                    ['Bananas', 2]
                  ]
                },
                'Cars': {
                  name: 'Cars',
                  id: 'cars',
                  data: [
                    ['Toyota', 1],
                    ['Volkswagen', 2],
                    ['Opel', 5]
                  ]
                }
              },
              drilldowns2 = {
                'Animals': {
                  name: 'Animals',
                  id: 'animals',
                  data: [
                    ['Cows', 8],
                    ['Sheep', 7]
                  ]
                },
                'Fruits': {
                  name: 'Fruits',
                  id: 'fruits',
                  data: [
                    ['Apples', 3],
                    ['Oranges', 1],
                    ['Bananas', 5]
                  ]
                },
                'Cars': {
                  name: 'Cars',
                  id: 'cars',
                  data: [
                    ['Toyota', 6],
                    ['Volkswagen', 5],
                    ['Opel', 11]
                  ]
                }
              },
              
              series1 = drilldowns1[e.point.name];
              series2 = drilldowns2[e.point.name];
              

            chart.addSingleSeriesAsDrilldown(e.point, series1);
            chart.addSingleSeriesAsDrilldown(e.point, series2);
            chart.applyDrilldown();
          }
        }"
          )
        )
      ) %>%
      hc_xAxis(type = "category") %>%
      hc_add_series(
        name = "Things", 
        id='things',
        data = list(
          list(name = "Animals", y = 5, drilldown = 'animals'), 
          list(name = "Fruits", y = 2, drilldown = 'fruits'),
          list(name = "Cars", y =4, drilldown = 'cars')
        )
      ) 
    
    
  }
  )
  
  observeEvent(input$update, {
    #browser()
    highchartProxy('plot') %>%
      # hcpxy_update_series(
      #   id='animals',
      #   data=list_parse2(
      #     data.frame(
      #     name=c('Cows', 'Sheep'),
      #     y=c(50, 50)
      #     )
      #   
      # )
      # ) %>%
      hcpxy_update_series(
        id='things',
        data=list_parse2(
          data.frame(
            name=c('Animals','Fruits', 'Cars'),
            y=c(20, 10, 40)
          )
          
        )
      ) %>%
    hcpxy_update(
      drilldown=list(
        series=list(
          list(
            id='animals',
              data=list_parse2(
                data.frame(
                name=c('Cows', 'Sheep'),
                y=c(50, 50)
                )

            )

          ),
          list(
            id='animals',
                    data=list_parse2(
                      data.frame(
                        name=c('Cows', 'Sheep'),
                        y=c(20, 20)
                      )

                    )

                  )
          )
        )
      )

    #) #%>%
      # hcpxy_update(
      #   drilldown=list(
      #     series=list(
      #       list(
      #         id='animals2',
      #         data=list_parse2(
      #           data.frame(
      #             name=c('Cows', 'Sheep'),
      #             y=c(20, 20)
      #           )
      #           
      #         )
      #         
      #       )
      #     )
      #   )
      #   
      # ) 
      
  })
  
  
}


shinyApp(ui, server)
    