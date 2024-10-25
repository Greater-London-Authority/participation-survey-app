ui <- fluidPage(
  actionButton(
    "update", "Update"
  ),
  highchartOutput('plot', height='75vh'),
)


server <- function(input, output) {
  
  output$plot <- renderHighchart({
    
    highchart() %>%
      hc_add_series(
        name='Value',
        type='column',
        id='value',
        data = data.frame(
          name=c('Republican','Democrats','Other'),
          y=c(5,2,4),
          drilldown=c('republican-value','democrats-value','other-value')
        ),
        hcaes(x=name, y=y, drilldown=drilldown)
      ) %>%
      hc_add_series(
        name='Error',
        type='errorbar',
        id='error',
        data=data.frame(
          name=c('Republican','Democrats','Other'),
          low=c(4,1,3),
          high=c(6,3,5),
          drilldown=c('republican-error','democrats-error','other-error')
        ),
        hcaes(x=name, low=low, high=high, drilldown=drilldown)
      ) %>%
      hc_drilldown(
        allowPointDrilldown=F,
        series = list(
          list(
            id='republican-value',
            type='column',
            data = list_parse2(
              data.frame(
                name=c('East', 'West', 'North', 'South'),
                value=c(4,2,1,4)
              )
            )
          ),
          list(
            id='republican-error',
            type='errorbar',
            data=list_parse2(
              data.frame(
                name=c('East', 'West', 'North', 'South'),
                low=c(3.5,1.5,.5, 3.5),
                high=c(4.5,2.5,1.5, 4.5)
              )
              
            )
          )
        )
      )
    
  }
  )
  
  observeEvent(input$update, {
    highchartProxy('plot') %>%

      hcpxy_update_series(
        id='value',
        data=c(10,8,6)
      ) %>%
      
      hcpxy_update_series(
        id='error',
        data=list_parse2(
          data.frame(
          low=c(8,2,6),
          high=c(12,6,10)
          )
        )
      ) %>%
      
      
    hcpxy_update(
      drilldown=list(
        series=list(
          list(
            id='republican-value',
            type='column',
              data=list_parse2(
                data.frame(
                  name=c('East', 'West', 'North', 'South'),
                  value=c(8,4,2,8)
                )

            )

          ),
          list(
            id='republican-error',
            type='errorbar',
            data=list_parse2(
              data.frame(
                name=c('East', 'West', 'North', 'South'),
                low=c(7,3,1, 7),
                high=c(9,5,3, 9)
              )
            )
               
        )
      )
    )
    )
    
    
  })
          # list(
          #   id='animals',
          #           data=list_parse2(
          #             data.frame(
          #               name=c('Cows', 'Sheep'),
          #               y=c(20, 20)
          #             )
          # 
          #           )
          # 
          #         )
      #     )
      #   )
      # )
#
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
      

  
}


shinyApp(ui, server)
    