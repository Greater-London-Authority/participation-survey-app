library(shiny)
library(DT)

ui <- fluidPage(
  mainPanel(
    highchartOutput(outputId = "plot"),
    DTOutput('citytemp')  
  )
)


server <- function(input, output) {
  
  citytemp <- as.data.frame(citytemp)
  
  line_clicked <- JS("function(event) {Shiny.onInputChange('line_clicked', this.name );}")
  legend_clicked <- JS("function(event) {Shiny.onInputChange('legend_clicked', [this.name, event.target.visible] );}")
  
  output$plot <- renderHighchart({
    
    #ad-hoc drill down:
    
    if(is.null(input$line_clicked)){
      
      series <- colnames(citytemp)[2:5]
    }
    else{
      if(input$line_clicked == "tokyo"){
        
        series <- colnames(citytemp)[4:5]
      }
      else{
        
        series <- colnames(citytemp)[2:5]
      }}
    
    hc <- highchart() %>% 
      hc_xAxis(categories = citytemp$month) %>% 
      hc_plotOptions(series = list(events = list(click = line_clicked, legendItemClick = legend_clicked)))
    
    for(i in 1:length(series))
    {
      hc <- hc %>% hc_add_series(name = series[i], data = citytemp[,series[i]])
    }
    hc
  })
  
  #Table output
  output$citytemp <- DT::renderDataTable({
    #here i would like to filter in the dataframe in connection to what legends are on. 
    datatable(citytemp)
  })
}
shinyApp(ui = ui, server = server)