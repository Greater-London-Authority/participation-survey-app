df <- data_frame(
  name = c("Animals", "Fruits", "Cars"),
  y = c(5, 2, 4),
  drilldown = c('animals','','')
)

df

dfan <- data_frame(
  name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
  value = c(4, 3, 1, 2, 1)
)

dfan2 <- data_frame(
  name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
  value = c(8, 6, 2, 4, 2)
)

list1 <- toString(
  lapply(
    1:nrow(dfan), function(n) {
      paste0("['",dfan[n,'name'],"',",dfan[n,'value'],"]")
    }
  )
)
list2 <- toString(
  lapply(
    1:nrow(dfan2), function(n) {
      paste0("['",dfan2[n,'name'],"',",dfan2[n,'value'],"]")
    }
  )
)




hc <- highchart() %>%
  # hc_chart(type = "column",
  #          events = list(
  #            click = JS(fn))) %>%
  hc_title(text = "Basic drilldown") %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>%
  hc_add_series(
    data = df,
    name = "Things",
    colorByPoint = TRUE
  ) %>%
  
  hc_chart(events = list(
    drilldown = JS(paste0(
      "function(e) {
        if(!e.seriesOptions){
        var chart=this,
        drilldowns={
          'Animals':{
            name:'Animals',
            data:[",list1,"],
            type:'bar'
          },
          'Animals2':{
            name:'Animals',color:'#f00',
            data:[",list2,"],
            type:'bar'
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
hc


df <- data_frame(
  name = c("Animals", "Fruits", "Cars"),
  y = c(5, 2, 4),
  drilldown = c('','','')
)
dfan1 <- data_frame(
  name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
  value = c(4, 3, 1, 2, 1)
)
dfan2 <- data_frame(
  name = c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),
  low = c(3.5,2.5,.5,1.5,.5),
  high= c(4.5,3.5,1.5,2.5,1.5)
)
list1 <- toString(
  lapply(
    1:nrow(dfan1), function(n) {
      paste0("['",dfan1[n,'name'],"',",dfan1[n,'value'],"]")
    }
  )
)
list2 <- toString(
  lapply(
    1:nrow(dfan2), function(n) {
      paste0("['",dfan2[n,'name'],"',",dfan2[n,'low'],",",dfan2[n,'high'],"]")
    }
  )
)
hc <- highchart() %>%
  hc_title(text = "Basic multi series drilldown") %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>%
  hc_add_series(
    data = df,
    name = "Things",
    colorByPoint = TRUE
  ) %>%
  hc_chart(events = list(
    drilldown = JS(paste0(
      "function(e) {
        if(!e.seriesOptions){
        var chart=this,
        drilldowns={
          'Animals':{
            name:'Animals',
            data:[",list1,"],
            type:'bar'
          },
          'Animals2':{
            name:'Animals',color:'#f00',
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
hc








































# 'Fruits':{name:'Fruits',data:[['Apples',5],['Oranges',7],['Bananas',2]]},'Fruits2':{name:'Fruits',color:'red',data:[['Apples',15],['Oranges',17],['Bananas',22]]},
# 'Cars':{name:'Cars',data:[['Toyota',1],['Volkswagen',2],['Opel',5]]},'Cars2':{name:'Cars',color:'#bada55',data:[['Toyota',11],['Volkswagen',21],['Opel',15]]}},



hc <- hc %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list(
      list(
        id = "animals",
        data = list_parse2(dfan)
      ),
      list(
        id = "fruits",
        data = list_parse2(dfru)
      ),
      list(
        id = "cars",
        type = "line",
        data = list_parse2(car_series)
      )
    )
  ) %>%

drilldown = JS(
  "function(e) {if(!e.seriesOptions){var chart=this,drilldowns={'Animals':{name:'Animals',data:[['Cows',2],['Sheep',3]]},'Animals2':{name:'Animals',color:'#f00',data:[['Cows',22],['Sheep',13]]},'Fruits':{name:'Fruits',data:[['Apples',5],['Oranges',7],['Bananas',2]]},'Fruits2':{name:'Fruits',color:'red',data:[['Apples',15],['Oranges',17],['Bananas',22]]},'Cars':{name:'Cars',data:[['Toyota',1],['Volkswagen',2],['Opel',5]]},'Cars2':{name:'Cars',color:'#bada55',data:[['Toyota',11],['Volkswagen',21],['Opel',15]]}},series=[drilldowns[e.point.name],drilldowns[e.point.name+'2']];chart.addSingleSeriesAsDrilldown(e.point,series[0]);chart.addSingleSeriesAsDrilldown(e.point,series[1]);chart.applyDrilldown()}}"
  )