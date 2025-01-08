library(shiny)

ui <- fluidPage(
  titlePanel("Tabsets"),
  div(style = "position:absolute;right:60vw;", 
      actionButton('load_inputs', 'Load inputs'),
      actionButton('save_inputs', 'Save inputs')
  ),
  tabsetPanel(
    tabPanel("Tab1", h2("Content 1")), 
    tabPanel("Tab2", h2("Content 2")), 
    tabPanel("Tab3", h2("Content 3"))
  )
)

server <- function(input, output, session) {
}
runApp(list(ui = ui, server = server))