library(shiny)

vector_choices <- seq(5, 50, by = 5)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("selector"),
      tags$div(class="row",
               tags$div(uiOutput("prevBin")),
               tags$div(uiOutput("nextBin")))
      
    ),
    
    mainPanel(
      textOutput("text"),
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  output$selector <- renderUI({
    selectInput("bins",
                "Bins",
                choices = as.list(vector_choices),
                selected = 25)
  })
  
  output$prevBin <- renderUI({
    actionButton("prevBin", 
                 label = "Previous")
  })
  output$nextBin <- renderUI({
    actionButton("nextBin", 
                 label = "Next")
  })
  
  observeEvent(input$prevBin, {
    current <- which(vector_choices == input$bins)
    if(current > 1){
      updateSelectInput(session, "bins",
                        choices = as.list(vector_choices),
                        selected = vector_choices[current - 1])
    }
  })
  observeEvent(input$nextBin, {
    current <- which(vector_choices == input$bins)
    if(current < length(vector_choices)){
      updateSelectInput(session, "bins",
                        choices = as.list(vector_choices),
                        selected = vector_choices[current + 1])
    }
  })
  
  output$distPlot <- renderPlot({
    x <- rnorm(100)
    bins <- seq(min(x), max(x), length.out = as.numeric(input$bins))
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

shinyApp(ui = ui, server = server)