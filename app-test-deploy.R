#
if(!require("shiny"))
  install.packages("shiny")
if(!require("tidyverse"))
  install.packages("tidyverse")
if(!require("rvest"))
  install.packages("rvest")
if(!require("readxl"))
  install.packages("readxl")
if(!require("readODS"))
  install.packages("readODS")
if(!require("tidyverse"))
  install.packages("tidyverse")
if(!require("geojsonio"))
  install.packages("geojsonio")
if(!require("gglaplot"))
  install.packages("gglaplot")
if(!require("highcharter"))
  install.packages("highcharter")
if(!require("here"))
  install.packages("here")
if(!require("shinyjs"))
  install.packages("shinyjs")
if(!require("bslib"))
  install.packages("bslib")
if(!require("shinyWidgets"))
  install.packages("shinyWidgets")
if(!require("shinycssloaders"))
  install.packages("shinycssloaders")
if(!require("reticulate"))
  install.packages("reticulate")

source('scripts/master.R')
knitr::opts_chunk$set(echo = TRUE,scipen=999)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
options("scipen"=100, "digits"=4)


faithful_data <- sample_n(faithful, 100)


ui <- 
  fluidPage(
    #includeCSS("FORMATTING\\GLAstyle.css"),
    selectInput("n_breaks", label = "Number of bins:",
                choices = c(10, 20, 35, 50), selected = 20),
    
    sliderInput("bw_adjust", label = "Bandwidth adjustment:",
                min = 0.2, max = 2, value = 1, step = 0.2),
    plotOutput("eruptions")
  )

server <- function(input, output) {
  
  output$eruptions <- renderPlot({
  hist(faithful_data$eruptions, probability = TRUE,
       breaks = as.numeric(input$n_breaks),
       xlab = "Duration (minutes)", main = "Geyser Eruption Duration")
  
  dens <- density(faithful_data$eruptions, adjust = input$bw_adjust)
  lines(dens, col = "blue")
})
  
}

shinyApp(ui = ui, server = server)
    