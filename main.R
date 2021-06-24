library(shiny)

source("server.R")
source("ui.R")

runApp(shinyApp(ui, server))