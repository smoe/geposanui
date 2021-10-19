library(shiny)

# Initialize data first.
source("data.R")

source("server.R")
source("ui.R")

runApp(shinyApp(ui, server))
