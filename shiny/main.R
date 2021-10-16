library(shiny)

source("process/process.R")
source("shiny/server.R")
source("shiny/ui.R")

runApp(shinyApp(ui, server))