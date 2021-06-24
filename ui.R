library(DT)
library(shiny)

ui <- fluidPage(
    titlePanel("TPE-OLD candidates"),
    sidebarLayout(
        position = "right",
        sidebarPanel(
            h3("Candidate selection"),
            DTOutput("genes"),
            width = 3
        ),
        mainPanel(
            plotOutput("scatter"),
        )
    )
)