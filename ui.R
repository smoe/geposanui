library(DT)
library(shiny)

ui <- fluidPage(
    titlePanel("TPE-OLD candidates"),
    sidebarLayout(
        position = "right",
        sidebarPanel(
            h3("Candidate selection"),
            selectInput(
                "species",
                "Species to include",
                choices = list(
                    "All qualified" = "all",
                    "Replicatively aging" = "replicative"
                )
            ),
            sliderInput(
                "range",
                "Gene position (Mbp)",
                min = 0,
                max = 50,
                value = c(0, 15),
                step = 0.1
            ),
            sliderInput(
                "length",
                "Minimum cluster size",
                min = 0,
                max = 30,
                value = 10
            ),
            DTOutput("genes"),
            width = 3
        ),
        mainPanel(
            plotOutput("scatter"),
        )
    )
)