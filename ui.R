library(DT)
library(plotly)
library(rclipboard)
library(shiny)

source("methods.R")

ui <- fluidPage(
    rclipboardSetup(),
    titlePanel("TPE-OLD candidates"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            h3("Filter criteria"),
            selectInput(
                "species",
                "Species to include",
                choices = list(
                    "Replicatively aging" = "replicative",
                    "All qualified" = "all"
                )
            ),
            uiOutput("n_species_slider"),
            sliderInput(
                "cutoff",
                "Cut-off score",
                post = "%",
                min = 0,
                max = 100,
                step = 1,
                value = 50
            ),
            h3("Ranking"),
            lapply(methods, function(method) {
                sliderInput(
                    method$id,
                    method$description,
                    post = "%",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 100
                )
            }),
            checkboxInput(
                "penalize",
                "Penalize missing values"
            ),
        ),
        mainPanel(
            tabsetPanel(
                type = "pills",
                header = div(style = "margin-top: 16px"),
                tabPanel(
                    "Results",
                    textOutput("synposis"),
                    div(
                        style = "margin-top: 16px",
                        uiOutput("copy")
                    ),
                    div(
                        style = "margin-top: 16px",
                        DTOutput("genes", height = "1000px")
                    )
                ),
                tabPanel(
                    "Positions",
                    plotlyOutput(
                        "scatter",
                        width = "100%",
                        height = "600px"
                    )
                ),
                tabPanel(
                    "Ranks",
                    plotlyOutput(
                        "rank_plot",
                        width = "100%",
                        height = "600px"
                    )
                ),
                tabPanel(
                    "Analysis",
                    checkboxInput(
                        "enable_gost",
                        "Perform a gene set enrichment analysis on the \
                        filtered result genes."
                    ),
                    conditionalPanel(
                        "input.enable_gost == true",
                        plotlyOutput(
                            "gost",
                            width = "100%",
                            height = "600px"
                        )
                    )
                )
            )
        )
    )
)