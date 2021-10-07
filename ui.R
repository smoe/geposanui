library(DT)
library(plotly)
library(shiny)

ui <- fluidPage(
    titlePanel("TPE-OLD candidates"),
    column(
        width = 3,
        wellPanel(
            h3("Filter criteria"),
            selectInput(
                "species",
                "Species to include",
                choices = list(
                    "Replicatively aging" = "replicative",
                    "All qualified" = "all"
                )
            )
        ),
        wellPanel(
            h3("Ranking"),
            sliderInput(
                "clusteriness",
                "Clustering of genes",
                post = "%",
                min = 0,
                max = 100,
                step = 1,
                value = 58
            ),
            sliderInput(
                "correlation",
                "Correlation with known genes",
                post = "%",
                min = 0,
                max = 100,
                step = 1,
                value = 36
            ),
            sliderInput(
                "neural",
                "Assessment by neural network",
                post = "%",
                min = 0,
                max = 100,
                step = 1,
                value = 6
            ),
            sliderInput(
                "cutoff",
                "Cut-off score",
                post = "%",
                min = 0,
                max = 100,
                step = 1,
                value = 50
            )
        ),
        wellPanel(
            h3("Results"),
            textOutput("synposis"),
            div(
                style = "overflow-x: auto; overflow-y: auto; margin-top: 16px",
                DTOutput("genes")
            )
        )
    ),
    column(
        width = 8,
        wellPanel(
            h3("Gene positions"),
            p("This plot shows the selected genes' distance to the telomeres \
               across species. It visualizes how certain genes have \
               evolutionary conserved positions."),
            div(
                style = "overflow-x: auto",
                div(
                    style = "min-width: 1400px",
                    plotOutput(
                        "scatter",
                        width = "100%",
                        height = "600px"
                    )
                )
            )
        ),
        wellPanel(
            h3("Gene set enrichment analysis"),
            checkboxInput(
                "enable_gost",
                "Perform a gene set enrichment analysis on the filtered result \
                 genes."
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