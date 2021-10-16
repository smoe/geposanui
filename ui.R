library(DT)
library(plotly)
library(rclipboard)
library(shiny)

source("methods.R")

ui <- fluidPage(
    shinyjs::useShinyjs(),
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
            h3("Methods"),
            actionButton(
                "optimize_button",
                "Find optimal weights",
                icon = icon("check-double")
            ),
            div(style = "margin-top: 16px"),
            lapply(methods, function(method) {
                verticalLayout(
                    checkboxInput(
                        method$id,
                        span(
                            method$description,
                            style = "font-weight: bold"
                        ),
                        value = TRUE
                    ),
                    sliderInput(
                        sprintf("%s_weight", method$id),
                        NULL,
                        post = "%",
                        min = 0,
                        max = 100,
                        step = 1,
                        value = 100
                    )
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
                    uiOutput("copy"),
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
                    "Assessment",
                    htmlOutput("assessment_synopsis"),
                    div(
                        style = "margin-top: 16px",
                        plotlyOutput(
                            "rank_plot",
                            width = "100%",
                            height = "600px"
                        )
                    ),
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