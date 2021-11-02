ui <- fluidPage(
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    titlePanel("TPE-OLD candidates"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            preset_editor_ui("preset_editor"),
            uiOutput("n_species_slider"),
            filters_ui("filters"),
            methods_ui("methods")
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
                        DT::DTOutput("genes", height = "1000px")
                    )
                ),
                tabPanel(
                    "Positions",
                    plotly::plotlyOutput(
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
                        plotly::plotlyOutput(
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
                        plotly::plotlyOutput(
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
