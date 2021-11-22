ui <- fluidPage(
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    titlePanel("TPE-OLD candidates"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            preset_editor_ui("preset_editor"),
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
                        DT::DTOutput("genes")
                    )
                ),
                tabPanel(
                    "Positions",
                    checkboxInput(
                        "use_positions",
                        "Show positions instead of distances"
                    ),
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
                    div(
                        style = "margin-top: 16px",
                        plotly::plotlyOutput(
                            "boxplot",
                            width = "100%",
                            height = "600px"
                        )
                    ),
                    div(
                        style = "margin-top: 16px",
                        plotly::plotlyOutput(
                            "chromosome_plot",
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
