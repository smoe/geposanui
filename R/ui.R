ui <- div(
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    navbarPage(
        theme = bslib::bs_theme(
            version = 3,
            bootswatch = "united",
            primary = "#1c71d8"
        ),
        title = "TPE-OLD candidates",
        selected = "Ranking",
        tabPanel(
            "Input data",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    preset_editor_ui("preset_editor"),
                    comparison_editor_ui("comparison_editor")
                ),
                mainPanel(
                    width = 9,
                    plotly::plotlyOutput(
                        "scatter",
                        width = "100%",
                        height = "600px"
                    )
                )
            ),
        ),
        tabPanel(
            "Ranking",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    methods_ui("methods"),
                    filters_ui("filters")
                ),
                mainPanel(
                    width = 9,
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
                            "rankings_plot",
                            width = "100%",
                            height = "600px"
                        )
                    ),
                )
            ),
        ),
        tabPanel(
            "Detailed results",
            uiOutput("copy"),
            div(
                style = "margin-top: 16px",
                DT::DTOutput("genes")
            )
        ),
        tabPanel(
            "Assessment",
            htmlOutput("assessment_synopsis"),
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
