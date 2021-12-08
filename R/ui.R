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
        selected = "Results",
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
            "Results",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    methods_ui("methods"),
                    filters_ui("filters")
                ),
                mainPanel(
                    width = 9,
                    tabsetPanel(
                        type = "pills",
                        tabPanel(
                            title = "Overview",
                            div(
                                style = "margin-top: 16px",
                                plotly::plotlyOutput(
                                    "rank_plot",
                                    width = "100%",
                                    height = "600px"
                                )
                            )
                        ),
                        tabPanel(
                            title = "Methods & Distribution",
                            div(
                                style = "margin-top: 16px",
                                plotly::plotlyOutput(
                                    "rankings_plot",
                                    width = "100%",
                                    height = "600px"
                                )
                            )
                        ),
                        tabPanel(
                            title = "Comparison",
                            div(
                                style = "margin-top: 16px",
                                plotly::plotlyOutput(
                                    "boxplot",
                                    width = "100%",
                                    height = "600px"
                                )
                            )
                        ),
                        tabPanel(
                            title = "Detailed results",
                            div(
                                style = "margin-top: 16px",
                                uiOutput("copy"),
                            ),
                            div(
                                style = "margin-top: 16px",
                                DT::DTOutput("genes")
                            )
                        ),
                        tabPanel(
                            title = "g:Profiler",
                            div(
                                style = "margin-top: 16px",
                                plotly::plotlyOutput("gost_plot"),
                            ),
                            div(
                                style = "margin-top: 16px",
                                DT::DTOutput("gost_details")
                            )
                        )
                    )
                )
            )
        ),
        tabPanel(
            title = "Publication"
        )
    )
)
