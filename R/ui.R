ui <- div(
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup(),
    navbarPage(
        theme = bslib::bs_theme(
            version = 5,
            bootswatch = "united",
            primary = "#1964bf"
        ),
        title = "TPE-OLD candidates",
        selected = "Results",
        tabPanel(
            "Input data",
            input_page_ui("input_page")
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
                            results_ui("results")
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
                        ),
                        tabPanel(
                            title = "DisGeNET",
                            div(
                                style = "margin-top: 16px",
                                DT::DTOutput("disgenet")
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
