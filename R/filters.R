# Construct UI for the filter editor.
filters_ui <- function(id) {
    verticalLayout(
        h3("Filter criteria"),
        selectInput(
            NS(id, "method"),
            "Filter method",
            choices = list(
                "Percentile" = "percentile",
                "Cut-off score" = "score",
                "Maximum number of genes" = "rank",
                "None" = "none"
            )
        ),
        tabsetPanel(
            id = NS(id, "sliders"),
            type = "hidden",
            tabPanelBody(
                value = "percentile",
                sliderInput(
                    NS(id, "percentile"),
                    label = "Minimum percentile",
                    post = "%",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 95
                )
            ),
            tabPanelBody(
                value = "score",
                sliderInput(
                    NS(id, "score"),
                    label = "Cut-off score",
                    post = "%",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = 75
                )
            ),
            tabPanelBody(
                value = "rank",
                sliderInput(
                    NS(id, "rank"),
                    label = "Maximum rank",
                    min = 0,
                    max = 2000,
                    step = 10,
                    value = 1000
                )
            ),
            tabPanelBody(
                value = "none"
            )
        )
    )
}

# Construct server for the filter editor.
#
# @param results The results to be filtered.
#
# @return A reactive containing the filtered results.
filters_server <- function(id, results) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$method, {
            updateTabsetPanel(session, "sliders", selected = input$method)
        })

        reactive({
            results <- results()

            if (input$method == "percentile")  {
                n_ranks <- nrow(results)
                results[rank <= (1 - (input$percentile / 100)) * n_ranks]
            } else if (input$method == "score") {
                results[score >= input$score / 100]
            } else if (input$method == "rank") {
                results[rank <= input$rank]
            } else {
                results
            }
        })
    })
}
