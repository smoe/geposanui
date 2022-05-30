# Construct UI for the filter editor.
filters_ui <- function(id) {
  verticalLayout(
    h3("Filter criteria"),
    selectInput(
      NS(id, "method"),
      "Filter method",
      choices = list(
        "Percentiles" = "percentile",
        "Scores" = "score",
        "Ranks" = "rank",
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
          label = "Included percentiles",
          post = "%",
          min = 0,
          max = 100,
          step = 1,
          value = c(95, 100)
        )
      ),
      tabPanelBody(
        value = "score",
        sliderInput(
          NS(id, "score"),
          label = "Included scores",
          post = "%",
          min = 0,
          max = 100,
          step = 1,
          value = c(90, 100)
        )
      ),
      tabPanelBody(
        value = "rank",
        sliderInput(
          NS(id, "rank"),
          label = "Included ranks",
          min = 1,
          max = 2000,
          step = 10,
          value = c(1, 1000)
        )
      ),
      tabPanelBody(
        value = "none"
      )
    ),
    sliderInput(
      NS(id, "distance"),
      label = "Distance to telomeres",
      post = " Mbp",
      min = 0,
      max = 150,
      value = c(0, 150)
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

    observeEvent(results(), {
      updateSliderInput(session, "rank", max = nrow(results()))
    })

    reactive({
      results <- results()

      results_prefiltered <- if (input$method == "percentile") {
        n_ranks <- nrow(results)
        results[
          rank <= (1 - (input$percentile[1] / 100)) * n_ranks &
            rank >= (1 - (input$percentile[2] / 100)) * n_ranks
        ]
      } else if (input$method == "score") {
        results[
          score >= input$score[1] / 100 &
            score <= input$score[2] / 100
        ]
      } else if (input$method == "rank") {
        results[
          rank >= input$rank[1] &
            rank <= input$rank[2]
        ]
      } else {
        results
      }

      results_prefiltered[
        distance >= 1000000 * input$distance[1] &
          distance <= 1000000 * input$distance[2]
      ]
    })
  })
}
