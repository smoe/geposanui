#' Create a GSEA page.
#' @noRd
gsea_ui <- function(id) {
  verticalLayout(
    div(
      style = "margin-top: 16px",
      plotly::plotlyOutput(NS(id, "plot")),
    ),
    div(
      style = "margin-top: 16px",
      DT::DTOutput(NS(id, "details"))
    )
  )
}

#' Create a server for the comparison editor.
#'
#' @param id ID for namespacing the inputs and outputs.
#' @param ranking The ranking to be analyzed.
#'
#' @noRd
gsea_server <- function(id, ranking) {
  moduleServer(id, function(input, output, session) {
    gsea_analysis <- reactive({
      withProgress(
        message = "Querying g:Profiler",
        value = 0.0,
        { # nolint
          setProgress(0.2)
          gprofiler2::gost(
            ranking()[, gene],
            custom_bg = NULL, # TODO
            domain_scope = "custom_annotated"
          )
        }
      )
    }) |> bindCache(ranking())

    output$plot <- plotly::renderPlotly({
      gprofiler2::gostplot(
        gsea_analysis(),
        capped = FALSE,
        interactive = TRUE
      )
    })

    output$details <- DT::renderDT({
      data <- data.table(gsea_analysis()$result)
      setorder(data, p_value)

      data[, total_ratio := term_size / effective_domain_size]
      data[, query_ratio := intersection_size / query_size]
      data[, increase := (query_ratio - total_ratio) / total_ratio]

      data <- data[, .(
        source,
        term_name,
        total_ratio,
        query_ratio,
        increase,
        p_value
      )]

      DT::datatable(
        data,
        rownames = FALSE,
        colnames = c(
          "Source",
          "Term",
          "Total ratio",
          "Query ratio",
          "Increase",
          "p-value"
        ),
        options = list(
          pageLength = 25
        )
      ) |>
        DT::formatRound("p_value", digits = 4) |>
        DT::formatPercentage(
          c("total_ratio", "query_ratio", "increase"),
          digits = 2
        )
    })
  })
}
