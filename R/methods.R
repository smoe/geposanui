# Construct UI for the methods editor.
methods_ui <- function(id, options) {
  verticalLayout(
    h5("Methods"),
    selectInput(
      NS(id, "optimization_genes"),
      "Genes to optimize for",
      choices = list(
        "Reference genes" = "reference",
        "Comparison genes" = "comparison"
      )
    ),
    selectInput(
      NS(id, "optimization_target"),
      "Optimization target",
      choices = list(
        "Mean rank" = "mean",
        "Median rank" = "median",
        "First rank" = "min",
        "Last rank" = "max",
        "Customize weights" = "custom"
      )
    ),
    lapply(options$methods, function(method) {
      verticalLayout(
        div(
          style = "display: flex;",
          checkboxInput(
            NS(id, method$id),
            span(
              method$description,
              class = "control-label"
            ),
            value = TRUE
          ),
          div(
            style = "margin-left: 5px;",
            bslib::popover(
              bsicons::bs_icon("question-circle"),
              title = method$name,
              method$help
            )
          )
        ),
        sliderInput(
          NS(id, sprintf("%s_weight", method$id)),
          NULL,
          min = -1.0,
          max = 1.0,
          step = 0.01,
          value = 1.0
        )
      )
    })
  )
}

#' Construct server for the methods editor.
#'
#' @param options Global options for the application.
#' @param analysis The reactive containing the results to be weighted.
#' @param comparison_gene_ids The comparison gene IDs.
#'
#' @return A reactive containing the weighted results.
#' @noRd
methods_server <- function(id, options, analysis, comparison_gene_ids) {
  moduleServer(id, function(input, output, session) {
    # Observe each method's enable button and synchronise the slider state.
    lapply(options$methods, function(method) {
      observeEvent(input[[method$id]], {
        shinyjs::toggleState(
          sprintf("%s_weight", method$id),
          condition = input[[method$id]]
        )
      })

      shinyjs::onclick(sprintf("%s_weight", method$id), {
        updateSelectInput(
          session,
          "optimization_target",
          selected = "custom"
        )
      })
    })

    # This reactive will always contain the currently selected optimization
    # gene IDs in a normalized form.
    optimization_gene_ids <- reactive({
      gene_ids <- if (input$optimization_genes == "comparison") {
        comparison_gene_ids()
      } else {
        analysis()$preset$reference_gene_ids
      }

      sort(unique(gene_ids))
    })

    # This reactive will always contain the optimal weights according to
    # the selected parameters.
    optimal_weights <- reactive({
      withProgress(message = "Optimizing weights", {
        setProgress(0.2)

        included_methods <- NULL

        for (method in options$methods) {
          if (input[[method$id]]) {
            included_methods <- c(included_methods, method$id)
          }
        }

        geposan::optimal_weights(
          analysis(),
          included_methods,
          optimization_gene_ids(),
          target = input$optimization_target
        )
      })
    }) |> bindCache(
      analysis(),
      optimization_gene_ids(),
      sapply(options$methods, function(method) input[[method$id]]),
      input$optimization_target
    )

    reactive({
      weights <- NULL

      if (length(optimization_gene_ids()) < 1 |
        input$optimization_target == "custom") {
        for (method in options$methods) {
          if (input[[method$id]]) {
            weight <- input[[sprintf("%s_weight", method$id)]]
            weights[[method$id]] <- weight
          }
        }
      } else {
        weights <- optimal_weights()

        for (method_id in names(weights)) {
          updateSliderInput(
            session,
            sprintf("%s_weight", method_id),
            value = weights[[method_id]]
          )
        }
      }

      geposan::ranking(analysis(), weights)
    })
  })
}
