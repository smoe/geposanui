# Construct UI for the methods editor.
methods_ui <- function(id) {
    verticalLayout(
        h3("Methods"),
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
        lapply(methods, function(method) {
            verticalLayout(
                checkboxInput(
                    NS(id, method$id),
                    span(
                        method$description,
                        class = "control-label"
                    ),
                    value = TRUE
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

# Construct server for the methods editor.
#
# @param analysis The reactive containing the results to be weighted.
#
# @return A reactive containing the weighted results.
methods_server <- function(id, analysis, comparison_gene_ids) {
    moduleServer(id, function(input, output, session) {
        # Observe each method's enable button and synchronise the slider state.
        lapply(methods, function(method) {
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

        reactive({
            analysis <- analysis()
            weights <- NULL

            gene_ids <- if (input$optimization_genes == "comparison") {
                comparison_gene_ids()
            } else {
                analysis$preset$reference_gene_ids
            }

            if (length(gene_ids) < 1 | input$optimization_target == "custom") {
                for (method in methods) {
                    if (input[[method$id]]) {
                        weight <- input[[sprintf("%s_weight", method$id)]]
                        weights[[method$id]] <- weight
                    }
                }
            } else {
                withProgress(message = "Optimizing weights", {
                    setProgress(0.2)

                    included_methods <- NULL

                    for (method in methods) {
                        if (input[[method$id]]) {
                            included_methods <- c(included_methods, method$id)
                        }
                    }

                    weights <- geposan::optimal_weights(
                        analysis,
                        included_methods,
                        gene_ids,
                        target = input$optimization_target
                    )

                    for (method_id in names(weights)) {
                        updateSliderInput(
                            session,
                            sprintf("%s_weight", method_id),
                            value = weights[[method_id]]
                        )
                    }
                })
            }

            geposan::ranking(analysis, weights)
        })
    })
}
