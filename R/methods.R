# Construct UI for the methods editor.
methods_ui <- function(id) {
    verticalLayout(
        h3("Methods"),
        div(style = "margin-top: 16px"),
        lapply(methods, function(method) {
            verticalLayout(
                checkboxInput(
                    NS(id, method$id),
                    span(
                        method$description,
                        style = "font-weight: bold"
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
        }),
        radioButtons(
            NS(id, "target"),
            "Optimization target",
            choices = list(
                "Mean rank of reference genes" = "mean",
                "First rank of reference genes" = "min",
                "Last rank of reference genes" = "max"
            )
        ),
        actionButton(
            NS(id, "optimize_button"),
            "Optimize weights",
            class = "btn-primary"
        )
    )
}

# Construct server for the methods editor.
#
# @param analysis The reactive containing the results to be weighted.
#
# @return A reactive containing the weighted results.
methods_server <- function(id, analysis) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$optimize_button, {
            analysis <- analysis()
            method_ids <- NULL

            # Only include activated methods.
            for (method in methods) {
                if (input[[method$id]]) {
                    method_ids <- c(method_ids, method$id)
                }
            }

            weights <- geposan::optimal_weights(
                analysis,
                method_ids,
                analysis$preset$reference_gene_ids,
                target = input$target
            )

            for (method_id in method_ids) {
                updateSliderInput(
                    session,
                    sprintf("%s_weight", method_id),
                    value = weights[[method_id]]
                )
            }
        })

        # Observe each method's enable button and synchronise the slider state.
        lapply(methods, function(method) {
            observeEvent(input[[method$id]],
                { # nolint
                    shinyjs::toggleState(sprintf("%s_weight", method$id))
                },
                ignoreInit = TRUE
            )
        })

        reactive({
            # Take the actual weights from the sliders.

            weights <- NULL

            for (method in methods) {
                if (input[[method$id]]) {
                    weight <- input[[sprintf("%s_weight", method$id)]]
                    weights[[method$id]] <- weight
                }
            }

            geposan::ranking(analysis(), weights)
        })
    })
}
