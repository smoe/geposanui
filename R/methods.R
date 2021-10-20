# Construct UI for the methods editor.
methods_ui <- function(id) {
    initial_weight <- 100 / length(methods)

    verticalLayout(
        h3("Methods"),
        actionButton(
            NS(id, "optimize_button"),
            "Find optimal weights",
            icon = icon("check-double")
        ),
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
                    post = "%",
                    min = 0,
                    max = 100,
                    step = 1,
                    value = initial_weight
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
methods_server <- function(id, analysis) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$optimize_button, {
            method_ids <- NULL

            # Only include activated methods.
            for (method in methods) {
                if (input[[method$id]]) {
                    method_ids <- c(method_ids, method$id)
                }
            }

            weights <- geposan::optimize_weights(
                analysis(),
                method_ids,
                genes_tpe_old
            )

            for (method_id in method_ids) {
                updateSliderInput(
                    session,
                    sprintf("%s_weight", method_id),
                    value = weights[[method_id]] * 100
                )
            }
        })

        # Observe each method's enable button and synchronise the slider state.
        lapply(methods, function(method) {
            observeEvent(input[[method$id]], {
                shinyjs::toggleState(
                    session$ns(sprintf("%s_weight", method$id))
                )
            }, ignoreInit = TRUE)
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
