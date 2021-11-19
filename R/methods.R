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
        actionButton(
            NS(id, "reset_button"),
            "Reset weights",
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
        # Observe each method's enable button and synchronise the slider state.
        lapply(methods, function(method) {
            observeEvent(input[[method$id]],
                { # nolint
                    shinyjs::toggleState(sprintf("%s_weight", method$id))
                },
                ignoreInit = TRUE
            )
        })

        observeEvent(
            { # nolint
                analysis()
                input$reset_button
            },
            { # nolint
                for (method in methods) {
                    updateCheckboxInput(
                        session,
                        method$id,
                        value = TRUE
                    )

                    updateSliderInput(
                        session,
                        sprintf("%s_weight", method$id),
                        value = analysis()$weights[[method$id]]
                    )
                }
            },
            ignoreNULL = FALSE
        )

        reactive({
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
