# Construct UI for the filter editor.
filters_ui <- function(id) {
    verticalLayout(
        h3("Filter criteria"),
        uiOutput(NS(id, "n_species_slider")),
        checkboxInput(
            NS(id, "filter_score"),
            span(
                "Cut-off score",
                style = "font-weight: bold"
            ),
            value = TRUE
        ),
        sliderInput(
            NS(id, "cut_off_score"),
            label = NULL,
            post = "%",
            min = 0,
            max = 100,
            step = 1,
            value = 50
        ),
        checkboxInput(
            NS(id, "filter_rank"),
            span(
                "Maximum number of genes",
                style = "font-weight: bold"
            ),
            value = FALSE
        ),
        sliderInput(
            NS(id, "max_rank"),
            label = NULL,
            min = 0,
            max = 5000,
            step = 50,
            value = 2000
        )
    )
}

# Construct server for the filter editor.
#
# @param preset A reactive containing the preset to apply.
#
# @return A reactive containing the filter values `n_species`, `cut_off_score`
#   and `max_rank` as well as the name of the filter to apply (`filter`).
filters_server <- function(id, preset) {
    moduleServer(id, function(input, output, session) {
        output$n_species_slider <- renderUI({
            sliderInput(
                session$ns("n_species"),
                "Required number of species per gene",
                min = 0,
                max = length(preset()$species_ids),
                step = 1,
                value = 10
            )
        })

        filter <- reactiveVal("score")

        observeEvent(input$filter_score, {
            if (input$filter_score) {
                updateCheckboxInput(session, "filter_rank", value = FALSE)
                shinyjs::enable("cut_off_score")
                shinyjs::disable("max_rank")
                filter("score")
            }
        })

        observeEvent(input$filter_rank, {
            if (input$filter_rank) {
                updateCheckboxInput(session, "filter_score", value = FALSE)
                shinyjs::enable("max_rank")
                shinyjs::disable("cut_off_score")
                filter("rank")
            }
        })

        reactive({
            list(
                n_species = input$n_species,
                filter = filter(),
                cut_off_score = input$cut_off_score,
                max_rank = input$max_rank
            )
        })
    })
}
