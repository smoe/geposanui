library(data.table)
library(DT)
library(shiny)

source("init.R")
source("scatter_plot.R")

server <- function(input, output) {
    #' This reactive expression applies all user defined filters as well as the
    #' desired ranking weights to the results.
    results <- reactive({
        # Select the species preset.

        results <- if (input$species == "all") {
            results_all
        } else {
            results_replicative
        }

        # Compute scoring factors and the weighted score.

        results[, score := input$clusteriness / 100 * clusteriness +
            input$correlation / 100 * r_mean]

        # Order the results based on their score. The resulting index will be
        # used as the "rank".

        setorder(results, -score, na.last = TRUE)
    })

    output$genes <- renderDT({
        datatable(
            results()[, .(.I, name, clusteriness, r_mean)],
            rownames = FALSE,
            colnames = c(
                "Rank",
                "Gene",
                "Clusteriness",
                "Correlation"
            ),
            style = "bootstrap"
        )
    })

    output$synposis <- renderText({
        results <- results()

        sprintf(
            "Found %i candidates including %i/%i verified and %i/%i suggested \
            TPE-OLD genes.",
            results[, .N],
            results[verified == TRUE, .N],
            genes[verified == TRUE, .N],
            results[suggested == TRUE, .N],
            genes[suggested == TRUE, .N]
        )
    })

    output$scatter <- renderPlot({
        results <- results()

        gene_ids <- results[input$genes_rows_selected, gene]
        genes <- genes[id %chin% gene_ids]

        species <- if (input$species == "all") {
            species
        } else {
            species[replicative == TRUE]
        }

        scatter_plot(results, species, genes, distances)
    })
}