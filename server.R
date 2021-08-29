library(data.table)
library(DT)
library(shiny)

source("input.R")
source("process.R")
source("scatter_plot.R")
source("util.R")

# Initialize global static data

inputs <- run_cached("input", load_input, "input")

#' All species excluding species with naturally or artificially short
#' chromosomes.
species_qualified <- inputs$species[median_distance >= 7500000]

#' All known replicatively aging species with long enough chromosomes.
species_replicative <- species_qualified[group == "replicative"]

#' Results computed from [`species_qualified`].
results_all <- run_cached(
    "results_all",
    process_input,
    inputs,
    species_qualified[, id]
)

#' Results computed from [`species_replicative`].
results_replicative <- run_cached(
    "results_replicative",
    process_input,
    inputs,
    species_replicative[, id]
)

# Add gene information to results for display.

results_all <- merge(
    results_all,
    inputs$genes,
    by.x = "gene",
    by.y = "id"
)

results_replicative <- merge(
    results_replicative,
    inputs$genes,
    by.x = "gene",
    by.y = "id"
)

# Order results by cluster length descendingly.
# TODO: Once other methods have been added, this has to be dynamic.
setorder(results_all, -cluster_length)
setorder(results_replicative, -cluster_length)

server <- function(input, output) {
    #' This expression applies all user defined filters to the available
    #' results.
    results <- reactive({
        results <- if (input$species == "all") {
            results_all
        } else {
            results_replicative
        }

        results[
            cluster_length >= input$length &
                cluster_mean >= input$range[1] * 1000000 &
                cluster_mean <= input$range[2] * 1000000
        ]
    })

    output$genes <- renderDT({
        datatable(
            results()[, .(.I, name, chromosome, cluster_length, cluster_mean)],
            rownames = FALSE,
            colnames = c(
                "Rank",
                "Gene",
                "Chromosome",
                "Cluster length",
                "Cluster mean"
            ),
            style = "bootstrap"
        )
    })

    output$scatter <- renderPlot({
        results <- results()

        gene_ids <- results[input$genes_rows_selected, gene]

        species <- if (input$species == "all") {
            species_qualified
        } else {
            species_replicative
        }

        scatter_plot(gene_ids, inputs, results, species)
    })
}