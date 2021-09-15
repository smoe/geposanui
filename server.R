library(data.table)
library(DT)
library(shiny)

source("input.R")
source("process.R")
source("scatter_plot.R")
source("util.R")

# Load input data

species <- run_cached("species", retrieve_species)
genes <- run_cached("genes", retrieve_genes)

distances <- run_cached(
    "distances",
    retrieve_distances,
    species[, id],
    genes[, id]
)

#' Results computed for all species.
results_all <- run_cached(
    "results_all",
    process_input,
    distances,
    species[, id],
    genes[, id]
)

#' Results computed for known replicatively aging species.
results_replicative <- run_cached(
    "results_replicative",
    process_input,
    distances,
    species[replicative == TRUE, id],
    genes[, id]
)

# Add gene information to results for display.

results_all <- merge(
    results_all,
    genes,
    by.x = "gene",
    by.y = "id"
)

results_replicative <- merge(
    results_replicative,
    genes,
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