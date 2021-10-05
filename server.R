library(data.table)
library(DT)
library(shiny)

source("init.R")
source("scatter_plot.R")

#' Java script function to replace gene IDs with Ensembl gene links.
js_link <- JS("function(row, data) {
    let id = data[1];
    var name = data[2];
    if (!name) name = 'Unknown';
    let url = `https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=${id}`;
    $('td:eq(1)', row).html(`<a href=\"${url}\" target=\"_blank\">${name}</a>`);
}")

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

        clusteriness_weight <- input$clusteriness / 100
        correlation_weight <- input$correlation / 100
        neural_weight <- input$neural / 100
        total_weight <- clusteriness_weight + correlation_weight + neural_weight
        clusteriness_factor <- clusteriness_weight / total_weight
        correlation_factor <- correlation_weight / total_weight
        neural_factor <- neural_weight / total_weight

        results[, score := clusteriness_factor * clusteriness +
            correlation_factor * r_mean + neural_factor * neural]

        # Apply the cut-off score.

        results <- results[score >= input$cutoff / 100]

        # Order the results based on their score. The resulting index will be
        # used as the "rank".

        setorder(results, -score, na.last = TRUE)
    })

    output$genes <- renderDT({
        dt <- datatable(
            results()[, .(
                .I,
                gene,
                name,
                clusteriness,
                r_mean,
                neural,
                score
            )],
            rownames = FALSE,
            colnames = c(
                "",
                "Gene",
                "",
                "Clusters",
                "Correlation",
                "Neural",
                "Score"
            ),
            style = "bootstrap",
            options = list(
                rowCallback = js_link,
                columnDefs = list(list(visible = FALSE, targets = 2))
            )
        )

        formatPercentage(
            dt,
            c("clusteriness", "r_mean", "neural", "score"),
            digits = 1
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