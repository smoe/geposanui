library(data.table)
library(DT)
library(shiny)

source("input.R")
source("process.R")
source("scatter_plot.R")
source("util.R")

data <- run_cached("input", load_input, "input")
results <- run_cached("results", process_input, data)

server <- function(input, output) {
    filtered <- results[cluster_length >= 10]
    merged <- merge.data.table(filtered, data$genes, by.x = "gene", by.y = "id")
    setorder(merged, -cluster_length)

    output$genes <- renderDT({
        datatable(
            merged[, .(.I, name, chromosome, cluster_length, cluster_mean)],
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
        gene_ids <- merged[input$genes_rows_selected, gene]
        scatter_plot(gene_ids, data)
    })
}