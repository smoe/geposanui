library(data.table)
library(DT)
library(shiny)

source("input.R")
source("process.R")
source("scatter_plot.R")
source("util.R")

data <- run_cached("input", load_input, "input")
results <- run_cached("results", process_input, data)
merged <- merge(results, data$genes, by.x = "gene", by.y = "id")
setorder(merged, -cluster_length)

server <- function(input, output) {
    filtered <- reactive({
        merged[
            cluster_length >= input$length &
                cluster_mean >= input$range[1] * 1000000 &
                cluster_mean <= input$range[2] * 1000000
        ]
    })

    output$genes <- renderDT({
        datatable(
            filtered()[, .(.I, name, chromosome, cluster_length, cluster_mean)],
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
        gene_ids <- filtered()[input$genes_rows_selected, gene]
        scatter_plot(gene_ids, data)
    })
}