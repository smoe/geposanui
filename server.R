library(data.table)
library(DT)
library(shiny)

source("data.R")
source("scatter_plot.R")
source("util.R")

data <- run_cached("input", load_data, "input")

server <- function(input, output) {
    output$genes <- renderDT({
        datatable(
            data$genes[, c("name", "chromosome")],
            rownames = FALSE,
            style = "bootstrap"
        )
    })

    output$scatter <- renderPlot({
        gene_ids <- data$genes[input$genes_rows_selected, id]
        scatter_plot(gene_ids, data)
    })
}