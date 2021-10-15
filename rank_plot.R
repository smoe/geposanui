library(data.table)
library(plotly)

#' Draw a plot displaying the rank of reference genes.
#'
#' The input table should contain the following columns:
#'
#'  - `gene` Gene IDs of genes to display.
#'  - `name` Name of genes to display.
#'  - `score` Score of the genes.
#'  - `rank` Rank of the genes based on the score.
#'
#' @param results Results to display.
#' @param reference_gene_ids IDs of reference genes.
rank_plot <- function(results, reference_gene_ids) {
    plot <- plot_ly() |> add_trace(
        data = results,
        x = ~rank,
        y = ~score,
        name = "All genes",
        type = "scatter",
        mode = "line",
        hoverinfo = "skip"
    ) |> add_trace(
        data = results[gene %chin% reference_gene_ids],
        x = ~rank,
        y = ~score,
        color = ~gene,
        name = ~name,
        width = 10,
        type = "bar"
    ) |> layout(
        xaxis = list(title = "Ranks"),
        yaxis = list(title = "Score")
    )
}