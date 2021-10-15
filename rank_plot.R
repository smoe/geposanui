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
#' @param cutoff Cut-off score.
rank_plot <- function(results, reference_gene_ids, cutoff) {
    first_not_included_rank <- results[score < cutoff, min(rank)]
    last_rank <- results[, .N]

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
    )  |> layout(
        xaxis = list(title = "Ranks"),
        yaxis = list(title = "Score")
    )

    if (first_not_included_rank <= last_rank) {
        plot <- plot |> layout(
            shapes = list(
                type = "rect",
                fillcolor = "black",
                opacity = 0.1,
                x0 = first_not_included_rank,
                x1 = last_rank,
                y0 = 0.0,
                y1 = 1.0
            )
        )
    }

    plot
}