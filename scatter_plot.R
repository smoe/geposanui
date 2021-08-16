library(data.table)
library(ggplot2)

#' Draw a scatter plot containing gene positions.
scatter_plot <- function(gene_ids, data) {
    plot <- ggplot() +
        scale_x_discrete(
            name = "Species",
            breaks = data$species$id,
            labels = data$species$label
        ) +
        scale_y_continuous(name = "Distance to telomeres [Mbp]")

    colors <- rainbow(length(gene_ids))

    for (i in seq_len(length(gene_ids))) {
        gene_id <- gene_ids[i]

        plot <- plot +
            geom_point(
                data$distances[gene == gene_id],
                mapping = aes(
                    x = species,
                    y = distance / 1000000,
                ),
                color = colors[i],
                size = 4
            )
    }

    plot
}