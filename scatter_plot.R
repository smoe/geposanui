library(data.table)
library(ggplot2)

#' Draw a scatter plot containing gene positions.
#'
#' @param results Results from [`process_input()`].
#' @param species Species to be displayed.
#' @param genes Genes to be displayed.
#' @param distances Distance data to display.
scatter_plot <- function(results, species, genes, distances) {
    if (nrow(genes) < 1) {
        return(ggplot())
    }

    species_ids <- species[, id]

    data <- merge(
        genes[, .(id, name)],
        distances[species %in% species_ids],
        by.x = "id", by.y = "gene"
    )

    for (gene_id in genes[, id]) {
        cluster_species <- unlist(results[gene == gene_id, cluster_species])
        data[id == gene_id, in_cluster := species %in% cluster_species]
    }

    ggplot(data) +
        scale_x_discrete(
            name = "Species",
            breaks = species$id,
            labels = species$name
        ) +
        scale_y_continuous(
            name = "Distance to telomeres [Mbp]",
            limits = function(x) {
                if (x[2] < 15) {
                    c(0, 15)
                } else {
                    x
                }
            }
        ) +
        scale_color_discrete(name = "Gene") +
        scale_shape_discrete(
            name = "Part of cluster",
            breaks = c(TRUE, FALSE),
            labels = c("Yes", "No")
        ) +
        geom_point(
            mapping = aes(
                x = species,
                y = distance / 1000000,
                color = name,
                shape = in_cluster
            ),
            size = 5
        ) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}