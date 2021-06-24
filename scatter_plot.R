library(data.table)
library(ggplot2)

#' Draw a scatter plot containing gene positions.
scatter_plot <- function(gene_ids, data) {
    species <- data$species
    setorder(species, median_distance)

    distances <- data$distances[geneid %in% gene_ids]

    plot <- ggplot() +
        scale_x_continuous(
            name = "Species",
            breaks = seq_len(nrow(species)),
            labels = species$label
        ) +
        scale_y_continuous(name = "Distance to telomeres [Mbp]") +
        geom_line(
            species,
            mapping = aes(
                x = as.numeric(rownames(species)),
                y = median_distance / 1000000
            )
        )

    colors <- rainbow(length(gene_ids))

    for (i in seq_len(length(gene_ids))) {
        gene_id <- gene_ids[i]

        gene_distances <- data.table(
            index = as.numeric(rownames(species)),
            distance = unlist(distances[geneid == gene_id, -1])
        )

        plot <- plot +
            geom_point(
                gene_distances,
                mapping = aes(
                    x = index,
                    y = distance / 1000000,
                ),
                color = colors[i]
            )
    }

    plot
    
    # plot(
    #     species[, median_distance] / 1000000,
    #     type = "l",
    #     xaxt = "n",
    #     xlab = "",
    #     ylab = "Distance to telomere (Mbp)",
    #     bty = "n"
    # )

    # axis(
    #     1,
    #     at = seq_len(nrow(species)),
    #     tick = FALSE,
    #     labels = FALSE
    # )

    # mtext(
    #     data$species[, label],
    #     side = 1,
    #     at = seq_len(nrow(species)),
    #     las = 2,
    #     # col = axis.labels.col,
    #     adj = 1
    # )
}