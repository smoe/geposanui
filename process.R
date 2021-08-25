library(data.table)
library(rlog)

#' Process genes screening for a likely TPE-OLD.
#'
#' The return value will be a table containing genes and data to take in
#' account when regarding them as TPE-OLD candidates.
#'
#' @param input Data from [`load_input()`].
process_input <- function(input) {
    results <- data.table(gene = input$genes$id)

    # Exclude species with naturally or artificially short chromosomes as well
    # as non-replicatively aging species.
    species_ids <- input$species[
        median_distance >= 7500000 & group == "replicative",
        id
    ]

    gene_ids <- input$genes[, id]
    gene_count <- length(gene_ids)

    for (i in seq_along(gene_ids)) {
        gene_id <- gene_ids[i]
        log_info(sprintf("Processing gene %i/%i (%i)", i, gene_count, gene_id))

        distances <- input$distances[
            species %chin% species_ids & gene == gene_id,
            .(species, distance)
        ]

        if (distances[, .N] < 12) {
            next
        }

        clusters <- hclust(dist(distances[, distance]))
        clusters_cut <- cutree(clusters, h = 1000000)
        cluster <- distances[which(clusters_cut == 1)]

        results[
            gene == gene_id,
            `:=`(
                cluster_length = cluster[, .N],
                cluster_mean = mean(cluster[, distance]),
                cluster_species = list(cluster[, species])
            )
        ]
    }

    results
}