library(data.table)
library(rlog)

#' Process genes screening for a likely TPE-OLD.
#'
#' The return value will be a table containing genes and data to take in
#' account when regarding them as TPE-OLD candidates.
#'
#' @param input Data from [`load_input()`].
#' @param species_ids IDs of species to include in the analysis.
process_input <- function(input, species_ids) {
    results <- data.table(gene = input$genes$id)

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

        # Find the largest cluster
        cluster_indices <- unique(clusters_cut)
        cluster_index <- cluster_indices[
            which.max(tabulate(match(clusters_cut, cluster_indices)))
        ]

        cluster <- distances[which(clusters_cut == cluster_index)]

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