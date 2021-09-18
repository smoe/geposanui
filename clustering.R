library(data.table)
library(rlog)

#' Process genes clustering their distance to telomeres.
#'
#' The return value will be a data.table with the following columns:
#'
#'  - `gene` Gene ID of the processed gene.
#'  - `cluster_length` Length of the largest cluster.
#'  - `cluster_mean` Mean value of the largest cluster.
#'  - `cluster_species` List of species contributing to the largest cluster.
#'
#' @param distances Gene distance data to use.
#' @param species_ids IDs of species to include in the analysis.
#' @param gene_ids Genes to include in the computation.
process_clustering <- function(distances, species_ids, gene_ids) {
    results <- data.table(gene = gene_ids)
    gene_count <- length(gene_ids)

    for (i in 1:gene_count) {
        gene_id <- gene_ids[i]

        log_info(sprintf(
            "[%3i%%] Processing gene \"%s\"",
            round(i / gene_count * 100),
            gene_id
        ))

        data <- distances[
            species %chin% species_ids & gene == gene_id,
            .(species, distance)
        ]

        if (data[, .N] < 12) {
            next
        }

        clusters <- hclust(dist(data[, distance]))
        clusters_cut <- cutree(clusters, h = 1000000)

        # Find the largest cluster
        cluster_indices <- unique(clusters_cut)
        cluster_index <- cluster_indices[
            which.max(tabulate(match(clusters_cut, cluster_indices)))
        ]

        cluster <- data[which(clusters_cut == cluster_index)]

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