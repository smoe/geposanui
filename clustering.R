library(data.table)
library(progress)
library(rlog)

#' Perform a cluster analysis.
#'
#' This function will cluster the data using `hclust` and `cutree` (with the
#' specified height). Every cluster with at least two members qualifies for
#' further analysis. Clusters are then ranked based on their size in relation
#' to the total number of values. The return value is a final score between
#' zero and one. Lower ranking clusters contribute less to this score.
clusteriness <- function(data, height = 1000000) {
    # Cluster the data and compute the cluster sizes.

    tree <- hclust(dist(data))
    clusters <- cutree(tree, h = height)
    cluster_sizes <- sort(tabulate(clusters), decreasing = TRUE)

    # Compute the "cluteriness" score.

    score <- 0.0
    n <- length(data)

    for (i in seq_along(cluster_sizes)) {
        cluster_size <- cluster_sizes[i]

        if (cluster_size >= 2) {
            cluster_score <- cluster_size / n
            score <- score + cluster_score / i
        }
    }

    score
}

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

    log_info(sprintf(
        "Clustering %i genes from %i species",
        gene_count,
        length(species_ids)
    ))

    progress <- progress_bar$new(
        total = gene_count,
        format = "Clustering genes [:bar] :percent (ETA :eta)"
    )

    for (i in 1:gene_count) {
        progress$tick()

        gene_id <- gene_ids[i]

        data <- distances[
            species %chin% species_ids & gene == gene_id,
            .(species, distance)
        ]

        if (data[, .N] < 12) {
            next
        }

        score <- clusteriness(data[, distance])

        results[
            gene == gene_id,
            clusteriness := score
        ]
    }

    results
}