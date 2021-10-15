library(data.table)

#' Score the mean distance of genes to the telomeres across species.
#'
#' A score will be given to each gene such that 0.0 corresponds to the maximal
#' mean distance across all genes and 1.0 corresponds to a distance of 0.
#'
#' The result will be a data.table with the following columns:
#'
#'  - `gene` Gene ID of the processed gene.
#'  - `score` Score for the proximity.
#'
#' @param distances Distance data to use.
#' @param species_ids Species, whose data should be included.
#' @param gene_ids Genes to process.
process_proximity <- function(distances, species_ids, gene_ids, ...) {
    species_count <- length(species_ids)

    # Prefilter distances by species.
    distances <- distances[species %chin% species_ids]

    # Compute the score as described above.

    distances <- distances[, .(mean_distance = mean(distance)), by = "gene"]
    max_distance <- distances[, max(mean_distance)]
    distances[, score := 1 - mean_distance / max_distance]

    distances[, .(gene, score)]
}
