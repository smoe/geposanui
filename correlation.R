library(data.table)

#' Compute the mean correlation coefficient comparing gene distances with a set
#' of reference genes.
#'
#' The result will be a data.table with the following columns:
#'
#'  - `gene` Gene ID of the processed gene.
#'  - `correlation` Mean correlation coefficient.
#'
#' @param distances Distance data to use.
#' @param species_ids Species, whose data should be included.
#' @param gene_ids Genes to process.
#' @param reference_gene_ids Genes to compare to.
process_correlation <- function(distances, species_ids, gene_ids,
                                reference_gene_ids) {
    results <- data.table(gene = gene_ids)
    reference_count <- length(reference_gene_ids)

    # Prefilter distances by species.
    distances <- distances[species %chin% species_ids]

    # Add an index for quickly accessing data per gene.
    setkey(distances, gene)

    # Prepare the reference genes' data.
    reference_distances <- distances[gene %chin% reference_gene_ids]

    #' Perform the correlation for one gene.
    compute <- function(gene_id) {
        gene_distances <- distances[gene_id]
        gene_species_count <- nrow(gene_distances)

        # Return a score of 0.0 if there is just one or no value at all.
        if (gene_species_count <= 1) {
            return(0.0)
        }

        #' Buffer for the sum of correlation coefficients.
        correlation_sum <- 0

        # Correlate with all reference genes but not with the gene itself.
        for (reference_gene_id in
            reference_gene_ids[reference_gene_ids != gene_id]) {
            data <- merge(
                gene_distances,
                reference_distances[reference_gene_id],
                by = "species"
            )

            # Skip this reference gene, if there are not enough value pairs.
            # This will lessen the final score, because it effectively
            # represents a correlation coefficient of 0.0.
            if (nrow(data) <= 1) {
                next
            }

            # Order data by the reference gene's distance to get a monotonic
            # relation.
            setorder(data, distance.y)

            correlation_sum <- correlation_sum + abs(cor(
                data[, distance.x], data[, distance.y],
                method = "spearman"
            ))
        }

        # Compute the score as the mean correlation coefficient.
        score <- correlation_sum / reference_count
    }

    results[, correlation := compute(gene), by = 1:nrow(results)]
}