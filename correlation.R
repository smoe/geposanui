library(data.table)
library(progress)
library(rlog)

#' Compute the mean correlation coefficient comparing gene distances with a set
#' of reference genes.
#'
#' The result will be a data.table with the following columns:
#'
#'  - `gene` Gene ID of the processed gene.
#'  - `r_mean` Mean correlation coefficient.
#'
#' @param distances Distance data to use.
#' @param species_ids Species, whose data should be included.
#' @param gene_ids Genes to process.
#' @param reference_gene_ids Genes to compare to.
process_correlation <- function(distances, species_ids, gene_ids,
                                reference_gene_ids) {
    results <- data.table(gene = gene_ids)
    gene_count <- length(gene_ids)
    reference_count <- length(reference_gene_ids)

    log_info(sprintf(
        "Correlating %i genes from %i species with %i reference genes",
        gene_count,
        length(species_ids),
        reference_count
    ))

    progress <- progress_bar$new(
        total = gene_count,
        format = "Correlating genes [:bar] :percent (ETA :eta)"
    )

    # Prefilter distances by species.
    distances <- distances[species %chin% species_ids]

    for (i in 1:gene_count) {
        progress$tick()

        gene_id <- gene_ids[i]
        gene_distances <- distances[gene == gene_id]

        if (nrow(gene_distances) < 10) {
            next
        }

        #' Buffer for the sum of correlation coefficients.
        r_sum <- 0

        # Correlate with all reference genes but not with the gene itself.
        for (reference_gene_id in
            reference_gene_ids[reference_gene_ids != gene_id]) {
            data <- merge(
                gene_distances,
                distances[gene == reference_gene_id],
                by = "species"
            )

            # Order data by the reference gene's distance to get a monotonic
            # relation.
            setorder(data, distance.y)

            r_sum <- r_sum + abs(cor(data[, distance.x], data[, distance.y]))
        }

        results[gene == gene_id, r_mean := r_sum / reference_count]
    }

    results
}