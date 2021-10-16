#' Find the best weights to rank the data.
#'
#' This function ranks the provided data table based on a weighted score
#' computed from the specified `columns`. It tries to find the optimal weights
#' that result in a ranking, where the mean rank of the given reference genes
#' is as high as possible.
#'
#' @param data Input data including the columns.
#' @param colums Columns containing the separate scores between 0.0 and 1.0.
#' @param reference_gene_ids IDs of the reference genes within the input data.
#'
#' @returns Vector of optimal column weights adding up to 1.0.
optimize_weights <- function(data, columns, reference_gene_ids) {
    #' Compute the mean rank of the reference genes when applying the weights.
    mean_rank <- function(weights) {
        data <- copy(data)
        data[, score := 0.0]

        for (i in seq_along(columns)) {
            column <- columns[i]
            weighted <- weights[i] * data[, ..column]
            data[, score := score + weighted]
        }

        setorder(data, -score)
        data[, rank := .I]

        data[gene %chin% reference_gene_ids, mean(rank)]
    }

    weights <- optim(rep(1.0, length(columns)), mean_rank)$par
    total_weight <- sum(weights)
    weights / total_weight
}