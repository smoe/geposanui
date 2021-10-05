library(data.table)
library(neuralnet)

#' Find genes by training a neural network on reference position data.
#'
#' The result will be a data.table with the following columns:
#'
#'  - `gene` Gene ID of the processed gene.
#'  - `neural` Output score given by the neural network.
#'
#' @param distances Distance data to use.
#' @param species_ids Species, whose data should be included.
#' @param gene_ids Genes to process. This should include the reference genes.
#' @param reference_gene_ids Genes to compare to.
#' @param seed A seed to get reproducible results.
process_neural <- function(distances, species_ids, gene_ids,
                           reference_gene_ids, seed = 726839) {
    set.seed(seed)
    gene_count <- length(gene_ids)

    # Prefilter distances by species.
    distances <- distances[species %chin% species_ids]

    #' Input data for the network. This contains the gene ID as an identifier
    #' as well as the per-species gene distances as input variables.
    data <- data.table(gene = gene_ids)

    #' Buffer to keep track of species included in the computation. Species
    #' from `species_ids` may be excluded if they don't have enough data.
    species_ids_included <- NULL

    for (species_id in species_ids) {
        # Make a column specific to this species.

        species_distances <- distances[species == species_id, .(gene, distance)]
        setnames(species_distances, "distance", species_id)

        # Only include species with at least 25% known values.

        species_distances <- na.omit(species_distances)

        if (nrow(species_distances) >= 0.25 * gene_count) {
            species_ids_included <- append(species_ids_included, species_id)
            data <- merge(data, species_distances, all = TRUE)
        }
    }

    # Replace missing data with mean values. The neural network can't handle
    # NAs in a meaningful way. Choosing extreme values here would result in
    # heavily biased results. Therefore, the mean value is chosen as a
    # compromise. However, this will of course lessen the significance of the
    # results.
    for (species_id in species_ids_included) {
        mean_value <- data[, mean(get(species_id), na.rm = TRUE)]

        data[
            is.na(get(species_id)),
            eval(quote(species_id)) := round(mean_value)
        ]
    }

    # Extract the reference genes.

    reference_data <- data[gene %chin% reference_gene_ids]
    reference_data[, neural := 1.0]

    # Take out random samples from the remaining genes. This is another
    # compromise with a negative impact on significance. Because there is no
    # information on genes with are explicitely *not* TPE-OLD genes, we have to
    # assume that a random sample of genes has a low probability of including
    # TPE-OLD genes.

    without_reference_data <- data[!gene %chin% reference_gene_ids]

    reference_samples <- without_reference_data[
        sample(
            nrow(without_reference_data),
            nrow(reference_data)
        )
    ]

    reference_samples[, neural := 0.0]

    # Merge training data. The training data includes all reference genes as
    # well as an equal number of random sample genes.
    training_data <- rbindlist(list(reference_data, reference_samples))

    # Construct and train the neural network.

    nn_formula <- as.formula(sprintf(
        "neural~%s",
        paste(species_ids_included, collapse = "+")
    ))

    layer1 <- length(species_ids_included) * 0.66
    layer2 <- layer1 * 0.66
    layer3 <- layer2 * 0.66

    nn <- neuralnet(
        nn_formula,
        training_data,
        hidden = c(layer1, layer2, layer3),
        linear.output = FALSE
    )

    # Return the resulting scores given by applying the neural network.

    data[, neural := compute(nn, data)$net.result]
    data[, .(gene, neural)]
}