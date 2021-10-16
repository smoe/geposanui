library(biomaRt)
library(data.table)
library(progress)
library(rlog)
library(stringr)

#' Species IDs of known replicatively aging species.
species_ids_replicative <- c(
    "bihybrid",
    "btaurus",
    "bthybrid",
    "cfamiliaris",
    "chircus",
    "cjacchus",
    "clfamiliaris",
    "csabaeus",
    "ecaballus",
    "fcatus",
    "ggorilla",
    "hsapiens",
    "lafricana",
    "mfascicularis",
    "mmulatta",
    "mmurinus",
    "mnemestrina",
    "nleucogenys",
    "oaries",
    "pabelii",
    "panubis",
    "ppaniscus",
    "ptroglodytes",
    "sscrofa",
    "tgelada"
)

#' Gene names of genes for verified TPE-OLD genes.
genes_verified_tpe_old <- c(
    "C1S",
    "DSP",
    "ISG15",
    "SORBS2",
    "TERT"
)

#' Gene names of genes with a suggested TPE-OLD.
genes_suggested_tpe_old <- c(
    "AKAP3",
    "ANO2",
    "CCND2",
    "CD163L1",
    "CD9",
    "FOXM1",
    "GALNT8",
    "NDUFA9",
    "TEAD4",
    "TIGAR",
    "TSPAN9"
)

#' Shared accessor for the Ensembl API.
ensembl <- NULL

#' Get the ensembl accessor and initialize it if necessary.
get_ensembl <- function() {
    if (is.null(ensembl)) {
        ensembl <<- useEnsembl("ensembl", version = 104)
    }

    ensembl
}

#' Get all chromosome names for a Ensembl dataset.
#'
#' Valid chromosome names include decimal numbers as well as 'X' and 'Y'.
get_chromosome_names <- function(dataset) {
    chromosome_names <- listFilterOptions(dataset, "chromosome_name")
    chromosome_names[str_which(chromosome_names, "^[0-9]+|[XY]$")]
}

#' Retrieve information on species.
#'
#' The result will be a `data.table` with the following columns:
#'
#'  - `id` Species ID as presented by Ensembl.
#'  - `name` Human readable species name.
#'  - `replicative` Whether the species is likely to be aging replicatively.
retrieve_species <- function() {
    # Ensembl datasets correspond to distinct species.
    ensembl_datasets <- data.table(listDatasets(get_ensembl()))

    # Filter out species ID and name from the result.
    species <- ensembl_datasets[, .(
        id = str_match(dataset, "(.*)_gene_ensembl")[, 2],
        name = str_match(description, "(.*) genes \\(.*\\)")[, 2]
    )]

    species[, replicative := id %chin% species_ids_replicative]
}

#' Retrieve information on human genes.
#'
#' This will only include genes on assembled chromosomes. Chromosomes are
#' filtered based on their name being either a decimal number, 'X' or 'Y'.
#'
#' The result will be a `data.table` with the following columns:
#'
#'  - `id` Ensembl gene ID.
#'  - `ǹame` HGNC name of the gene.
#'  - `chromosome` Human chromosome on which the gene is located.
retrieve_genes <- function() {
    dataset <- useDataset("hsapiens_gene_ensembl", mart = get_ensembl())

    genes <- data.table(getBM(
        attributes = c("ensembl_gene_id", "hgnc_symbol", "chromosome_name"),
        filters = "chromosome_name",
        values = get_chromosome_names(dataset),
        mart = useDataset("hsapiens_gene_ensembl", mart = get_ensembl())
    ))

    genes[, .(
        id = ensembl_gene_id,
        name = hgnc_symbol,
        chromosome = chromosome_name,
        verified = hgnc_symbol %chin% genes_verified_tpe_old,
        suggested = hgnc_symbol %chin% genes_suggested_tpe_old
    )]
}

#' Retrieve gene distance data.
#'
#' The data will include all available values for the given species and genes
#' that are located on assembled chromosomes.
#'
#' The result will be a `data.table` with the following columns:
#'
#'  - `species` Species ID.
#'  - `gene` Ensembl gene ID.
#'  - `distance` Distance to nearest telomere in base pairs.
retrieve_distances <- function(species_ids, gene_ids) {
    ensembl <- get_ensembl()

    # Exclude the human from the species, in case it is present there.
    species_ids <- species_ids[species_ids != "hsapiens"]

    species_count <- length(species_ids)
    gene_count <- length(gene_ids)

    log_info(sprintf(
        "Retrieving distance data for %i genes from %i species",
        gene_count,
        species_count
    ))

    progress <- progress_bar$new(
        total = gene_count,
        format = "Retrieving distance data [:bar] :percent (ETA :eta)"
    )

    # Special case the human species and retrieve all available distance
    # information.

    dataset <- useDataset("hsapiens_gene_ensembl", mart = ensembl)

    human_distances <- data.table(getBM(
        attributes = c(
            "ensembl_gene_id",
            "chromosome_name",
            "start_position",
            "end_position"
        ),
        filters = "chromosome_name",
        values = get_chromosome_names(dataset),
        mart = dataset
    ))

    # Compute the nearest distance to telomeres.

    human_distances[,
        chromosome_length := max(end_position),
        by = chromosome_name
    ]

    distances <- human_distances[, .(
        species = "hsapiens",
        gene = ensembl_gene_id,
        distance = pmin(
            start_position,
            chromosome_length - end_position
        )
    )]

    for (i in 1:species_count) {
        species_id <- species_ids[i]

        progress$tick()

        dataset <- useDataset(
            sprintf("%s_gene_ensembl", species_id),
            mart = ensembl
        )

        # Besides the attributes that are always present, we need to check for
        # human orthologs. Some species don't have that information and will be
        # skipped.
        if (!"hsapiens_homolog_ensembl_gene" %chin%
            listAttributes(dataset, what = "name")) {
            next
        }

        chromosome_names <- get_chromosome_names(dataset)

        # Skip the species, if there are no assembled chromosomes.
        if (length(chromosome_names) <= 0) {
            next
        }

        # Retrieve information on all genes of the current species, that have
        # human orthologs. This is called "homolog" in the Ensembl schema.
        ensembl_distances <- data.table(getBM(
            attributes = c(
                "hsapiens_homolog_ensembl_gene",
                "chromosome_name",
                "start_position",
                "end_position"
            ),
            filters = c("with_hsapiens_homolog", "chromosome_name"),
            values = list(TRUE, chromosome_names),
            mart = dataset
        ))

        # Precompute the genes' distance to the nearest telomere.

        ensembl_distances[,
            chromosome_length := max(end_position),
            by = chromosome_name
        ]

        species_distances <- ensembl_distances[, .(
            species = species_id,
            gene = hsapiens_homolog_ensembl_gene,
            distance = pmin(
                start_position,
                chromosome_length - end_position
            )
        )]

        distances <- rbindlist(list(distances, species_distances))
    }

    # Arbitrarily exclude duplicated genes.
    # TODO: Consider a refined approach or work out how to include all
    # duplicates.
    unique(distances, by = c("species", "gene"))
}