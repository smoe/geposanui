# Species IDs of known replicatively aging species.
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

# Species from [geposan] and their aging status.
species <- geposan::species[, .(
    id,
    name,
    replicative = id %chin% species_ids_replicative
)]

# Gene names of genes for verified TPE-OLD genes.
genes_verified_tpe_old <- c(
    "C1S",
    "DSP",
    "ISG15",
    "SORBS2",
    "TERT"
)

# Gene names of genes with a suggested TPE-OLD.
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

# Genes from [geposan] and their TPE-OLD status.
genes <- geposan::genes[, .(
    id,
    name,
    chromosome,
    suggested = name %chin% genes_suggested_tpe_old,
    verified = name %chin% genes_verified_tpe_old
)]

# All available methods from [geposan] and additional information on them.
methods <- list(
    list(
        id = "clusteriness",
        name = "Clustering (distances)",
        description = "Clustering of genes (distances)"
    ),
    list(
        id = "clusteriness_positions",
        name = "Clustering (positions)",
        description = "Clustering of genes (positions)"
    ),
    list(
        id = "correlation",
        name = "Correlation (distances)",
        description = "Correlation with known genes (distances)"
    ),
    list(
        id = "correlation_positions",
        name = "Correlation (positions)",
        description = "Correlation with known genes (positions)"
    ),
    list(
        id = "neural",
        name = "Neural",
        description = "Assessment by neural network"
    ),
    list(
        id = "proximity",
        name = "Proximity",
        description = "Proximity to telomeres"
    )
)

# IDs of methods for geposan.
method_ids <- sapply(methods, function(method) method$id)

# Names of methods for geposan.
method_names <- sapply(methods, function(method) method$name)
