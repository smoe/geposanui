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
        name = "Clustering",
        description = "Clustering of genes"
    ),
    list(
        id = "correlation",
        name = "Correlation",
        description = "Correlation with known genes"
    ),
    list(
        id = "proximity",
        name = "Proximity",
        description = "Proximity to telomeres"
    ),
    list(
        id = "neural",
        name = "Neural",
        description = "Assessment by neural network"
    )
)

# Gene IDs of known or suggested TPE-OLD genes.
genes_tpe_old <- genes[suggested | verified == TRUE, id]

# Species IDs for replicatively aging species.
species_replicative <- species[replicative == TRUE, id]

# Preset for [geposan] including all species and TPE-OLD genes for reference.
preset_all_species <- geposan::preset(
    methods = c("clusteriness", "correlation", "proximity", "neural"),
    species = species$id,
    genes = genes$id,
    reference_genes = genes_tpe_old
)

# Preset for [geposan] including only replicatively aging species as well as
# TPE-OLD genes for reference.
preset_replicative_species <- geposan::preset(
    methods = c("clusteriness", "correlation", "proximity", "neural"),
    species = species_replicative,
    genes = genes$id,
    reference_genes = genes_tpe_old
)
