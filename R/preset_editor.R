#' Create the UI for a preset editor.
#'
#' @param id ID for namespacing.
#' @return The UI elements.
#'
#' @noRd
preset_editor_ui <- function(id) {
    verticalLayout(
        h3("Inputs"),
        selectInput(
            NS(id, "species"),
            "Species to include",
            choices = list(
                "All species" = "all",
                "Known replicatively aging species" = "replicative",
                "Customize" = "custom"
            )
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", NS(id, "species")),
            selectizeInput(
                inputId = NS(id, "custom_species"),
                label = "Select input species",
                choices = NULL,
                multiple = TRUE
            ),
        ),
        selectInput(
            NS(id, "reference_genes"),
            "Reference genes",
            choices = list(
                "Verified or suggested TPE-OLD genes" = "tpeold",
                "Only verified TPE-OLD genes" = "verified",
                "Customize" = "custom"
            )
        ),
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'custom'",
                NS(id, "reference_genes")
            ),
            selectInput(
                NS(id, "identifier_type"),
                "Gene identifiers",
                choices = list(
                    "HGNC symbols" = "hgnc",
                    "Ensembl gene IDs" = "ensembl"
                )
            ),
            textAreaInput(
                inputId = NS(id, "custom_reference_genes"),
                label = "Enter reference genes",
                height = "250px"
            )
        )
    )
}

#' Application logic for the preset editor.
#'
#' @param id ID for namespacing the inputs and outputs.
#' @return A reactive containing the preset or `NULL`, if the input data doesn't
#'   result in a valid one.
#'
#' @noRd
preset_editor_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        species_choices <- geposan::species$id
        names(species_choices) <- geposan::species$name

        updateSelectizeInput(
            session,
            "custom_species",
            choices = species_choices,
            server = TRUE
        )

        reactive({
            reference_gene_ids <- if (input$reference_genes == "tpeold") {
                genes[verified | suggested == TRUE, id]
            } else if (input$reference_genes == "verified") {
                genes[verified == TRUE, id]
            } else {
                inputs <- strsplit(input$custom_reference_genes, "\\s+")[[1]]

                gene_ids <- if (input$identifier_type == "hgnc") {
                    geposan::genes[name %chin% inputs, id]
                } else {
                    geposan::genes[id %chin% inputs, id]
                }
            }

            species_ids <- if (input$species == "replicative") {
                species_ids_replicative
            } else if (input$species == "all") {
                geposan::species$id
            } else {
                input$custom_species
            }

            tryCatch(
                geposan::preset(
                    reference_gene_ids,
                    species_ids = species_ids
                ),
                error = function(err) NULL
            )
        })
    })
}
