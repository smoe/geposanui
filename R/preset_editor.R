# Create a preset editor.
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
        ),
        tabsetPanel(
            id = NS(id, "apply_panel"),
            type = "hidden",
            tabPanelBody(value = "hide"),
            tabPanelBody(
                value = "show",
                actionButton(
                    NS(id, "apply_button"),
                    "Perform analysis",
                    class = "btn-primary",
                    style = "margin-top: 16px; margin-bottom: 16px"
                )
            )
        )
    )
}

# Create a server for the preset editor.
#
# @param id ID for namespacing the inputs and outputs.
#
# @return A reactive containing the preset.
preset_editor_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        species_choices <- species$id
        names(species_choices) <- species$name

        updateSelectizeInput(
            session,
            "custom_species",
            choices = species_choices,
            server = TRUE
        )

        current_preset <- reactiveVal(geposan::preset(
            genes[suggested | verified == TRUE, id],
            methods = methods,
            species_ids = species$id,
            gene_ids = genes$id
        ))

        new_preset <- reactive({
            species_ids <- if (input$species == "replicative") {
                species[replicative == TRUE, id]
            } else if (input$species == "all") {
                species$id
            } else {
                input$custom_species
            }

            reference_gene_ids <- if (input$reference_genes == "tpeold") {
                genes[verified | suggested == TRUE, id]
            } else if (input$reference_genes == "verified") {
                genes[verified == TRUE, id]
            } else {
                inputs <- strsplit(input$custom_reference_genes, "\\s+")[[1]]
                if (input$identifier_type == "hgnc") {
                    geposan::genes[name %chin% inputs, id]
                } else {
                    geposan::genes[id %chin% inputs, id]
                }
            }

            geposan::preset(
                reference_gene_ids,
                methods = methods,
                species_ids = species_ids,
                gene_ids = genes$id
            )
        })

        observeEvent(
            { # nolint
                current_preset()
                new_preset()
            },
            { # nolint
                if (rlang::hash(new_preset()) !=
                    rlang::hash(current_preset())) {
                    updateTabsetPanel(
                        session,
                        "apply_panel",
                        selected = "show"
                    )
                } else {
                    updateTabsetPanel(
                        session,
                        "apply_panel",
                        selected = "hide"
                    )
                }
            }
        )

        observeEvent(input$apply_button, {
            current_preset(new_preset())
        })

        current_preset
    })
}
