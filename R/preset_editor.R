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
            selectizeInput(
                inputId = NS(id, "custom_reference_genes"),
                label = "Select reference genes",
                choices = NULL,
                multiple = TRUE
            )
        ),
        selectInput(
            NS(id, "optimization_target"),
            "Optimization target",
            choices = list(
                "Mean rank of reference genes" = "mean",
                "Median rank of reference genes" = "median",
                "First rank of reference genes" = "min",
                "Last rank of reference genes" = "max"
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

        known_genes <- genes[name != ""]
        gene_choices <- known_genes$id
        names(gene_choices) <- known_genes$name

        updateSelectizeInput(
            session,
            "custom_species",
            choices = species_choices,
            server = TRUE
        )

        updateSelectizeInput(
            session,
            "custom_reference_genes",
            choices = gene_choices,
            server = TRUE
        )

        current_preset <- reactiveVal(geposan::preset(
            methods = method_ids,
            species_ids = species$id,
            gene_ids = genes$id,
            reference_gene_ids = genes[suggested | verified == TRUE, id],
            optimization_target = "mean"
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
                input$custom_reference_genes
            }

            geposan::preset(
                methods = method_ids,
                species_ids = species_ids,
                gene_ids = genes$id,
                reference_gene_ids = reference_gene_ids,
                optimization_target = input$optimization_target
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
