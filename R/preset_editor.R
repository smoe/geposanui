# Create a preset editor.
preset_editor_ui <- function(id) {
    species_choices <- species$id
    names(species_choices) <- species$name

    known_genes <- genes[name != ""]
    gene_choices <- known_genes$id
    names(gene_choices) <- known_genes$name

    verticalLayout(
        h3("Preset"),
        selectInput(
            NS(id, "preset"),
            "Default presets",
            choices = list(
                "Replicatively aging species" = "replicative",
                "All species" = "all",
                "Customize" = "custom"
            )
        ),
        tabsetPanel(
            id = NS(id, "customization"),
            type = "hidden",
            tabPanelBody(value = "none"),
            tabPanelBody(
                value = "custom",
                shinyWidgets::pickerInput(
                    inputId = NS(id, "species"),
                    label = "Included species",
                    choices = species_choices,
                    selected = species_replicative,
                    options = list(
                        "actions-box" = TRUE,
                        "live-search" = TRUE
                    ),
                    multiple = TRUE
                ),
                shinyWidgets::pickerInput(
                    inputId = NS(id, "reference_genes"),
                    label = "Reference genes",
                    choices = gene_choices,
                    selected = genes_tpe_old,
                    options = list(
                        "actions-box" = TRUE,
                        "live-search" = TRUE
                    ),
                    multiple = TRUE
                ),
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
        result <- reactiveVal(preset_replicative_species)

        observeEvent(input$preset, {
            panel <- if (input$preset == "replicative") {
                result(preset_replicative_species)
                "none"
            } else if (input$preset == "all") {
                result(preset_all_species)
                "none"
            } else {
                "custom"
            }

            updateTabsetPanel(session, "customization", selected = panel)
        })

        observeEvent(input$apply_button, {
            result(geposan::preset(
                methods = method_ids,
                species_ids = input$species,
                gene_ids = genes$id,
                reference_gene_ids = input$reference_genes
            ))
        })

        result
    })
}
