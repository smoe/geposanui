# Create a comparison editor.
comparison_editor_ui <- function(id) {
    known_genes <- genes[name != ""]
    gene_choices <- known_genes$id
    names(gene_choices) <- known_genes$name

    verticalLayout(
        h3("Comparison"),
        selectInput(
            NS(id, "comparison_genes"),
            "Comparison genes",
            choices = list(
                "None" = "none",
                "Random genes" = "random",
                "Verified or suggested TPE-OLD genes" = "tpeold",
                "Only verified TPE-OLD genes" = "verified",
                "Only suggested TPE-OLD genes" = "suggested",
                "Customize" = "custom"
            )
        ),
        tabsetPanel(
            id = NS(id, "custom_comparison_genes_panel"),
            type = "hidden",
            tabPanelBody(value = "hide"),
            tabPanelBody(
                value = "show",
                shinyWidgets::pickerInput(
                    inputId = NS(id, "custom_comparison_genes"),
                    choices = gene_choices,
                    options = list(
                        "actions-box" = TRUE,
                        "live-search" = TRUE
                    ),
                    multiple = TRUE
                )
            )
        )
    )
}

# Create a server for the comparison editor.
#
# @param id ID for namespacing the inputs and outputs.
# @param preset A reactive containing the current preset.
#
# @return A reactive containing the comparison gene IDs.
comparison_editor_server <- function(id, preset) {
    moduleServer(id, function(input, output, session) {
        observeEvent(input$comparison_genes, {
            if (input$comparison_genes == "custom") {
                updateTabsetPanel(
                    session,
                    "custom_comparison_genes_panel",
                    selected = "show"
                )
            } else {
                updateTabsetPanel(
                    session,
                    "custom_comparison_genes_panel",
                    selected = "hide"
                )
            }
        })

        reactive({
            if (input$comparison_genes == "none") {
                NULL
            } else if (input$comparison_genes == "random") {
                preset <- preset()
                gene_pool <- preset$gene_ids
                reference_gene_ids <- preset$reference_gene_ids
                gene_pool <- gene_pool[!gene_pool %chin% reference_gene_ids]
                gene_pool[sample(length(gene_pool), length(reference_gene_ids))]
            } else if (input$comparison_genes == "tpeold") {
                genes[verified | suggested == TRUE, id]
            } else if (input$comparison_genes == "verified") {
                genes[verified == TRUE, id]
            } else if (input$comparison_genes == "suggested") {
                genes[suggested == TRUE, id]
            } else {
                input$custom_comparison_genes
            }
        })
    })
}
