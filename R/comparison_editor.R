# Create a comparison editor.
comparison_editor_ui <- function(id) {
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
        conditionalPanel(
            condition = sprintf(
                "input['%s'] == 'custom'",
                NS(id, "comparison_genes")
            ),
            selectizeInput(
                inputId = NS(id, "custom_comparison_genes"),
                label = "Select comparison genes",
                choices = NULL,
                multiple = TRUE
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
        known_genes <- genes[name != ""]
        gene_choices <- known_genes$id
        names(gene_choices) <- known_genes$name

        updateSelectizeInput(
            session,
            "custom_comparison_genes",
            choices = gene_choices,
            server = TRUE
        )

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
