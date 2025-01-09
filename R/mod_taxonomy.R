#' taxonomy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_taxonomy_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      id = "card_taxonomy",
      htmltools::h2("Theme categories"),
      shiny::tableOutput(ns("taxonomy_theme")),
      htmltools::h2("Topic categories"),
      shiny::tableOutput(ns("taxonomy_topic")),
      htmltools::h2("Setting"),
      shiny::tableOutput(ns("taxonomy_setting")),
      htmltools::h2("Evidence type"),
      shiny::tableOutput(ns("taxonomy_evidence_type")),
      htmltools::h2("Clinical conditions"),
      shiny::tableOutput(ns("taxonomy_clinical")),
      htmltools::h2("High-level outcome categories"),
      shiny::textOutput(ns("taxonomy_high_level_preamble")),
      shiny::tableOutput(ns("taxonomy_high_level_outcome"))
    )
  )

}

#' taxonomy Server Functions
#'
#' @noRd
mod_taxonomy_server <- function(id, taxonomy) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$taxonomy_theme <- shiny::renderTable({
      taxonomy[["Theme categories"]]
    })

    output$taxonomy_topic <- shiny::renderTable({
      taxonomy[["Topic categories"]] |>
        tidyr::replace_na(list("Category" = ""))
    })

    output$taxonomy_setting <- shiny::renderTable({
      taxonomy[["Setting"]]
    })

    output$taxonomy_evidence_type <- shiny::renderTable({
      taxonomy[["Evidence type"]]
    })

    output$taxonomy_clinical <- shiny::renderTable({
      taxonomy[["Clinical conditions"]]
    })

    output$taxonomy_high_level_preamble <- shiny::renderText({
      taxonomy[["High-level outcome preamble"]]
    })

    output$taxonomy_high_level_outcome <- shiny::renderTable({
      taxonomy[["High-level outcome categories"]]
    })

  })
}

