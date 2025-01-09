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
      bslib::accordion(
        id = "accordion_taxonomy",
        open = FALSE,
        bslib::accordion_panel(
          title = "Theme categories",
          DT::DTOutput(ns("taxonomy_theme"))
        ),
        bslib::accordion_panel(
          title = "Topic categories",
          DT::DTOutput(ns("taxonomy_topic"))
        ),
        bslib::accordion_panel(
          title = "Setting",
          DT::DTOutput(ns("taxonomy_setting"))
        ),
        bslib::accordion_panel(
          title = "Evidence type",
          DT::DTOutput(ns("taxonomy_evidence_type"))
        ),
        bslib::accordion_panel(
          title = "Clinical conditions",
          DT::DTOutput(ns("taxonomy_clinical"))
        ),
        bslib::accordion_panel(
          title = "High-level outcome categories",
          shiny::textOutput(ns("taxonomy_high_level_preamble")),
          DT::DTOutput(ns("taxonomy_high_level_outcome"))
        )
      )
    )
  )

}

#' taxonomy Server Functions
#'
#' @noRd
mod_taxonomy_server <- function(id, taxonomy) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$taxonomy_theme <- DT::renderDT({
      taxonomy[["Theme categories"]] |> tabulate_taxonomy_table()
    })

    output$taxonomy_topic <-DT::renderDT({
      taxonomy[["Topic categories"]] |>
        tidyr::replace_na(list("Category" = "")) |>
        tabulate_taxonomy_table()
    })

    output$taxonomy_setting <- DT::renderDT({
      taxonomy[["Setting"]] |>
        tidyr::replace_na(list("Description" = "–")) |>
        tabulate_taxonomy_table()
    })

    output$taxonomy_evidence_type <- DT::renderDT({
      taxonomy[["Evidence type"]] |> tabulate_taxonomy_table()
    })

    output$taxonomy_clinical <- DT::renderDT({
      taxonomy[["Clinical conditions"]] |> tabulate_taxonomy_table()
    })

    output$taxonomy_high_level_preamble <- shiny::renderText({
      taxonomy[["High-level outcome preamble"]]
    })

    output$taxonomy_high_level_outcome <- DT::renderDT({
      taxonomy[["High-level outcome categories"]] |> tabulate_taxonomy_table()
    })

  })
}

