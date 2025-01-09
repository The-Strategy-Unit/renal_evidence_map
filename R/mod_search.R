#' search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_search_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      id = "card_search",
      full_screen = TRUE,
      DT::DTOutput(ns("search_table"))
    )
  )

}

#' search Server Functions
#'
#' @noRd
mod_search_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$search_table <- DT::renderDT({

      dat_prepared <- dat |>
        dplyr::rename(  # because the waffle errored unless dat names were changed
          "Topic" = "topic",
          "Type of evidence" = "type_of_evidence",
          "High level outcomes" = "high_level_outcomes"
        ) |>
        dplyr::select(-"Abstract") |>
        dplyr::mutate(
          dplyr::across(
            c(
              "Publication year",
              "Journal",
              "Type of evidence",
              "High level outcomes",
              "Topic (high level)",
              "Topic",
              "Setting",
            ),
            factor  # allows discrete dropdown in datatable
          )
        )

      dat_prepared |>
        DT::datatable(
          style = "default",
          class = "stripe",
          rownames = FALSE,
          selection = "none",
          escape = FALSE,
          filter = list(position = "top"),
          extensions = "Buttons",
          options = list(search = list(regex = TRUE))
        )

    })

  })
}

