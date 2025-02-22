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
        dplyr::select(-c("Unique reference number", "Abstract")) |>
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
          extensions = "Buttons",
          filter = list(
            position = "top",
            clear = FALSE  # disable 'clear' buttons
          ),
          options = list(
            dom = "Bftipr",
            search = list(regex = TRUE),
            order = list(list(1, "desc")),
            columnDefs = list(
              list(visible = FALSE, targets = "URL") # hide but show in doanload
            ),
            buttons = list(
              list(
                extend = "csv",
                text = "Download CSV",
                title = "renal-evidence-map_data"
              )
            )
          )
        )

    }, server = FALSE)  # to allow download of whole table

  })
}

