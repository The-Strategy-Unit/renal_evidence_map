#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Data ----

  pinned_data <- get_pinned_data("matt.dray/renal_evidence_map_data")

  taxonomy <- pinned_data[["About this map"]]

  dat <- get_evidence_data(pinned_data) |>
    dplyr::rename(  # the waffle errors without this
      "type_of_evidence" = "Type of evidence",
      "high_level_outcomes" = "High level outcomes",
      "topic" = "Topic"
    )

  taxonomy <- get_taxonomy_tables(pinned_data)

  # Modules ----

  mod_about_server("mod_about", pinned_data)
  mod_evidence_map_server("mod_evidence_map", dat)
  mod_search_server("mod_search", dat)
  mod_taxonomy_server("mod_taxonomy", taxonomy)

}
