#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  pinned_data <- get_pinned_data("matt.dray/renal_evidence_map_data")
  # about_raw_data <- pinned_data[["About this map"]]  # TODO: temporarily disabled
  evidence_data <- get_evidence_data(pinned_data)

  mod_summary_table_server("summary_table", evidence_data)
  mod_search_server("search", evidence_data)
  # mod_taxonomy_server("taxonomy", about_raw_data)  # TODO: temporarily disabled

}
