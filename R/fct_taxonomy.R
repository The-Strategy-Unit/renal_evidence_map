#' taxonomy 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

extract_taxonomy_table <- function(taxonomy_raw_data, rows_i, cols_i, ...) {
  
  taxonomy_raw_data |> 
    dplyr::slice(rows_i) |> 
    dplyr::select(cols_i) |> 
    janitor::row_to_names(1) |>
    dplyr::rename(...) 
  
}

create_taxonomy_gt <- function(taxonomy_table) {
  
  taxonomy_table |> 
    gt::gt(rowname_col = names(taxonomy_table[1])) |> 
    gt::tab_stubhead(label = names(taxonomy_table[1])) |> 
    gt::tab_options(
      table.font.size = "12px",
      column_labels.font.size = "14px",
      column_labels.font.weight = "bold",
      stub.font.size = "14px"
    )
  
}