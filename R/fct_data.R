get_pinned_data <- function(pin_name) {

  board <- pins::board_connect()
  pin_exists <- pins::pin_exists(board, pin_name)

  if (!pin_exists) stop(glue::glue("The pin {pin_name} could not be found"))
  if (pin_exists) pins::pin_read(board, pin_name)

}

get_evidence_data <- function(pinned_data) {

  studies_raw <- pinned_data[["Datasheet"]]

  studies_raw |>
    dplyr::slice(-1) |>  # ignore first row, which is a second row of headers
    dplyr::select(
      # Metadata
      "Unique reference number",
      "Authors",
      "Publication year",
      "Title",
      "Journal",
      "Abstract",
      "Link" = "DOI",
      # Categories
      "Type of evidence",
      "High level outcomes",
      "Focus of the paper",
      # Additional variables
      "Outcomes",
      "Study design",
      "Setting" = `...20`,  # second-level header under 'Population'
      "Population"
    ) |>
    dplyr::mutate(
      `Focus (simplified)` = stringr::str_remove_all(
        `Focus of the paper`,
        "\\s*\\(.*?\\)"  # remove leading spaces, parens, anything inside
      ),
      .after = "Focus of the paper"
    ) |>
    dplyr::mutate(
      `High level outcomes` = dplyr::if_else(
        is.na(`High level outcomes`),
        "Uncategorised",
        `High level outcomes`
      ),
      `Type of evidence` = dplyr::if_else(
        is.na(`Type of evidence`),
        "Uncategorised",
        `Type of evidence`
      ),
      Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
    )

}

get_intro <- function(pinned_data) {
  about_raw <- pinned_data[["About this map"]]
  about_raw[[5, 1]]
}

get_taxonomy_tables <- function(pinned_data) {

  about_raw <- pinned_data[["About this map"]]

  taxonomy_list <- list(
    `Theme categories` = about_raw[8:14, 1:2],
    `Focus categories` = about_raw[16:29, 1:3],
    `Setting` = about_raw[31:40, 1:2],
    `Evidence type` = about_raw[42:43, 1:2],
    `Clinical conditions` = about_raw[45:47, 1:2],
    `High-level outcome preamble` = about_raw[[49, 1]],
    `High-level outcome categories` = about_raw[50:66, 1:2]
  )

  taxonomy_list[names(taxonomy_list) != "High-level outcome preamble"] <-
    taxonomy_list[names(taxonomy_list) != "High-level outcome preamble"] |>
    purrr::map(\(x) dplyr::rename(x, "Category" = 1, "Description" = 2))

  taxonomy_list[["Focus categories"]] <- taxonomy_list[["Focus categories"]] |>
    dplyr::rename("Subcategory" = 2, "Description" = 3)

  taxonomy_list

}
