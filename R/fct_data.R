get_pinned_data <- function(pin_name) {

  board <- pins::board_connect(server = "connect.strategyunitwm.nhs.uk")
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
      "URL" = "DOI",
      # Categories
      "Type of evidence",
      "High level outcomes",
      "Topic (high level)" = "Focus of the paper",
      # Additional variables
      "Outcomes",
      "Study design",
      "Setting" = `...20`,  # second-level header under 'Population'
      "Population"
    ) |>
    dplyr::mutate(
      `Topic` = stringr::str_remove_all(
        `Topic (high level)`,
        "\\s*\\(.*?\\)"  # remove leading spaces, parens, anything inside
      ),
      .after = `Topic (high level)`
    ) |>
    dplyr::mutate(
      Link = paste0("<a href='", URL, "' target = 'new'>Link</a>"),
      .after = "URL"
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
      )
    )

}

get_intro <- function(pinned_data) {
  about_raw <- pinned_data[["About this map"]]
  about_raw[[5, 1]] |>
    stringr::str_replace_all(r"{\r\n}", "<br>")  # HTML newline
}

get_taxonomy_tables <- function(pinned_data) {

  about_raw <- pinned_data[["About this map"]]

  taxonomy_tables <- list(
    # hard-coded given placement in the input spreadsheet
    "Theme categories"              = about_raw[8:14,  1:2],
    "Topic categories"              = about_raw[16:29, 1:3],
    "Setting"                       = about_raw[31:40, 1:2],
    "Evidence type"                 = about_raw[42:43, 1:2],
    "Clinical conditions"           = about_raw[45:47, 1:2],
    "High-level outcome categories" = about_raw[50:61, 1:2]
  )

  taxonomy_tables <- taxonomy_tables |>
    purrr::map(\(x) dplyr::rename(x, "Category" = 1, "Description" = 2))

  taxonomy_tables[["Topic categories"]] <-
    taxonomy_tables[["Topic categories"]] |>
    dplyr::rename("Subcategory" = 2, "Description" = 3)  # name extra column

  taxonomy_tables <-taxonomy_tables |>
    purrr::map(
      \(x) x |>
        tidyr::replace_na(  # prefer text to NA
          list(
            Category = "",
            Subcategory = "–",
            Description = "–"
          )
        ) |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::any_of(c("Subcategory", "Description")),
            \(x) stringr::str_replace_all(x, r"{\r\n}", "<br>")  # HTML newline
          )
        )
    )

  taxonomy_text <- list(
    "High-level outcome preamble" = about_raw[[49, 1]] |>
      stringr::str_replace_all(r"{\r\n}", "<br>")
  )

  c(taxonomy_tables, taxonomy_text)

}
