# Fetch pin (a list where each element is a sheet from the workbook)
get_pinned_data <- function(pin_name) {

  board <- pins::board_connect()
  pin_exists <- pins::pin_exists(board, pin_name)

  if (!pin_exists) stop(glue::glue("The pin {pin_name} could not be found"))
  if (pin_exists) pins::pin_read(board, pin_name)

}

# Extract and wrangle evidence data from pin
get_evidence_data <- function(pinned_data) {

  studies_raw <- pinned_data[["Datasheet"]]

  studies_raw |>
    dplyr::slice(-1) |>  # ignore first row, which is a second row of headers
    dplyr::select(
      # Metadata
      "Unique ref no",
      "Authors",
      "Publication year",
      "Title",
      "Journal",
      "Abstract",
      "Link" = "DOI",
      # Categories
      "evidence_type" = "Type of evidence",  # TODO: why is renaming necessary?
      "Outcomes (high level)",    # TODO: a higher-level grouping of this?
      # Additional variables
      "Outcomes",  # too many levels to be useful
      "Study design",
      "Setting" = `...20`,  # second-level header under 'Population'
      "Population"
    ) |>
    dplyr::mutate(
      `Outcomes (high level)` = dplyr::if_else(  # have updated now in original data
        `Outcomes (high level)` == "Not applcable",
        "Not applicable",
        `Outcomes (high level)`
      ),
      `Outcomes (high level)` = dplyr::if_else(
        is.na(`Outcomes (high level)`),
        "Uncategorised",
        `Outcomes (high level)`
      ),
      `Outcomes (higher level)` = dplyr::case_when(  # TODO: I just made this up
        stringr::str_detect(`Outcomes (high level)`, "^Cardiov") ~ "Cardiovascular",
        stringr::str_detect(`Outcomes (high level)`, "^Cardior") ~ "Cardiorenal",
        stringr::str_detect(`Outcomes (high level)`, "^Env") ~ "Environmental impacts",
        stringr::str_detect(`Outcomes (high level)`, "^Renal o") ~ "Renal outcomes",
        .default = `Outcomes (high level)`
      ),
      evidence_type = dplyr::if_else(
        is.na(evidence_type),
        "Uncategorised",
        evidence_type
      ),
      Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
    )

}
