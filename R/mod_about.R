#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_about_ui <- function(id) {

  ns <- shiny::NS(id)

  card_intro <- bslib::card(
    id = "card_intro",
    full_screen = TRUE,
    bslib::card_header("Introduction", class = "bg-light"),
    tags$img(
      src = "www/tsu_logo_black_screen_transparent.png",
      style = "height:91px;width:108px"
    ),
    shiny::textOutput(ns("intro"))
  )

  card_how_to <- bslib::card(
    id = "card_how_to",
    full_screen = TRUE,
    bslib::card_header("How to use", class = "bg-light"),
    md_file_to_html("app", "text", "about-how-to.md")
  )

  card_meta <- bslib::card(
    id = "card_meta",
    full_screen = TRUE,
    bslib::card_header("Meta information", class = "bg-light"),
    md_file_to_html("app", "text", "about-meta.md")
  )

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/2,
      card_intro,
      bslib::layout_column_wrap(
        width = 1,
        heights_equal = "row",
        card_how_to,
        card_meta
      )
    )
  )

}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id, pinned_data) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$intro <- shiny::renderText({
      get_intro(pinned_data)
    })

  })
}

