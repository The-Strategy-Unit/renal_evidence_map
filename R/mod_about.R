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
    shiny::div(
      style = "display:flex;",  # images side-by-side
      tags$img(
        src = "www/nhp_logo.png",
        style = "height:91px;width:151px;padding-right:10px;"
      ),
      tags$img(
        src = "www/tsu_logo_black_screen_transparent.png",
        style = "height:91px;width:108px"
      )
    ),
    shiny::uiOutput(ns("intro"))
  )

  card_how_to <- bslib::card(
    id = "card_how_to",
    full_screen = TRUE,
    bslib::card_header("How to use", class = "bg-light"),
    md_file_to_html("app", "text", "about-how-to.md")
  )

  card_methods <- bslib::card(
    id = "card_methods",
    full_screen = TRUE,
    bslib::card_header("Methodology", class = "bg-light"),
    shiny::tags$p(
      "You can",
      shiny::tags$a(
        "download a PDF document",
        href = "www/search-protocol.pdf"
      ),
      "that details the search protocol and strategy for the gathered evidence."
    )
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
        card_methods,
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

    output$intro <- shiny::renderUI({
      htmltools::HTML(get_intro(pinned_data))
    })

  })
}

